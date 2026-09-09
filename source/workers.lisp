(in-package #:sbcl-workers)

(defclass sbcl-worker ()
  ((environment
    :initarg :environment
    :accessor worker--environment
    :type sbcl-worker-environment
    :documentation "The host paths and runtime settings used by the worker.")
   (name
    :initarg :name
    :reader sbcl-worker-name
    :type string
    :documentation "The stable name used to route requests.")
   (image-identifier
    :initarg :image-identifier
    :reader sbcl-worker-used-image-identifier
    :accessor worker--image-identifier
    :type string
    :documentation "The pristine or saved image used at process start.")
   (core-pathname
    :initarg :core-pathname
    :initform nil
    :reader sbcl-worker-core-pathname
    :accessor worker--core-pathname
    :type (or null pathname)
    :documentation "The compatible saved core, or NIL for a pristine worker.")
   (process
    :initform nil
    :accessor worker--process
    :type t
    :documentation "The active UIOP process info, or NIL.")
   (input
    :initform nil
    :accessor worker--input
    :type (or null stream)
    :documentation "The worker's request stream.")
   (output
    :initform nil
    :accessor worker--output
    :type (or null stream)
    :documentation "The worker's response stream.")
   (next-request-id
    :initform 1
    :accessor worker--next-request-id
    :type integer
    :documentation "The next protocol request identifier.")
   (lock
    :initform (make-recursive-lock "SBCL worker")
    :reader worker--lock
    :documentation "The recursive lock serializing protocol and lifecycle changes."))
  (:documentation "One named persistent, heap-isolated SBCL process."))

(defun sbcl-worker-name-p (value)
  "Return true when VALUE is safe as one named worker."
  (and (worker--non-empty-string-p value)
       (<= (length value) 80)
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_")))
              value)
       t))

(defun worker--validate-name (name)
  "Return valid worker NAME or signal a worker condition."
  (unless (sbcl-worker-name-p name)
    (worker--signal-error
     (format nil
             "Invalid SBCL worker name ~S. Use 1 to 80 letters, digits, hyphens, or underscores."
             name)
     :operation :workers
     :stage :name))
  name)

(defun worker--create-record
    (environment &key name image-identifier core-pathname)
  "Create a stopped worker record with an already resolved CORE-PATHNAME."
  (make-instance 'sbcl-worker
                 :environment environment
                 :name name
                 :image-identifier image-identifier
                 :core-pathname core-pathname))

(defun sbcl-worker-create
    (environment
     &key
       (name "default")
       (image-identifier +pristine-sbcl-worker-image-identifier+))
  "Create a stopped named worker based on IMAGE-IDENTIFIER."
  (worker--validate-name name)
  (let ((image
          (unless (string= image-identifier
                           +pristine-sbcl-worker-image-identifier+)
            (sbcl-worker-image-load environment image-identifier))))
    (when (and image (not (sbcl-worker-image-compatible-p image)))
      (worker-image--signal-error
       (format nil "SBCL worker image ~A is incompatible with this runtime."
               image-identifier)
       :operation :start
       :pathname (sbcl-worker-image-manifest-pathname image)
       :stage :compatibility))
    (worker--create-record
     environment
     :name name
     :image-identifier image-identifier
     :core-pathname (and image (sbcl-worker-image-core-pathname image)))))

(defun sbcl-worker-running-p (worker)
  "Return true when WORKER has a live subprocess."
  (let ((process (worker--process worker)))
    (and process (uiop:process-alive-p process) t)))

(defun worker--command (worker)
  "Return the argv list that boots WORKER from pristine or a saved core."
  (if (sbcl-worker-core-pathname worker)
      (list (worker-environment--sbcl-command (worker--environment worker))
            "--noinform"
            "--core"
            (namestring (sbcl-worker-core-pathname worker))
            "--end-runtime-options")
      (copy-list
       (worker-environment--pristine-command (worker--environment worker)))))

(defun worker--handshake-p (worker form)
  "Return true when FORM is the exact expected handshake for WORKER."
  (let* ((environment (worker--environment worker))
         (properties (and (listp form) (rest form))))
    (and properties
         (eq (first form) (worker-environment--protocol-tag environment))
         (= (or (getf properties :version) 0)
            (worker-environment--protocol-version environment))
         (string= (or (getf properties :image) "")
                  (sbcl-worker-used-image-identifier worker))
         t)))

(defun sbcl-worker-start (worker)
  "Start WORKER when necessary and verify its protocol handshake."
  (unless (sbcl-worker-running-p worker)
    (let ((environment (worker--environment worker)))
      (handler-case
          (let ((process
                  (uiop:launch-program
                   (worker--command worker)
                   :directory
                   (sbcl-worker-environment-working-directory environment)
                   :input :stream
                   :output :stream
                   :error-output *error-output*
                   :wait nil)))
            (setf (worker--process worker) process
                  (worker--input worker) (uiop:process-info-input process)
                  (worker--output worker) (uiop:process-info-output process))
            (loop for line = (read-line (worker--output worker) nil nil)
                  do (unless line
                       (worker--signal-error
                        "The SBCL worker exited before its handshake."
                        :operation :start
                        :stage :handshake))
                     (let* ((*read-eval* nil)
                            (form (handler-case
                                      (read-from-string line)
                                    (error ()
                                      nil))))
                       (when (and (listp form)
                                  (eq (first form)
                                      (worker-environment--protocol-tag
                                       environment)))
                         (unless (worker--handshake-p worker form)
                           (worker--signal-error
                            "The SBCL worker reported the wrong protocol or image identity."
                            :operation :start
                            :stage :handshake))
                         (return)))))
        (sbcl-worker-error (condition)
          (sbcl-worker-stop worker)
          (error condition))
        (error (condition)
          (sbcl-worker-stop worker)
          (worker--signal-error
           (format nil "Could not start the SBCL worker: ~A" condition)
           :operation :start
           :stage :launch
           :cause condition)))))
  worker)

(defun worker--cleanup-process (process input output)
  "Terminate detached PROCESS and close its protocol streams."
  (when process
    (when (uiop:process-alive-p process)
      (ignore-errors (uiop:terminate-process process :urgent t)))
    (ignore-errors (uiop:wait-process process)))
  (dolist (stream (list input output))
    (when (and stream (open-stream-p stream))
      (ignore-errors (close stream))))
  nil)

(defun worker--detach-process (worker)
  "Detach WORKER's process state and return its process and protocol streams."
  (let ((process (worker--process worker))
        (input (worker--input worker))
        (output (worker--output worker)))
    (setf (worker--process worker) nil
          (worker--input worker) nil
          (worker--output worker) nil
          (worker--next-request-id worker) 1)
    (values process input output)))

(defun sbcl-worker-stop (worker)
  "Terminate WORKER and discard all process streams and heap state."
  (multiple-value-bind (process input output)
      (with-recursive-lock-held ((worker--lock worker))
        (worker--detach-process worker))
    (worker--cleanup-process process input output))
  nil)

(defun sbcl-worker-cancel-request (worker)
  "Detach WORKER immediately and clean its interrupted process asynchronously.

This operation is safe from a condition handler running inside
SBCL-WORKER-REQUEST. The next request starts a fresh process without waiting for
old process reaping or stream cleanup."
  (multiple-value-bind (process input output)
      (with-recursive-lock-held ((worker--lock worker))
        (worker--detach-process worker))
    (when (or process input output)
      (make-thread
       (lambda ()
         (worker--cleanup-process process input output))
       :name "SBCL worker cancelled-request cleanup")))
  nil)

(defun sbcl-worker-reset (worker)
  "Discard WORKER's process and restart it from the same base image."
  (sbcl-worker-stop worker)
  (sbcl-worker-start worker))

(defun worker--working-directory-form (directory)
  "Return a readable worker form that changes to DIRECTORY."
  (format nil
          "(progn (uiop:chdir ~S) ~
                  (setf *default-pathname-defaults* (uiop:getcwd)) ~
                  (namestring (uiop:getcwd)))"
          (namestring directory)))

(defun sbcl-worker-change-working-directory (worker environment)
  "Move WORKER to ENVIRONMENT's directory without discarding its heap."
  (when (sbcl-worker-running-p worker)
    (let* ((directory
             (sbcl-worker-environment-working-directory environment))
           (response
             (sbcl-worker-request
              worker
              :eval
              (list :form (worker--working-directory-form directory))))
           (properties (rest response)))
      (unless (eq (getf properties :status) :ok)
        (worker--signal-error
         (format nil "SBCL worker ~A could not change to ~A: ~A"
                 (sbcl-worker-name worker)
                 directory
                 (or (getf properties :message) "unknown worker failure"))
         :operation :change-working-directory
         :pathname directory))))
  (setf (worker--environment worker) environment)
  worker)


;;;; -- Named Worker Pools --

(defclass sbcl-worker-pool ()
  ((environment
    :initarg :environment
    :accessor sbcl-worker-pool-environment
    :type sbcl-worker-environment
    :documentation "The paths and settings shared by managed workers.")
   (workers
    :initform (make-hash-table :test #'equal)
    :reader worker-pool--workers
    :type hash-table
    :documentation "Named workers mapped to their process managers.")
   (lock
    :initform (make-lock "SBCL worker pool")
    :reader worker-pool--lock
    :documentation "The lock serializing creation, reset, and removal."))
  (:documentation "A manager for independent named persistent SBCL workers."))

(defun sbcl-worker-pool-create (environment)
  "Create an empty named worker pool for ENVIRONMENT."
  (make-instance 'sbcl-worker-pool :environment environment))

(defun sbcl-worker-pool-start (pool name image-identifier)
  "Start or return NAME, enforcing IMAGE-IDENTIFIER when supplied."
  (worker--validate-name name)
  (with-lock-held ((worker-pool--lock pool))
    (let ((existing (gethash name (worker-pool--workers pool))))
      (when existing
        (unless (or (null image-identifier)
                    (string= image-identifier
                             (sbcl-worker-used-image-identifier existing)))
          (worker--signal-error
           (format nil
                   "SBCL worker ~A already uses image ~A; reset it explicitly to switch to ~A."
                   name
                   (sbcl-worker-used-image-identifier existing)
                   image-identifier)
           :operation :start))
        (return-from sbcl-worker-pool-start (sbcl-worker-start existing)))
      (let* ((image-identifier
               (or image-identifier
                   +pristine-sbcl-worker-image-identifier+))
             (worker
               (sbcl-worker-create
                (sbcl-worker-pool-environment pool)
                :name name
                :image-identifier image-identifier)))
        (setf (gethash name (worker-pool--workers pool)) worker)
        (handler-case
            (sbcl-worker-start worker)
          (error (condition)
            (remhash name (worker-pool--workers pool))
            (sbcl-worker-stop worker)
            (error condition)))))))

(defun sbcl-worker-pool-worker (pool name)
  "Return NAME, starting it from pristine SBCL when absent."
  (sbcl-worker-pool-start pool name nil))

(defun sbcl-worker-pool-stop (pool name &key (if-missing :error))
  "Stop and forget named worker NAME."
  (worker--validate-name name)
  (with-lock-held ((worker-pool--lock pool))
    (let ((worker (gethash name (worker-pool--workers pool))))
      (cond
        (worker
         (sbcl-worker-stop worker)
         (remhash name (worker-pool--workers pool)))
        ((eq if-missing :error)
         (worker--signal-error
          (format nil "No SBCL worker named ~A exists." name)
          :operation :stop)))))
  nil)

(defun sbcl-worker-pool-reset (pool name image-identifier)
  "Replace named worker NAME with a fresh process from IMAGE-IDENTIFIER."
  (sbcl-worker-pool-stop pool name :if-missing :ignore)
  (sbcl-worker-pool-start pool name image-identifier))

(defun sbcl-worker-pool-stop-all (pool)
  "Stop and forget every worker managed by POOL."
  (with-lock-held ((worker-pool--lock pool))
    (maphash (lambda (name worker)
               (declare (ignore name))
               (sbcl-worker-stop worker))
             (worker-pool--workers pool))
    (clrhash (worker-pool--workers pool)))
  nil)

(defun sbcl-worker-pool-change-working-directory (pool environment)
  "Move every worker in POOL to ENVIRONMENT with all-or-rollback semantics."
  (with-lock-held ((worker-pool--lock pool))
    (let ((previous (sbcl-worker-pool-environment pool))
          (changed nil))
      (handler-case
          (progn
            (maphash
             (lambda (name worker)
               (declare (ignore name))
               (sbcl-worker-change-working-directory worker environment)
               (push worker changed))
             (worker-pool--workers pool))
            (setf (sbcl-worker-pool-environment pool) environment)
            pool)
        (error (condition)
          (let ((rollback-failures nil))
            (dolist (worker changed)
              (handler-case
                  (sbcl-worker-change-working-directory worker previous)
                (error (rollback-condition)
                  (sbcl-worker-stop worker)
                  (setf (worker--environment worker) previous)
                  (push (cons worker rollback-condition) rollback-failures))))
            (when rollback-failures
              (worker--signal-error
               (format nil
                       "Changing SBCL worker directories failed (~A), and rollback stopped: ~{~A~^; ~}."
                       condition
                       (loop for (worker . rollback-condition)
                               in (nreverse rollback-failures)
                             collect
                             (format nil "~A: ~A"
                                     (sbcl-worker-name worker)
                                     rollback-condition)))
               :operation :change-working-directory))
            (error condition)))))))

(defun sbcl-worker-pool-render (pool)
  "Return a concise list of named workers, process states, and images."
  (let ((workers nil))
    (with-lock-held ((worker-pool--lock pool))
      (maphash (lambda (name worker)
                 (declare (ignore name))
                 (push worker workers))
               (worker-pool--workers pool)))
    (if workers
        (with-output-to-string (stream)
          (dolist (worker (sort workers #'string< :key #'sbcl-worker-name))
            (format stream "~A  ~A  image ~A~%"
                    (sbcl-worker-name worker)
                    (if (sbcl-worker-running-p worker) "running" "stopped")
                    (sbcl-worker-used-image-identifier worker))))
        "No named SBCL workers exist.")))


;;;; -- Portable Requests and Image Snapshots --

(defun sbcl-worker-request (worker operation arguments)
  "Send OPERATION and portable ARGUMENTS to WORKER and return its response."
  (with-lock-held ((worker--lock worker))
    (sbcl-worker-start worker)
    (let* ((request-id (worker--next-request-id worker))
           (request (list :request
                          :id request-id
                          :operation operation
                          :arguments arguments)))
      (incf (worker--next-request-id worker))
      (handler-case
          (progn
            (let ((*print-readably* t)
                  (*print-circle* t))
              (prin1 request (worker--input worker))
              (terpri (worker--input worker))
              (finish-output (worker--input worker)))
            (let ((*read-eval* nil)
                  (response (read (worker--output worker) t nil)))
              (unless (and (listp response)
                           (eq (first response) :response)
                           (= (or (getf (rest response) :id) -1) request-id))
                (worker--signal-error
                 "The SBCL worker returned an invalid response."
                 :operation operation
                 :stage :response))
              response))
        (sbcl-worker-error (condition)
          (sbcl-worker-stop worker)
          (error condition))
        (error (condition)
          (sbcl-worker-stop worker)
          (worker--signal-error
           (format nil "The SBCL worker protocol failed: ~A" condition)
           :operation operation
           :stage :protocol
           :cause condition))))))

(defun worker--probe-core (environment identifier core-pathname)
  "Boot unpublished CORE-PATHNAME and verify its protocol and identity."
  (let ((probe
          (worker--create-record
           environment
           :name "image-probe"
           :image-identifier identifier
           :core-pathname core-pathname)))
    (unwind-protect
         (let ((response
                 (sbcl-worker-request probe :eval '(:form "(+ 20 22)"))))
           (unless (and (eq (getf (rest response) :status) :ok)
                        (equal (getf (rest response) :values) '("42")))
             (worker-image--signal-error
              "The unpublished SBCL worker image failed its protocol probe."
              :operation :save-image
              :pathname core-pathname
              :stage :probe)))
      (sbcl-worker-stop probe)))
  nil)

(defun sbcl-worker-save-image
    (environment worker &key identifier note)
  "Save WORKER as immutable IDENTIFIER with durable NOTE and return its image."
  (sbcl-worker-image-validate-identifier identifier)
  (unless (worker-image--valid-note-p note)
    (worker-image--signal-error
     "A saved SBCL worker image needs a non-empty note of at most 4000 characters."
     :operation :save-image
     :stage :manifest))
  (let* ((directory (worker-image--directory environment identifier))
         (staging
           (sbcl-worker-image-staging-directory environment identifier))
         (core (merge-pathnames "worker.core" staging)))
    (when (probe-file directory)
      (worker-image--signal-error
       (format nil "SBCL worker image ~A already exists." identifier)
       :operation :save-image
       :pathname directory
       :stage :publish))
    (ensure-directories-exist core)
    (unwind-protect
         (let ((response
                 (sbcl-worker-request
                  worker
                  :save-image
                  (list :pathname (namestring core)
                        :identifier identifier))))
           (unless (eq (getf (rest response) :status) :ok)
             (worker-image--signal-error
              (format nil "The worker could not save image ~A: ~A"
                      identifier
                      (or (getf (rest response) :message)
                          "unknown worker failure"))
              :operation :save-image
              :pathname core
              :stage :save))
           (let ((restarting-p (getf (rest response) :restarting-p)))
             (when restarting-p
               (worker--await-save-exit worker core))
             (worker--probe-core environment identifier core)
             (let ((image
                     (worker-image--publish-saved-core
                      environment
                      :identifier identifier
                      :parent-identifier (sbcl-worker-used-image-identifier worker)
                      :note note
                      :staging-directory staging)))
               (when restarting-p
                 ;; The worker exited to save its heap; it continues from
                 ;; the image it just became, so its name stays live.
                 (worker--adopt-image worker image)
                 (sbcl-worker-start worker))
               image)))
      (when (uiop:directory-exists-p staging)
        (uiop:delete-directory-tree staging
                                    :validate t
                                    :if-does-not-exist :ignore)))))

(defun worker--await-save-exit (worker core)
  "Wait for WORKER, which saves itself and exits, to leave CORE behind.

Hosts without fork save an image by exiting the worker process after its
response. The exit is the completion signal, so this reaps the process and
requires the unpublished core to exist."
  (let ((process (worker--process worker)))
    (when process
      (ignore-errors (uiop:wait-process process))))
  (sbcl-worker-stop worker)
  (unless (probe-file core)
    (worker-image--signal-error
     "The SBCL worker exited without saving its image."
     :operation :save-image
     :pathname core
     :stage :save))
  nil)

(defun worker--adopt-image (worker image)
  "Make WORKER restart from published IMAGE, whose heap it just became."
  (setf (worker--image-identifier worker) (sbcl-worker-image-identifier image)
        (worker--core-pathname worker) (sbcl-worker-image-core-pathname image))
  nil)


;;;; -- Fork-Safe Host Checkpoints --

(defun worker--detach-inherited-process (worker)
  "Detach inherited process descriptors without signaling the subprocess."
  (dolist (stream (list (worker--input worker) (worker--output worker)))
    (when (and stream (open-stream-p stream))
      (ignore-errors (close stream))))
  (setf (worker--process worker) nil
        (worker--input worker) nil
        (worker--output worker) nil
        (worker--next-request-id worker) 1)
  nil)

(defun sbcl-worker-manager-detach-inherited-processes (manager)
  "Detach a forked host's inherited worker descriptors without killing workers."
  (typecase manager
    (sbcl-worker
     (worker--detach-inherited-process manager))
    (sbcl-worker-pool
     (with-lock-held ((worker-pool--lock manager))
       (maphash (lambda (name worker)
                  (declare (ignore name))
                  (worker--detach-inherited-process worker))
                (worker-pool--workers manager))))
    (null
     nil)
    (otherwise
     (worker--signal-error
      "The supplied value is not an SBCL worker manager."
      :operation :detach)))
  nil)
