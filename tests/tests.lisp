(in-package #:sbcl-workers/tests)

(defvar *tests-run* 0
  "The number of assertions evaluated by the current test run.")

(defun test-assert (condition description)
  "Record and require CONDITION, reporting DESCRIPTION on failure."
  (incf *tests-run*)
  (unless condition
    (error "Test failed: ~A" description)))

(defun test-root ()
  "Return a fresh temporary directory for one test group."
  (merge-pathnames
   (format nil "sbcl-workers-tests-~36R-~36R/"
           (get-universal-time)
           (random most-positive-fixnum))
   (uiop:temporary-directory)))

(defun test-pristine-command ()
  "Return an argv list that boots this checkout's worker runtime."
  (let* ((asd (asdf:system-source-file :sbcl-workers))
         (root (uiop:pathname-directory-pathname asd)))
    (list "sbcl"
          "--noinform"
          "--non-interactive"
          "--eval"
          "(require :asdf)"
          "--eval"
          (format nil
                  "(asdf:initialize-source-registry '(:source-registry (:directory #P~S) :inherit-configuration))"
                  (namestring root))
          "--eval"
          (format nil "(asdf:load-asd #P~S)" (namestring asd))
          "--eval"
          "(asdf:load-system :sbcl-workers)"
          "--eval"
          "(sbcl-workers:sbcl-worker-main)")))

(defun test-environment (root &key (working-directory root) context)
  "Return a worker environment rooted beneath ROOT."
  (sbcl-worker-environment-create
   :pristine-command (test-pristine-command)
   :working-directory working-directory
   :image-root (merge-pathnames "images/" root)
   :source-revision-function (lambda () "test-revision")
   :context context))

(defun test-write-sparse-core (pathname)
  "Write a sparse file large enough for saved-core shape validation."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :element-type '(unsigned-byte 8))
    (file-position stream +minimum-sbcl-worker-core-size+)
    (write-byte 0 stream))
  pathname)

(defun test-write-text (pathname content)
  "Replace PATHNAME with exact UTF-8 CONTENT."
  (ensure-directories-exist pathname)
  (with-open-file (stream pathname
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create
                          :external-format :utf-8)
    (write-string content stream))
  pathname)

(defun test-write-source-audit-system (directory marker)
  "Write a source-audit ASDF system beneath DIRECTORY using MARKER."
  (let ((asd (merge-pathnames "sbcl-workers-source-audit.asd" directory)))
    (test-write-text
     asd
     (format nil
             "(asdf:defsystem #:sbcl-workers-source-audit~%  :serial t~%  :components ((:file \"source\")))~%"))
    (test-write-text
     (merge-pathnames "source.lisp" directory)
     (format nil
             "(defparameter cl-user::*sbcl-workers-source-audit* ~S)~%"
             marker))
    asd))

(defun test-worker-names ()
  "Test the public worker-name predicate and structured validation failure."
  (dolist (name '("default" "alpha-1" "worker_name"))
    (test-assert (eq (sbcl-worker-name-p name) t)
                 (format nil "~S is a valid worker name" name)))
  (dolist (name (list nil "" "with space" "slash/name"
                      (make-string 81 :initial-element #\a)))
    (test-assert (null (sbcl-worker-name-p name))
                 (format nil "~S is not a valid worker name" name)))
  (let* ((root (test-root))
         (environment (test-environment root)))
    (unwind-protect
         (test-assert
          (handler-case
              (progn
                (sbcl-worker-create environment :name "bad/name")
                nil)
            (sbcl-worker-error (condition)
              (and (eq (sbcl-worker-error-operation condition) :workers)
                   (eq (sbcl-worker-error-stage condition) :name))))
          "worker creation preserves its structured invalid-name error")
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(defun test-runtime ()
  "Test portable evaluation success and condition responses."
  (let* ((circular (list :root))
         (success
          (sbcl-worker-handle-request
           '(:request :id 1 :operation :eval :arguments (:form "(+ 20 22)"))))
         (failure
          (sbcl-worker-handle-request
           '(:request :id 2 :operation :eval :arguments (:form "(/ 1 0)")))))
    (setf (rest circular) circular)
    (test-assert (search "#1=" (sbcl-worker-render-value circular))
                 "value rendering safely represents circular structure")
    (test-assert (equal (getf (rest success) :values) '("42"))
                 "the runtime returns rendered evaluation values")
    (test-assert (eq (getf (rest failure) :status) :error)
                 "the runtime serializes evaluation conditions")
    (test-assert (stringp (getf (rest failure) :backtrace))
                 "runtime failures include a portable backtrace")))

  (let* ((root (test-root))
         (old-asd
           (test-write-source-audit-system
            (merge-pathnames "old/" root) :old))
         (new-asd
           (test-write-source-audit-system
            (merge-pathnames "new/" root) :new)))
    (unwind-protect
         (progn
           (asdf:load-asd old-asd)
           (asdf:load-system :sbcl-workers-source-audit)
           (test-assert
            (eq (symbol-value 'cl-user::*sbcl-workers-source-audit*) :old)
            "the audit fixture starts from its first registered definition")
           (let ((response
                   (sbcl-worker-handle-request
                    (list :request
                          :id 3
                          :operation :load-system
                          :arguments
                          (list :system :sbcl-workers-source-audit
                                :asd-pathname (namestring new-asd))))))
             (test-assert
              (and (eq (getf (rest response) :status) :ok)
                   (eq (symbol-value 'cl-user::*sbcl-workers-source-audit*) :new)
                   (search (namestring (truename new-asd))
                           (first (getf (rest response) :values))))
              "load-system replaces a stale registered system from an exact ASD file"))
           (asdf:clear-system :sbcl-workers-source-audit)
           (asdf:load-asd old-asd)
           (asdf:load-system :sbcl-workers-source-audit)
           (let ((response
                   (sbcl-worker-handle-request
                    (list :request
                          :id 4
                          :operation :run-tests
                          :arguments
                          (list :system :sbcl-workers-source-audit
                                :asd-pathname (namestring new-asd))))))
             (test-assert
              (and (eq (getf (rest response) :status) :ok)
                   (eq (symbol-value 'cl-user::*sbcl-workers-source-audit*) :new)
                   (search (namestring (truename new-asd))
                           (first (getf (rest response) :values))))
              "run-tests replaces a stale registered system from an exact ASD file"))
      (asdf:clear-system :sbcl-workers-source-audit)
      (when (boundp 'cl-user::*sbcl-workers-source-audit*)
        (makunbound 'cl-user::*sbcl-workers-source-audit*))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(defun test-images ()
  "Test immutable manifests, compatibility, scans, and structured errors."
  (let* ((root (test-root))
         (environment (test-environment root))
         (identifier "instrumented")
         (directory (merge-pathnames "images/instrumented/" root))
         (core (merge-pathnames "worker.core" directory)))
    (unwind-protect
         (progn
           (test-write-sparse-core core)
           (let ((image
                   (sbcl-worker-image-publish-manifest
                    environment
                    :identifier identifier
                    :parent-identifier
                    +pristine-sbcl-worker-image-identifier+
                    :note "Carries compiler instrumentation."
                    :core-pathname core
                    :source-revision "abc123")))
             (test-assert
              (string= (sbcl-worker-image-identifier image) identifier)
              "published images retain their identifier")
             (test-assert (sbcl-worker-image-compatible-p image)
                          "a manifest from the current host is compatible"))
           (multiple-value-bind (images failures)
               (sbcl-worker-image-scan environment)
             (test-assert (and (= (length images) 1) (null failures))
                          "image scans return valid immutable manifests"))
           (test-assert
            (handler-case
                (progn
                  (sbcl-worker-image-publish-manifest
                   environment
                   :identifier identifier
                   :parent-identifier
                   +pristine-sbcl-worker-image-identifier+
                   :note "Duplicate."
                   :core-pathname core)
                  nil)
              (sbcl-worker-image-error ()
                t))
            "an image identifier cannot be published twice")
           (test-assert
            (handler-case
                (progn
                  (sbcl-worker-image-validate-identifier "pristine")
                  nil)
              (sbcl-worker-image-error (condition)
                (eq (sbcl-worker-error-operation condition) :images)))
            "the pristine identifier is reserved with a structured error"))
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun test-pool ()
  "Test isolated heaps, persistence, workspace changes, reset, and removal."
  (let* ((root (test-root))
         (environment (test-environment root :context :original))
         (pool (sbcl-worker-pool-create environment)))
    (ensure-directories-exist (merge-pathnames "marker" root))
    (unwind-protect
         (let* ((alpha (sbcl-worker-pool-start pool "alpha" "pristine"))
                (beta (sbcl-worker-pool-start pool "beta" "pristine")))
           (sbcl-worker-request
            alpha :eval '(:form "(defparameter *pool-value* 41)"))
           (test-assert
            (equal (getf (rest (sbcl-worker-request
                                alpha :eval '(:form "(1+ *pool-value*)")))
                         :values)
                   '("42"))
            "a named worker retains its heap")
           (test-assert
            (equal (getf (rest (sbcl-worker-request
                                beta :eval '(:form "(boundp '*pool-value*)")))
                         :values)
                   '("NIL"))
            "separate workers do not share heap state")
           (test-assert
            (eq (getf (rest (sbcl-worker-request
                             alpha
                             :load-system
                             '(:system :sbcl-workers/read-eval-fixture)))
                      :status)
                :ok)
            "request execution permits standard reader evaluation")
           (test-assert
            (equal (getf (rest (sbcl-worker-request
                                alpha
                                :eval
                                '(:form "cl-user::*sbcl-worker-reader-evaluated-value*")))
                         :values)
                   '("42"))
            "reader evaluation computes dependency source forms")
           (test-assert (search "alpha  running  image pristine"
                                (sbcl-worker-pool-render pool))
                        "the pool reports worker state and image identity")
           (let ((moved (merge-pathnames "moved/" root)))
             (ensure-directories-exist (merge-pathnames "marker" moved))
             (let ((moved-environment
                     (test-environment root
                                       :working-directory moved
                                       :context :moved)))
               (sbcl-worker-pool-change-working-directory
                pool moved-environment)
               (test-assert
                (search (namestring moved)
                        (first
                         (getf
                          (rest
                           (sbcl-worker-request
                            alpha :eval
                            '(:form "(namestring (uiop:getcwd))")))
                          :values)))
                "a workspace change updates live process directories")
               (test-assert
                (eq (sbcl-worker-environment-context
                     (sbcl-worker-pool-environment pool))
                    :moved)
                "the pool retains the new opaque host context")))
           (let ((missing-environment
                   (test-environment
                    root
                    :working-directory (merge-pathnames "missing/" root)
                    :context :invalid)))
             (test-assert
              (handler-case
                  (progn
                    (sbcl-worker-pool-change-working-directory
                     pool missing-environment)
                    nil)
                (sbcl-worker-error ()
                  t))
              "a failed workspace change signals a worker condition")
             (test-assert
              (eq (sbcl-worker-environment-context
                   (sbcl-worker-pool-environment pool))
                  :moved)
              "a failed workspace change preserves the pool environment"))
           (test-assert
            (handler-case
                (progn
                  (sbcl-worker-pool-start pool "alpha" "other")
                  nil)
              (sbcl-worker-error ()
                t))
            "an existing worker cannot switch images implicitly")
           (sbcl-worker-pool-reset pool "alpha" "pristine")
           (test-assert
            (equal
             (getf (rest (sbcl-worker-request
                          (sbcl-worker-pool-worker pool "alpha")
                          :eval
                          '(:form "(boundp '*pool-value*)")))
                   :values)
             '("NIL"))
            "reset replaces only the named worker heap")
           (sbcl-worker-pool-stop pool "beta")
           (test-assert (not (search "beta" (sbcl-worker-pool-render pool)))
                        "stopping a worker removes it from the pool"))
      (sbcl-worker-pool-stop-all pool)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun test-worker-request-cancellation ()
  "Test an interrupted request detaches promptly and restarts from a clean heap."
  (let* ((root (test-root))
         (environment (test-environment root))
         (worker (sbcl-worker-create environment :name "cancel"))
         (marker (merge-pathnames "request-started" root))
         (request-thread nil))
    (ensure-directories-exist marker)
    (unwind-protect
         (progn
           (setf request-thread
                 (sb-thread:make-thread
                  (lambda ()
                    (handler-case
                        (sbcl-worker-request
                         worker
                         :eval
                         (list
                          :form
                          (format
                           nil
                           "(progn (with-open-file (stream ~S :direction :output :if-exists :supersede :if-does-not-exist :create) (write-line \"started\" stream)) (defparameter *cancelled-worker-state* t) (sleep 30))"
                           (namestring marker))))
                      (serious-condition ()
                        nil)))
                  :name "SBCL worker cancellation test"))
           (loop repeat 300
                 until (probe-file marker)
                 do (sleep 0.05))
           (test-assert (probe-file marker)
                        "the cancelled worker request reaches its process")
           (sb-thread:interrupt-thread
            request-thread
            (lambda ()
              (sbcl-worker-cancel-request worker)
              (error "Cancel the active worker request.")))
           (sb-thread:join-thread request-thread :timeout 5)
           (test-assert (not (sb-thread:thread-alive-p request-thread))
                        "request cancellation promptly unwinds the caller")
           (test-assert (not (sbcl-worker-running-p worker))
                        "request cancellation detaches the interrupted process")
           (test-assert
            (equal
             (getf
              (rest
               (sbcl-worker-request
                worker :eval '(:form "(boundp '*cancelled-worker-state*)")))
              :values)
             '("NIL"))
            "the next request starts from a clean protocol process"))
      (sbcl-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(defun test-image-snapshot ()
  "Test forked heap saving, probing, publication, and independent cloning."
  (let* ((root (test-root))
         (environment (test-environment root))
         (pool (sbcl-worker-pool-create environment)))
    (ensure-directories-exist (merge-pathnames "marker" root))
    (unwind-protect
         (let ((source (sbcl-worker-pool-start pool "source" "pristine")))
           (sbcl-worker-request
            source :eval
            '(:form "(defparameter *saved-worker-marker* 9001)"))
           (let ((image
                   (sbcl-worker-save-image
                    environment source
                    :identifier "diddled"
                    :note "Carries a marker proving that the heap was retained.")))
             (test-assert
              (and (string= (sbcl-worker-image-identifier image) "diddled")
                   (sbcl-worker-image-plausible-core-p
                    (sbcl-worker-image-core-pathname image)))
              "saving publishes a plausible immutable core")
             (test-assert (sbcl-worker-running-p source)
                          "saving leaves the parent worker running")
             (let ((clone
                     (sbcl-worker-pool-start pool "clone" "diddled")))
               (test-assert
                (equal
                 (getf (rest (sbcl-worker-request
                              clone :eval '(:form "*saved-worker-marker*")))
                       :values)
                 '("9001"))
                "a clone inherits the saved heap"))))
      (sbcl-worker-pool-stop-all pool)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun run-tests ()
  "Run the complete sbcl-workers test suite and return true."
  (setf *tests-run* 0)
  (test-worker-names)
  (test-runtime)
  (test-images)
  (test-pool)
  (test-worker-request-cancellation)
  (test-image-snapshot)
  (format t "~&sbcl-workers: ~D tests passed.~%" *tests-run*)
  t)
