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
           '(:request :id 1 :operation :eval :arguments (:forms ("(+ 20 22)")))))
         (failure
          (sbcl-worker-handle-request
           '(:request :id 2 :operation :eval :arguments (:forms ("(/ 1 0)"))))))
    (setf (rest circular) circular)
    (test-assert (search "#1=" (sbcl-worker-render-value circular))
                 "value rendering safely represents circular structure")
    (test-assert (equal (getf (rest success) :values) '("42"))
                 "the runtime returns rendered evaluation values")
    (test-assert (eq (getf (rest failure) :status) :error)
                 "the runtime serializes evaluation conditions")
    (test-assert (stringp (getf (rest failure) :backtrace))
                 "runtime failures include a portable backtrace")))
  (let ((response
          (sbcl-worker-handle-request
           '(:request :id 3 :operation :load-system :arguments (:system :asdf)))))
    (test-assert
     (eq (getf (rest response) :status) :ok)
     "systems without standalone ASD source files still load successfully"))

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
                          :id 4
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
                          :id 5
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
            alpha :eval '(:forms ("(defparameter *pool-value* 41)")))
           (test-assert
            (equal (getf (rest (sbcl-worker-request
                                alpha :eval '(:forms ("(1+ *pool-value*)"))))
                         :values)
                   '("42"))
            "a named worker retains its heap")
           (test-assert
            (equal (getf (rest (sbcl-worker-request
                                beta :eval '(:forms ("(boundp '*pool-value*)"))))
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
                                '(:forms ("cl-user::*sbcl-worker-reader-evaluated-value*"))))
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
                            '(:forms ("(namestring (uiop:getcwd))"))))
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
                          '(:forms ("(boundp '*pool-value*)"))))
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
                          :forms
                          (list
                           (format
                            nil
                            "(progn (with-open-file (stream ~S :direction :output :if-exists :supersede :if-does-not-exist :create) (write-line \"started\" stream)) (defparameter *cancelled-worker-state* t) (sleep 30))"
                            (namestring marker)))))
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
                worker :eval '(:forms ("(boundp '*cancelled-worker-state*)"))))
              :values)
             '("NIL"))
            "the next request starts from a clean protocol process"))
      (sbcl-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(defun test-computed-pristine-command ()
  "Test a pristine command function is consulted at every start and validated."
  (let* ((root (test-root))
         (calls 0)
         (environment
           (sbcl-worker-environment-create
            :pristine-command (lambda ()
                                (incf calls)
                                (test-pristine-command))
            :working-directory root
            :image-root (merge-pathnames "images/" root)))
         (worker (sbcl-worker-create environment :name "computed"))
         (invalid
           (sbcl-worker-create
            (sbcl-worker-environment-create
             :pristine-command (lambda () (list "sbcl" ""))
             :working-directory root
             :image-root (merge-pathnames "images/" root))
            :name "invalid")))
    (ensure-directories-exist root)
    (unwind-protect
         (flet ((answer ()
                  "Return the worker's rendered answer to a fixed form."
                  (getf (rest (sbcl-worker-request
                               worker :eval '(:forms ("(+ 40 2)"))))
                        :values)))
           (test-assert (equal (answer) '("42"))
                        "a computed pristine command starts a worker")
           (sbcl-worker-stop worker)
           (test-assert (and (equal (answer) '("42")) (= calls 2))
                        "every pristine start computes its command again")
           (test-assert
            (handler-case
                (progn
                  (sbcl-worker-request invalid :eval '(:forms ("1")))
                  nil)
              (sbcl-worker-error ()
                t))
            "an invalid computed command is rejected"))
      (sbcl-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(defun test-form-sequences ()
  "Test several forms are read and evaluated in order and failures name their form."
  (let* ((root (test-root))
         (worker (sbcl-worker-create (test-environment root) :name "sequences")))
    (ensure-directories-exist root)
    (unwind-protect
         (flet ((request (operation &rest forms)
                  "Send FORMS to the worker with OPERATION and return the response plist."
                  (rest (sbcl-worker-request worker operation (list :forms forms)))))
           (let ((late (request :eval
                                "(defpackage #:sbcl-workers-test-late (:use #:cl) (:export #:answer))"
                                "(defun sbcl-workers-test-late:answer () (format t \"late~%\") 42)"
                                "(sbcl-workers-test-late:answer)")))
             (test-assert (and (eq (getf late :status) :ok)
                               (equal (getf late :values) '("42"))
                               (search "late" (getf late :output)))
                          "a later form reads a package an earlier form created"))
           (let ((failed (request :eval
                                  "(defparameter *sequence-marker* 1)"
                                  "(error \"second form failed\")"
                                  "(defparameter *sequence-marker* 3)")))
             (test-assert (and (eq (getf failed :status) :error)
                               (search "second form failed" (getf failed :message))
                               (eql (getf failed :form-index) 2)
                               (eql (getf failed :form-count) 3))
                          "a failure reports which form of how many failed")
             (test-assert (equal (getf (request :eval "*sequence-marker*") :values) '("1"))
                          "forms before a failure keep their effects and later forms do not run"))
           (let ((unreadable (request :eval "1" "(list 2")))
             (test-assert (and (eq (getf unreadable :status) :error)
                               (eql (getf unreadable :form-index) 2))
                          "a form that fails to read is named by its position"))
           (test-assert (equal (getf (request :compile "(defparameter *compiled* 5)" "(* *compiled* 2)")
                                     :values)
                               '("10"))
                        "compiled sequences return the last form's values")
           (let ((empty (rest (sbcl-worker-request worker :eval '(:forms ())))))
             (test-assert (and (eq (getf empty :status) :error)
                               (null (getf empty :form-index)))
                          "an empty form list is refused")))
      (sbcl-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(defun test-request-diagnostics ()
  "Test user code prints freely and failures keep their output and failing frames."
  (let* ((root (test-root))
         (worker (sbcl-worker-create (test-environment root) :name "diagnostics")))
    (ensure-directories-exist root)
    (unwind-protect
         (flet ((request (form)
                  "Evaluate FORM in the worker and return the response plist."
                  (rest (sbcl-worker-request worker :eval (list :forms (list form))))))
           (let ((printed (request "(progn (prin1 (make-condition 'simple-error :format-control \"unreadable\")) 7)")))
             (test-assert (and (eq (getf printed :status) :ok)
                               (equal (getf printed :values) '("7"))
                               (search "#<SIMPLE-ERROR" (getf printed :output)))
                          "user code prints unreadable objects with ordinary printer settings"))
           (let ((failed (request "(progn (format t \"compiler-diagnostic-line~%\") (sbcl-workers-test-failing-frame))")))
             (test-assert (eq (getf failed :status) :error)
                          "an undefined function fails the request")
             (test-assert (search "compiler-diagnostic-line" (getf failed :output))
                          "a failed request keeps the output printed before its error")
             (test-assert (search "SBCL-WORKERS-TEST-FAILING-FRAME" (getf failed :backtrace))
                          "the backtrace shows the frames where the error was signaled"))
           (let ((report (request "(progn (define-condition sbcl-workers-test-bad-report (error) () (:report (lambda (condition stream) (declare (ignore condition stream)) (error \"report broke\")))) (error 'sbcl-workers-test-bad-report))")))
             (test-assert (and (eq (getf report :status) :error)
                               (search "SBCL-WORKERS-TEST-BAD-REPORT" (getf report :message)))
                          "a condition whose report fails is still named in the response"))
           (let ((trailing (request "(list 1 (+ 1 2)) ) (print :lost)")))
             (test-assert (and (eq (getf trailing :status) :error)
                               (search "ends at character 16" (getf trailing :message))
                               (search ") (print :lost)" (getf trailing :message)))
                          "text after the first form is refused with where that form ended"))
           (test-assert (equal (getf (request (format nil "(+ 1 2) ; checked~%")) :values) '("3"))
                        "a trailing comment after the one form is accepted"))
      (sbcl-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  nil)

(defun test-early-exit-diagnostics ()
  "Test a worker that dies before its handshake reports its status and error output."
  (let ((root (test-root)))
    (ensure-directories-exist root)
    (flet ((start-failure (form)
             "Return the start failure message of a worker running FORM, or NIL."
             (let ((worker (sbcl-worker-create
                            (sbcl-worker-environment-create
                             :pristine-command (list "sbcl" "--noinform" "--non-interactive"
                                                     "--no-userinit" "--eval" form)
                             :working-directory root
                             :image-root (merge-pathnames "images/" root))
                            :name "early-exit")))
               (handler-case (progn (sbcl-worker-start worker) nil)
                 (sbcl-worker-error (condition)
                   (and (eq (sbcl-worker-error-stage condition) :handshake)
                        (sbcl-worker-error-message condition)))))))
      (unwind-protect
           (let ((noisy (start-failure "(progn (format *error-output* \"boot-failure-marker~%\") (sb-ext:exit :code 3 :abort t))"))
                 (silent (start-failure "(sb-ext:exit :code 4 :abort t)")))
             (test-assert (and noisy
                               (search "exit status 3" noisy)
                               (search "boot-failure-marker" noisy))
                          "an early exit reports the status and the boot error output")
             (test-assert (and silent
                               (search "exit status 4" silent)
                               (search "no error output" silent))
                          "a silent early exit says it wrote no error output"))
        (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))
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
            '(:forms ("(defparameter *saved-worker-marker* 9001)")))
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
                              clone :eval '(:forms ("*saved-worker-marker*"))))
                       :values)
                 '("9001"))
                "a clone inherits the saved heap"))))
      (sbcl-worker-pool-stop-all pool)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore))))

(defun test-output-bounds ()
  "Test long evaluation output keeps its head and tail around a marker."
  (let* ((response
           (sbcl-worker-handle-request
            '(:request :id 4 :operation :eval
              :arguments
              (:forms ("(progn (dotimes (i 4000) (format t \"line-~4,'0D~%\" i)) :done)")))))
         (output (getf (rest response) :output)))
    (test-assert (eq (getf (rest response) :status) :ok)
                 "the long-output evaluation succeeds")
    (test-assert (<= (length output) 13000)
                 "captured output is bounded near the configured limit")
    (test-assert (and (search "line-0000" output)
                      (search "line-3999" output)
                      (search "characters dropped" output))
                 "bounded output keeps its head and tail around the marker")))

(defun test-source-not-found ()
  "Name the requested definition when no SBCL source location matches it."
  (let ((message
          (handler-case
              (progn
                (sbcl-worker-source "cl-user::sbcl-workers-missing-definition" nil)
                nil)
            (sbcl-worker-error (condition)
              (sbcl-worker-error-message condition)))))
    (test-assert (and message
                      (search "SBCL-WORKERS-MISSING-DEFINITION" message)
                      (search "No SBCL definition source" message))
                 "a missing definition is reported by name for every kind"))
  (let ((message
          (handler-case
              (progn
                (sbcl-worker-source "cl-user::sbcl-workers-missing-definition"
                                    "function")
                nil)
            (sbcl-worker-error (condition)
              (sbcl-worker-error-message condition)))))
    (test-assert (and message
                      (search "SBCL-WORKERS-MISSING-DEFINITION" message)
                      (search "No function definition source" message))
                 "a missing definition is reported by name for one kind")))

(defun test-source-recorded-project-file ()
  "Read a loaded definition from its recorded file without any matching SBCL source."
  (let* ((root (test-root))
         (source (merge-pathnames "recorded-definition.lisp" root)))
    (test-write-text
     source
     (format nil "(in-package :cl-user)~%(defun sbcl-workers-recorded-definition (x)~%  (list :recorded x))~%"))
    (let ((*error-output* (make-broadcast-stream))
          (*standard-output* (make-broadcast-stream)))
      (load (compile-file source
                          :output-file (merge-pathnames "recorded-definition.fasl" root))))
    (let ((sbcl-workers::*worker-source-root-environment-variable*
            "SBCL_WORKERS_TESTS_UNSET_SOURCE_ROOT"))
      (let ((output (nth-value 1 (sbcl-worker-source
                                  "cl-user::sbcl-workers-recorded-definition"
                                  "function"))))
        (test-assert (and (search "(defun sbcl-workers-recorded-definition" output)
                          (search (namestring (truename source)) output))
                     "a loaded definition reads from its recorded file without matching SBCL source")))))

(defun run-tests ()
  "Run the complete sbcl-workers test suite and return true."
  (setf *tests-run* 0)
  (test-source-not-found)
  (test-source-recorded-project-file)
  (test-worker-names)
  (test-runtime)
  (test-output-bounds)
  (test-images)
  (test-pool)
  (test-worker-request-cancellation)
  (test-computed-pristine-command)
  (test-request-diagnostics)
  (test-form-sequences)
  (test-early-exit-diagnostics)
  (test-image-snapshot)
  (format t "~&sbcl-workers: ~D tests passed.~%" *tests-run*)
  t)
