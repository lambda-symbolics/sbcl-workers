(in-package #:sbcl-workers/tests)

(defun host-test-eval (worker form dispatcher &rest options)
  "Evaluate FORM in a real worker with DISPATCHER and OPTIONS."
  (apply #'sbcl-workers:sbcl-worker-host-request worker :eval
         (list :forms (list form)) :dispatcher dispatcher options))

(defun host-test-value (response)
  "Read the first printed evaluation value from RESPONSE."
  (let ((*read-eval* nil))
    (read-from-string (first (getf (rest response) :values)))))

(defun host-test-code (thunk code)
  "Assert THUNK raises the requested structured callback failure."
  (test-assert
   (handler-case (progn (funcall thunk) nil)
     (sbcl-workers:sbcl-worker-host-error (condition)
       (eq code (sbcl-workers:sbcl-worker-host-error-code condition))))
   (format nil "Structured failure ~A" code)))

(defun host-test-dispatch (payload &key context identity cancelled-p)
  "Echo callback PAYLOAD with optional inherited CONTEXT."
  (declare (ignore identity cancelled-p))
  (list :payload payload :context context))

(defun run-host-callback-tests ()
  "Exercise host callbacks and failure cleanup through persistent subprocesses."
  (let* ((*tests-run* 0) (root (test-root))
         (environment (test-environment root))
         (worker (sbcl-worker-create environment :name "host-callback-test")))
    (ensure-directories-exist (merge-pathnames "placeholder" root))
    (unwind-protect
         (progn
           (sbcl-worker-start worker)
           (sbcl-workers::host--write :malformed (sbcl-workers::worker--input worker) 256)
           (let ((*read-eval* nil))
             (test-assert
              (eq (getf (rest (read (sbcl-workers::worker--output worker))) :status) :error)
              "Malformed ordinary packet does not crash the optional response hook"))
           (test-assert
            (equalp (host-test-value
                     (host-test-eval worker
                      "(sbcl-workers:sbcl-worker-host-call (vector t nil :keyword 1/3 1.25d0 (loop for i below 80 collect i) (format nil \"a~%b(\\\"c\")))"
                      (lambda (payload &key context identity cancelled-p)
                        (declare (ignore context identity cancelled-p)) payload)))
                    (vector t nil :keyword 1/3 1.25d0
                            (loop for i below 80 collect i) (format nil "a~%b(\"c")))
            "Portable vectors, booleans, numbers, long lists and escaped multiline strings")
           (let ((identities nil) (opaque (make-hash-table)))
             (test-assert
              (equal
               (host-test-value
                (host-test-eval
                 worker
                 "(list (sbcl-workers:sbcl-worker-host-context) (sbcl-workers:sbcl-worker-host-call :first) (sbcl-workers:sbcl-worker-host-call :second))"
                 (lambda (payload &key context identity cancelled-p)
                   (declare (ignore cancelled-p))
                   (test-assert (eq context opaque) "Host-local context is opaque")
                   (push identity identities)
                   payload)
                 :context opaque :worker-context '(:budget 9)))
               '((:budget 9) :first :second))
              "Sequential callbacks and portable inherited context")
             (test-assert (= (getf (first identities) :call-id) 2) "Monotonic call IDs")
             (test-assert (= (getf (second identities) :call-id) 1) "Initial call ID")
             (test-assert (string= (getf (first identities) :session)
                                   (getf (second identities) :session)) "Exact shared session")
             (test-assert (= (getf (first identities) :request-id)
                             (getf (second identities) :request-id)) "Exact evaluation identity"))
           (test-assert
            (equal (host-test-value
                    (host-test-eval worker
                     "(handler-case (sbcl-workers:sbcl-worker-host-call :fail) (sbcl-workers:sbcl-worker-host-error (c) (list (sbcl-workers:sbcl-worker-host-error-code c) (sbcl-workers:sbcl-worker-host-error-remote-type c))))"
                     (lambda (payload &key context identity cancelled-p)
                       (declare (ignore payload context identity cancelled-p))
                       (error 'simple-error :format-control "host secret"))))
                   '(:host-failure "SIMPLE-ERROR"))
            "Typed host failure without condition report leakage")
           (dolist (case '(("(make-string 300 :initial-element #\\a)" :bound)
                           ("(make-hash-table)" :non-portable)
                           ("(let ((x (list 1))) (setf (cdr x) x) x)" :non-portable)))
             (test-assert
              (eq (host-test-value
                   (host-test-eval worker
                    (format nil "(handler-case (sbcl-workers:sbcl-worker-host-call ~A) (sbcl-workers:sbcl-worker-host-error (c) (sbcl-workers:sbcl-worker-host-error-code c)))" (first case))
                    #'host-test-dispatch :request-limit 256))
                  (second case))
              "Worker request bound and portable-tree validation"))
           (test-assert
            (eq (host-test-value
                 (host-test-eval worker
                  "(handler-case (sbcl-workers:sbcl-worker-host-call :oversize) (sbcl-workers:sbcl-worker-host-error (c) (sbcl-workers:sbcl-worker-host-error-code c)))"
                  (lambda (payload &key context identity cancelled-p)
                    (declare (ignore payload context identity cancelled-p))
                    (make-string 300 :initial-element #\a)) :result-limit 256)) :bound)
            "Host result bound is returned as a typed worker error")
           (test-assert
            (eq (host-test-value
                 (host-test-eval worker
                  "(handler-case (sbcl-workers:sbcl-worker-host-call :nested) (sbcl-workers:sbcl-worker-host-error (c) (sbcl-workers:sbcl-worker-host-error-code c)))"
                  (lambda (payload &key context identity cancelled-p)
                    (declare (ignore payload context identity cancelled-p))
                    (sbcl-worker-request worker :eval '(:forms ("42")))))) :reentrancy)
            "Same-worker ordinary request from dispatcher is rejected without deadlock")
           (test-assert
            (eq (host-test-value
                 (host-test-eval worker
                  "(handler-case (let ((sbcl-workers::*host-runtime-in-call* t)) (sbcl-workers:sbcl-worker-host-call :nested)) (sbcl-workers:sbcl-worker-host-error (c) (sbcl-workers:sbcl-worker-host-error-code c)))"
                  #'host-test-dispatch)) :reentrancy)
            "Nested worker callback is rejected")
           (test-assert
            (equal (host-test-value
                    (host-test-eval worker
                     "(progn (handler-case (sbcl-workers:sbcl-worker-host-call (make-string 300)) (sbcl-workers:sbcl-worker-host-error () nil)) (sbcl-workers:sbcl-worker-host-call :after-bound))"
                     #'host-test-dispatch :request-limit 256))
                   '(:payload :after-bound :context nil))
            "A rejected payload does not consume a transmitted call ID")
           (test-assert
            (eq (host-test-value
                 (host-test-eval worker
                  "(sb-thread:join-thread (sb-thread:make-thread (lambda () (handler-case (sbcl-workers:sbcl-worker-host-call :thread) (sbcl-workers:sbcl-worker-host-error (c) (sbcl-workers:sbcl-worker-host-error-code c))))))"
                  #'host-test-dispatch)) :unavailable)
            "Spawned worker threads cannot use a parent callback transport")
           (let ((other (sbcl-worker-create environment :name "other-worker")))
             (unwind-protect
                  (test-assert
                   (= (host-test-value
                       (host-test-eval worker "(sbcl-workers:sbcl-worker-host-call :other)"
                        (lambda (payload &key context identity cancelled-p)
                          (declare (ignore payload context identity cancelled-p))
                          (host-test-value (sbcl-worker-request other :eval
                                             '(:forms ("(+ 40 2)"))))))) 42)
                   "Different-worker requests from the dispatcher are allowed")
               (sbcl-worker-stop other)))
           (test-assert
            (eq (host-test-value
                 (host-test-eval worker "(sbcl-workers:sbcl-worker-host-call :immutable)"
                  (lambda (payload &key context identity cancelled-p)
                    (declare (ignore context cancelled-p))
                    (setf (char (getf identity :session) 0) #\X)
                    payload))) :immutable)
            "Dispatcher identity mutation cannot change the reply correlation")
           (let ((called nil))
             (host-test-code
              (lambda ()
                (host-test-eval worker
                 "(progn (setf (getf sbcl-workers::*host-runtime* :session) \"wrong\") (sbcl-workers:sbcl-worker-host-call :wrong))"
                 (lambda (payload &key context identity cancelled-p)
                   (declare (ignore payload context identity cancelled-p))
                   (setf called t)))) :identity)
             (test-assert (not called) "Identity mismatch is rejected before host dispatch")
             (test-assert (not (sbcl-worker-running-p worker))
                          "Identity mismatch discards protocol state"))
           (host-test-code
            (lambda ()
              (host-test-eval worker
               "(progn (write-string (make-string 6000 :initial-element #\\x) (getf sbcl-workers::*host-runtime* :output)) (finish-output (getf sbcl-workers::*host-runtime* :output)) (sleep 60))"
               #'host-test-dispatch :request-limit 512 :result-limit 256)) :bound)
           (host-test-code
            (lambda () (host-test-eval worker "42" #'host-test-dispatch
                                      :worker-context (make-hash-table))) :non-portable)
           (let ((cancel nil) (unwound nil) (token nil))
             (host-test-code
              (lambda ()
                (host-test-eval worker "(sbcl-workers:sbcl-worker-host-call :wait)"
                 (lambda (payload &key context identity cancelled-p)
                   (declare (ignore payload context identity))
                   (setf token cancelled-p cancel t)
                   (unwind-protect (sleep 60) (setf unwound t)))
                 :cancel-p (lambda () cancel))) :cancelled)
             (test-assert unwound "Cancellation unwinds supervised host dispatch")
             (test-assert (funcall token) "Cancellation propagates to host predicate")
             (test-assert (not (sbcl-worker-running-p worker)) "Cancelled worker is detached"))
           (let ((deadline (+ (get-internal-real-time) internal-time-units-per-second)))
             (host-test-code
              (lambda () (host-test-eval worker "(sleep 60)" #'host-test-dispatch
                           :cancel-p (lambda () (> (get-internal-real-time) deadline))))
              :cancelled))
           (test-assert
            (equal (getf (rest (sbcl-worker-request worker :eval '(:forms ("(+ 20 22)"))))
                         :values) '("42")) "Ordinary eval works after cancellation")
           (host-test-code
            (lambda () (host-test-eval worker "(sb-ext:exit :code 0)" #'host-test-dispatch))
            :worker-dead)
           (let ((unwound nil))
             (host-test-code
              (lambda ()
                (host-test-eval worker "(sbcl-workers:sbcl-worker-host-call :die)"
                 (lambda (payload &key context identity cancelled-p)
                   (declare (ignore payload context identity cancelled-p))
                   (unwind-protect
                        (progn (uiop:terminate-process (sbcl-workers::worker--process worker)
                                                       :urgent t)
                               (sleep 60))
                     (setf unwound t))))) :worker-dead)
             (test-assert unwound "Worker death unwinds active host dispatch"))
           ;; A real process with its host input closed must report EOF and exit.
           (sbcl-worker-request worker :eval
            (list :forms (list (format nil "(asdf:load-asd ~S)"
                                      (namestring (asdf:system-source-file :sbcl-workers)))
                               "(asdf:load-system :sbcl-workers/host-callbacks)")))
           ;; A wrong host reply must also be rejected in the real worker.
           (let ((input (sbcl-workers::worker--input worker))
                 (output (sbcl-workers::worker--output worker)))
             (sbcl-workers::host--write
              '(:request :id 899 :operation :host-request
                :arguments (:session "identity-test" :request-limit 256 :result-limit 256
                            :operation :eval
                            :arguments (:forms ("(handler-case (sbcl-workers:sbcl-worker-host-call :waiting) (sbcl-workers:sbcl-worker-host-error (c) (sbcl-workers:sbcl-worker-host-error-code c)))"))))
              input 4096)
             (let ((packet (sbcl-workers::host--read output 4096)))
               (test-assert (eq (first packet) :host-call) "Worker emitted identity test request")
               (sbcl-workers::host--write
                '(:host-result :identity (:session "wrong" :request-id 899 :call-id 1)
                  :status :ok :value :bad) input 4096)
               (test-assert (eq (host-test-value (sbcl-workers::host--read output 16384))
                                :identity) "Worker rejects mismatched reply identity")))
           (let ((process (sbcl-workers::worker--process worker))
                 (input (sbcl-workers::worker--input worker))
                 (output (sbcl-workers::worker--output worker)))
             (sbcl-workers::host--write
              '(:request :id 900 :operation :host-request
                :arguments (:session "disconnect" :request-limit 256 :result-limit 256
                            :operation :eval
                            :arguments (:forms ("(sbcl-workers:sbcl-worker-host-call :waiting)"))))
              input 4096)
             (test-assert (eq (first (sbcl-workers::host--read output 4096)) :host-call)
                          "Worker emits callback before disconnect")
             (close input)
             (let ((response (sbcl-workers::host--read output 16384)))
               (test-assert (eq (getf (rest response) :status) :error)
                            "Host disconnect yields error response"))
             (sbcl-workers::worker--wait-until
              (lambda () (not (uiop:process-alive-p process))))
             (test-assert (not (uiop:process-alive-p process))
                          "Host disconnect terminates worker without hangs"))
           (format t "~&sbcl-workers host callbacks: ~D assertions passed.~%" *tests-run*))
      (sbcl-worker-stop worker)
      (uiop:delete-directory-tree root :validate t :if-does-not-exist :ignore)))
  t)
