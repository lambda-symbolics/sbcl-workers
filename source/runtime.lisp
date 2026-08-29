(in-package #:sbcl-workers)

(defparameter *worker-image-identifier*
  +pristine-sbcl-worker-image-identifier+
  "The pristine or saved image identity reported by this worker process.")

(defparameter *worker-protocol-tag* :sbcl-worker
  "The first element emitted in this worker's handshake.")

(defparameter *worker-protocol-version* 1
  "The line-protocol version emitted in this worker's handshake.")

(defun sbcl-worker-runtime-configure
    (&key
       (evaluation-package *worker-evaluation-package-name*)
       (protocol-tag *worker-protocol-tag*)
       (protocol-version *worker-protocol-version*)
       (source-root-environment-variable
         *worker-source-root-environment-variable*))
  "Configure the protocol runtime embedded in pristine and saved workers."
  (unless (and (worker--non-empty-string-p evaluation-package)
               (keywordp protocol-tag)
               (typep protocol-version '(integer 1 *))
               (worker--non-empty-string-p
                source-root-environment-variable))
    (worker--signal-error
     "The SBCL worker runtime configuration is invalid."
     :operation :runtime
     :stage :configuration))
  (setf *worker-evaluation-package-name* evaluation-package
        *worker-protocol-tag* protocol-tag
        *worker-protocol-version* protocol-version
        *worker-source-root-environment-variable*
        source-root-environment-variable)
  nil)

(defun sbcl-worker-render-value (value)
  "Return a bounded, circularity-safe representation of VALUE."
  (worker--bounded-string
   (write-to-string value
                    :readably nil
                    :circle t
                    :level 10
                    :length 100)))

(defun worker--capture-evaluation (function)
  "Call FUNCTION while capturing output, returning values and output."
  (let ((result-values nil))
    (let ((output
            (with-output-to-string (stream)
              (let ((*standard-output* stream)
                    (*error-output* stream)
                    (*trace-output* stream)
                    (*debug-io* stream)
                    (*package* (worker--evaluation-package)))
                (setf result-values
                      (multiple-value-list (funcall function)))))))
      (values (mapcar #'sbcl-worker-render-value result-values) output))))

(defun worker--single-threaded-p ()
  "Return true when the worker has no live Lisp thread besides this one."
  (notany (lambda (thread)
            (and (not (eq thread sb-thread:*current-thread*))
                 (sb-thread:thread-alive-p thread)))
          (sb-thread:list-all-threads)))

(defun worker--save-image-child (pathname identifier)
  "Save this forked worker heap to PATHNAME with embedded IDENTIFIER."
  (handler-case
      (progn
        (setf *worker-image-identifier* identifier)
        (sb-ext:save-lisp-and-die
         (namestring pathname)
         :toplevel #'sbcl-worker-main
         :executable nil
         :purify nil
         :compression nil))
    (error ()
      (sb-posix:_exit 1)))
  nil)

(defun worker--save-image (pathname identifier)
  "Fork a saver for this worker heap and return portable result values."
  (unless (worker--single-threaded-p)
    (worker--signal-error
     "An SBCL worker image requires exactly one live Lisp thread."
     :operation :save-image))
  (when (probe-file pathname)
    (worker--signal-error
     "The unpublished SBCL worker core already exists."
     :operation :save-image
     :pathname pathname))
  (let ((saver-pid (sb-posix:fork)))
    (if (zerop saver-pid)
        (worker--save-image-child pathname identifier)
        (multiple-value-bind (waited-pid status)
            (sb-posix:waitpid saver-pid 0)
          (unless (and (= waited-pid saver-pid)
                       (sb-posix:wifexited status)
                       (zerop (sb-posix:wexitstatus status)))
            (worker--signal-error
             "The SBCL worker image saver failed."
             :operation :save-image
             :pathname pathname)))))
  (values (list (namestring pathname)) ""))

(defun worker--load-system (system)
  "Load SYSTEM with Quicklisp when present, otherwise through ASDF."
  (if (find-package '#:ql)
      (uiop:symbol-call '#:ql '#:quickload system)
      (asdf:load-system system)))

(defun worker--dispatch (operation arguments)
  "Execute worker OPERATION with portable ARGUMENTS."
  (ecase operation
    (:save-image
     (worker--save-image (pathname (getf arguments :pathname))
                         (getf arguments :identifier)))
    (:eval
     (worker--capture-evaluation
      (lambda ()
        (eval (worker--read-form (getf arguments :form))))))
    (:compile
     (worker--capture-evaluation
      (lambda ()
        (funcall
         (compile nil
                  `(lambda ()
                     ,(worker--read-form (getf arguments :form))))))))
    (:load-system
     (worker--capture-evaluation
      (lambda ()
        (worker--load-system (getf arguments :system)))))
    (:describe
     (worker--capture-evaluation
      (lambda ()
        (describe (worker--read-form (getf arguments :designator)))
        (values))))
    (:source
     (sbcl-worker-source (getf arguments :name)
                         (getf arguments :kind)))
    (:run-tests
     (worker--capture-evaluation
      (lambda ()
        (asdf:test-system (getf arguments :system)))))))

(defun worker--condition-backtrace ()
  "Return a bounded SBCL backtrace for the current worker condition."
  (worker--bounded-string
   (with-output-to-string (stream)
     (sb-debug:print-backtrace :stream stream :count 20))
   :limit 6000))

(defun sbcl-worker-handle-request (request)
  "Execute one portable worker REQUEST and return a protocol response."
  (let ((request-id (getf (rest request) :id))
        (operation (getf (rest request) :operation))
        (arguments (getf (rest request) :arguments)))
    (handler-case
        (multiple-value-bind (result-values output)
            (worker--dispatch operation arguments)
          (list :response
                :id request-id
                :status :ok
                :values result-values
                :output output))
      (error (condition)
        (list :response
              :id request-id
              :status :error
              :condition-type (string (type-of condition))
              :message (princ-to-string condition)
              :backtrace (worker--condition-backtrace))))))

(defun sbcl-worker-main
    (&key
       (evaluation-package *worker-evaluation-package-name*)
       (protocol-tag *worker-protocol-tag*)
       (protocol-version *worker-protocol-version*)
       (source-root-environment-variable
         *worker-source-root-environment-variable*))
  "Run the worker's line-oriented readable S-expression protocol until EOF."
  (sbcl-worker-runtime-configure
   :evaluation-package evaluation-package
   :protocol-tag protocol-tag
   :protocol-version protocol-version
   :source-root-environment-variable source-root-environment-variable)
  (let ((*package* (worker--evaluation-package))
        (*print-readably* t)
        (*print-circle* t))
    (prin1 (list *worker-protocol-tag*
                 :version *worker-protocol-version*
                 :image *worker-image-identifier*))
    (terpri)
    (finish-output)
    (loop for request = (let ((*read-eval* nil))
                          (read *standard-input* nil :end))
          until (eq request :end)
          do (let ((response
                     (if (and (listp request) (eq (first request) :request))
                         (sbcl-worker-handle-request request)
                         (list :response
                               :id nil
                               :status :error
                               :message "Malformed worker request."
                               :backtrace ""))))
               (prin1 response)
               (terpri)
               (finish-output))))
  nil)
