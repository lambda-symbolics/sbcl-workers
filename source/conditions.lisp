(in-package #:sbcl-workers)

(define-condition sbcl-worker-error (error)
  ((message
    :initarg       :message
    :reader        sbcl-worker-error-message
    :type          string
    :documentation "A concise description of the failure.")
   (operation
    :initarg       :operation
    :initform      nil
    :reader        sbcl-worker-error-operation
    :type          (or null string keyword)
    :documentation "The operation that failed, when known.")
   (pathname
    :initarg       :pathname
    :initform      nil
    :reader        sbcl-worker-error-pathname
    :type          (or null pathname)
    :documentation "The filesystem location involved, when any.")
   (stage
    :initarg       :stage
    :initform      nil
    :reader        sbcl-worker-error-stage
    :type          (or null keyword)
    :documentation "The lifecycle stage that failed, when known.")
   (cause
    :initarg       :cause
    :initform      nil
    :reader        sbcl-worker-error-cause
    :type          t
    :documentation "The underlying condition, when one was retained."))
  (:documentation "A failure in an isolated SBCL worker or its protocol.")
  (:report (lambda (condition stream)
             (write-string (sbcl-worker-error-message condition) stream))))

(define-condition sbcl-worker-image-error (sbcl-worker-error)
  ()
  (:documentation "A failure validating, saving, or publishing a worker image."))

(defun worker--non-empty-string-p (value)
  "Return true when VALUE is a string containing at least one character."
  (and (stringp value) (plusp (length value)) t))

(defun worker--signal-error (message &key operation pathname stage cause)
  "Signal a structured worker failure carrying MESSAGE and optional context."
  (error 'sbcl-worker-error
         :message   message
         :operation operation
         :pathname  pathname
         :stage     stage
         :cause     cause))

(defun worker-image--signal-error
    (message &key operation pathname stage cause)
  "Signal a structured worker-image failure carrying MESSAGE and context."
  (error 'sbcl-worker-image-error
         :message   message
         :operation operation
         :pathname  pathname
         :stage     stage
         :cause     cause))
