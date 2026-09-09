(in-package #:sbcl-workers)

;;;; -- Forked Image Saving --

;;; A worker that can fork saves its own heap from a child so the live REPL
;;; keeps running. Hosts without fork load runtime-restart.lisp instead.

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
