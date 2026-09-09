(in-package #:sbcl-workers)

;;;; -- Image Saving by Restart --

;;; Windows has no fork, so a worker cannot hand its heap to a child and keep
;;; running. Instead the worker answers the request, then saves and exits
;;; itself. Its response carries :RESTARTING-P so the pool waits for the
;;; exit, probes and publishes the core, and restarts the named worker from
;;; that core. The heap is exact, because it is the heap that was saved; the
;;; visible cost is one process restart.

(defun worker--save-image (pathname identifier)
  "Schedule saving this worker's heap to PATHNAME after the current response."
  (unless (worker--single-threaded-p)
    (worker--signal-error
     "An SBCL worker image requires exactly one live Lisp thread."
     :operation :save-image))
  (when (probe-file pathname)
    (worker--signal-error
     "The unpublished SBCL worker core already exists."
     :operation :save-image
     :pathname pathname))
  (setf *worker-pending-save*
        (lambda ()
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
              (sb-ext:exit :code 1 :abort t)))))
  (values (list (namestring pathname)) ""))
