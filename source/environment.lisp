(in-package #:sbcl-workers)

(defclass sbcl-worker-environment ()
  ((sbcl-command
    :initarg :sbcl-command
    :reader worker-environment--sbcl-command
    :type string
    :documentation "The SBCL executable used to boot saved cores.")
   (pristine-command
    :initarg :pristine-command
    :reader worker-environment--pristine-command
    :type (or cons function)
    :documentation "The argv list that starts a pristine protocol runtime, or a
nullary function returning one, called at every pristine start.")
   (working-directory
    :initarg :working-directory
    :reader sbcl-worker-environment-working-directory
    :type pathname
    :documentation "The process directory for workers in this environment.")
   (image-root
    :initarg :image-root
    :reader sbcl-worker-environment-image-root
    :type pathname
    :documentation "The private root containing immutable worker images.")
   (evaluation-package
    :initarg :evaluation-package
    :reader worker-environment--evaluation-package
    :type string
    :documentation "The package name used to read and evaluate client forms.")
   (protocol-tag
    :initarg :protocol-tag
    :reader worker-environment--protocol-tag
    :type keyword
    :documentation "The first element of the worker handshake.")
   (protocol-version
    :initarg :protocol-version
    :reader worker-environment--protocol-version
    :type (integer 1 *)
    :documentation "The exact line-protocol version expected from workers.")
   (source-root-environment-variable
    :initarg :source-root-environment-variable
    :reader worker-environment--source-root-environment-variable
    :type string
    :documentation "The environment variable naming matching SBCL source.")
   (source-revision-function
    :initarg :source-revision-function
    :reader worker-environment--source-revision-function
    :type (or null function)
    :documentation "A nullary function returning host source provenance.")
   (context
    :initarg :context
    :initform nil
    :reader sbcl-worker-environment-context
    :type t
    :documentation "Opaque host application context associated with the paths."))
  (:documentation "Host configuration needed to manage isolated SBCL workers."))

(defun worker--command-p (value)
  "Return true when VALUE is a nonempty argv list of nonempty strings."
  (and (consp value)
       (every #'worker--non-empty-string-p value)
       t))

(defun worker-environment--pristine-argv (environment)
  "Return a fresh pristine argv list for ENVIRONMENT, validating a computed one."
  (let ((command (worker-environment--pristine-command environment)))
    (if (functionp command)
        (let ((argv (funcall command)))
          (unless (worker--command-p argv)
            (worker--signal-error
             "The pristine worker command function returned an invalid argv list."
             :operation :start
             :stage :validation))
          (copy-list argv))
        (copy-list command))))

(defun sbcl-worker-environment-create
    (&key
       (sbcl-command "sbcl")
       pristine-command
       (working-directory (uiop:getcwd))
       image-root
       (evaluation-package "CL-USER")
       (protocol-tag :sbcl-worker)
       (protocol-version 2)
       (source-root-environment-variable "SBCL_SOURCE_ROOT")
       source-revision-function
       context)
  "Create and validate a reusable worker ENVIRONMENT."
  (unless (and (worker--non-empty-string-p sbcl-command)
               (or (functionp pristine-command)
                   (worker--command-p pristine-command))
               image-root
               (worker--non-empty-string-p evaluation-package)
               (keywordp protocol-tag)
               (typep protocol-version '(integer 1 *))
               (worker--non-empty-string-p
                source-root-environment-variable)
               (or (null source-revision-function)
                   (functionp source-revision-function)))
    (worker--signal-error
     "The SBCL worker environment is incomplete or invalid."
     :operation :configure
     :stage :validation))
  (make-instance
   'sbcl-worker-environment
   :sbcl-command sbcl-command
   :pristine-command (if (functionp pristine-command)
                         pristine-command
                         (copy-list pristine-command))
   :working-directory (uiop:ensure-directory-pathname working-directory)
   :image-root (uiop:ensure-directory-pathname image-root)
   :evaluation-package evaluation-package
   :protocol-tag protocol-tag
   :protocol-version protocol-version
   :source-root-environment-variable source-root-environment-variable
   :source-revision-function source-revision-function
   :context context))
