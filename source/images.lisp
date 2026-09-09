(in-package #:sbcl-workers)

(defparameter +pristine-sbcl-worker-image-identifier+ "pristine"
  "The reserved virtual base used for fresh SBCL workers.")

(defconstant +sbcl-worker-image-manifest-version+ 1
  "The current immutable worker-image manifest version.")

(defconstant +minimum-sbcl-worker-core-size+ 1048576
  "The smallest plausible saved SBCL core in bytes.")

(defclass sbcl-worker-image ()
  ((identifier
    :initarg :identifier
    :reader sbcl-worker-image-identifier
    :type string
    :documentation "The immutable worker-image identifier.")
   (directory
    :initarg :directory
    :reader sbcl-worker-image-directory
    :type pathname
    :documentation "The directory containing this image's artifacts.")
   (core-pathname
    :initarg :core-pathname
    :reader sbcl-worker-image-core-pathname
    :type pathname
    :documentation "The saved SBCL core pathname.")
   (manifest-pathname
    :initarg :manifest-pathname
    :reader sbcl-worker-image-manifest-pathname
    :type pathname
    :documentation "The portable immutable manifest pathname.")
   (parent-identifier
    :initarg :parent-identifier
    :reader sbcl-worker-image-parent-identifier
    :type string
    :documentation "The pristine or saved image from which this image descended.")
   (note
    :initarg :note
    :reader sbcl-worker-image-note
    :type string
    :documentation "The durable explanation of modifications and intended use.")
   (sbcl-version
    :initarg :sbcl-version
    :reader sbcl-worker-image-sbcl-version
    :type string
    :documentation "The exact SBCL version that saved the heap.")
   (operating-system
    :initarg :operating-system
    :reader sbcl-worker-image-operating-system
    :type string
    :documentation "The operating system that saved the heap.")
   (operating-system-version
    :initarg :operating-system-version
    :reader sbcl-worker-image-operating-system-version
    :type string
    :documentation "The operating-system build that saved the heap.")
   (architecture
    :initarg :architecture
    :reader sbcl-worker-image-architecture
    :type string
    :documentation "The machine architecture that saved the heap.")
   (source-revision
    :initarg :source-revision
    :reader sbcl-worker-image-source-revision
    :type (or null string)
    :documentation "Optional host source provenance recorded at publication.")
   (created-at
    :initarg :created-at
    :reader sbcl-worker-image-created-at
    :type integer
    :documentation "The image creation time as Common Lisp universal time."))
  (:documentation "An immutable named SBCL worker heap with durable provenance."))

(defun sbcl-worker-image-identifier-p (value)
  "Return true when VALUE is a safe saved-image path component."
  (and (worker--non-empty-string-p value)
       (<= (length value) 80)
       (not (string= value +pristine-sbcl-worker-image-identifier+))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_")))
              value)
       t))

(defun sbcl-worker-image-validate-identifier (identifier)
  "Return valid saved-image IDENTIFIER or signal an image condition."
  (unless (sbcl-worker-image-identifier-p identifier)
    (worker-image--signal-error
     (format nil
             "Invalid SBCL worker image name ~S. Use 1 to 80 letters, digits, hyphens, or underscores; pristine is reserved."
             identifier)
     :operation :images
     :stage :name))
  identifier)

(defun worker-image--directory (environment identifier)
  "Return IDENTIFIER's private directory within ENVIRONMENT."
  (merge-pathnames
   (format nil "~A/" (sbcl-worker-image-validate-identifier identifier))
   (sbcl-worker-environment-image-root environment)))

(defun sbcl-worker-image-plausible-core-p (pathname)
  "Return true when PATHNAME names a plausibly sized regular SBCL core."
  (and (probe-file pathname)
       (handler-case
           (with-open-file (stream pathname
                                   :direction :input
                                   :element-type '(unsigned-byte 8))
             (> (file-length stream) +minimum-sbcl-worker-core-size+))
         (error ()
           nil))
       t))

(defun worker-image--valid-parent-p (identifier)
  "Return true when IDENTIFIER can name an image parent."
  (and (stringp identifier)
       (or (string= identifier +pristine-sbcl-worker-image-identifier+)
           (sbcl-worker-image-identifier-p identifier))
       t))

(defun worker-image--valid-note-p (note)
  "Return true when NOTE is suitable for durable image provenance."
  (and (worker--non-empty-string-p note) (<= (length note) 4000) t))

(defun worker-image--manifest-form
    (&key identifier parent-identifier note core-pathname source-revision
          created-at)
  "Return the complete portable manifest for one saved worker image."
  (list :lisp-image
        :version +sbcl-worker-image-manifest-version+
        :id identifier
        :parent parent-identifier
        :note note
        :core (namestring core-pathname)
        :sbcl-version (lisp-implementation-version)
        :operating-system (software-type)
        :operating-system-version (software-version)
        :architecture (machine-type)
        :source-commit source-revision
        :created-at created-at))

(defun worker-image--write-manifest (pathname form)
  "Atomically publish immutable worker-image manifest FORM at PATHNAME."
  (snapshot-write pathname form :mode #o444))

(defun worker-image--validate-publication (parent-identifier note pathname)
  "Validate common image publication metadata or signal at PATHNAME."
  (unless (and (worker-image--valid-parent-p parent-identifier)
               (worker-image--valid-note-p note))
    (worker-image--signal-error
     "An SBCL worker image needs a valid parent and a non-empty note of at most 4000 characters."
     :operation :save-image
     :pathname pathname
     :stage :manifest)))

(defun sbcl-worker-image-publish-manifest
    (environment
     &key identifier parent-identifier note core-pathname source-revision)
  "Validate CORE-PATHNAME and publish IDENTIFIER's immutable manifest."
  (setf identifier (sbcl-worker-image-validate-identifier identifier))
  (worker-image--validate-publication parent-identifier note core-pathname)
  (let* ((directory (worker-image--directory environment identifier))
         (expected-core (merge-pathnames "worker.core" directory))
         (manifest (merge-pathnames "manifest.sexp" directory)))
    (unless (and (equal (pathname core-pathname) expected-core)
                 (sbcl-worker-image-plausible-core-p expected-core))
      (worker-image--signal-error
       "The saved SBCL worker core is absent, misplaced, or implausibly small."
       :operation :save-image
       :pathname core-pathname
       :stage :core))
    (when (probe-file manifest)
      (worker-image--signal-error
       (format nil "SBCL worker image ~A already exists." identifier)
       :operation :save-image
       :pathname manifest
       :stage :publish))
    (setf (ls-compat.posix:file-mode expected-core) #o444)
    (worker-image--write-manifest
     manifest
     (worker-image--manifest-form
      :identifier identifier
      :parent-identifier parent-identifier
      :note note
      :core-pathname expected-core
      :source-revision source-revision
      :created-at (get-universal-time)))
    (sbcl-worker-image-load environment identifier)))

(defvar *worker-image-staging-counter* 0
  "A process-local counter used to make unpublished image directories unique.")

(defun sbcl-worker-image-staging-directory (environment identifier)
  "Return a fresh unpublished directory for saved image IDENTIFIER."
  (sbcl-worker-image-validate-identifier identifier)
  (loop
    for counter = (incf *worker-image-staging-counter*)
    for token = (format nil "~36R-~36R-~36R"
                        (get-universal-time)
                        (sb-posix:getpid)
                        counter)
    for pathname = (merge-pathnames
                    (format nil ".~A.~A/" identifier token)
                    (sbcl-worker-environment-image-root environment))
    unless (probe-file pathname)
      return pathname))

(defun worker-environment--source-revision (environment)
  "Return ENVIRONMENT's current optional host source revision."
  (let ((function
          (worker-environment--source-revision-function environment)))
    (when function
      (handler-case
          (let ((revision (funcall function)))
            (and (worker--non-empty-string-p revision) revision))
        (error ()
          nil)))))

(defun worker-image--publish-saved-core
    (environment
     &key identifier parent-identifier note staging-directory)
  "Atomically publish a validated core from STAGING-DIRECTORY."
  (setf identifier (sbcl-worker-image-validate-identifier identifier))
  (let* ((root (sbcl-worker-environment-image-root environment))
         (directory (worker-image--directory environment identifier))
         (staging-directory (uiop:ensure-directory-pathname staging-directory))
         (staging-core (merge-pathnames "worker.core" staging-directory))
         (staging-manifest (merge-pathnames "manifest.sexp" staging-directory))
         (published-core (merge-pathnames "worker.core" directory)))
    (unless (and (uiop:subpathp staging-directory root)
                 (not (equal staging-directory directory))
                 (sbcl-worker-image-plausible-core-p staging-core))
      (worker-image--signal-error
       "The unpublished SBCL worker core is absent or outside its staging root."
       :operation :save-image
       :pathname staging-core
       :stage :core))
    (when (probe-file directory)
      (worker-image--signal-error
       (format nil "SBCL worker image ~A already exists." identifier)
       :operation :save-image
       :pathname directory
       :stage :publish))
    (worker-image--validate-publication
     parent-identifier note staging-manifest)
    (setf (ls-compat.posix:file-mode staging-core) #o444)
    (worker-image--write-manifest
     staging-manifest
     (worker-image--manifest-form
      :identifier identifier
      :parent-identifier parent-identifier
      :note note
      :core-pathname published-core
      :source-revision (worker-environment--source-revision environment)
      :created-at (get-universal-time)))
    (handler-case
        (rename-file staging-directory directory)
      (error (condition)
        (worker-image--signal-error
         (format nil "Could not publish SBCL worker image ~A: ~A"
                 identifier condition)
         :operation :save-image
         :pathname directory
         :stage :publish
         :cause condition)))
    (sbcl-worker-image-load environment identifier)))

(defun worker-image--valid-manifest-p
    (form properties identifier directory core sole-form-p)
  "Return true when parsed image manifest data satisfies every invariant."
  (and sole-form-p
       (listp form)
       (eq (first form) :lisp-image)
       (= (or (getf properties :version) 0)
          +sbcl-worker-image-manifest-version+)
       (string= (or (getf properties :id) "") identifier)
       (worker-image--valid-parent-p (getf properties :parent))
       (worker-image--valid-note-p (getf properties :note))
       core
       (uiop:subpathp core directory)
       (sbcl-worker-image-plausible-core-p core)
       (worker--non-empty-string-p (getf properties :sbcl-version))
       (worker--non-empty-string-p (getf properties :operating-system))
       (worker--non-empty-string-p
        (getf properties :operating-system-version))
       (worker--non-empty-string-p (getf properties :architecture))
       (or (null (getf properties :source-commit))
           (worker--non-empty-string-p (getf properties :source-commit)))
       (integerp (getf properties :created-at))
       t))

(defun sbcl-worker-image-load (environment identifier)
  "Load and fully validate saved SBCL worker image IDENTIFIER."
  (let* ((directory (worker-image--directory environment identifier))
         (manifest (merge-pathnames "manifest.sexp" directory)))
    (unless (probe-file manifest)
      (worker-image--signal-error
       (format nil "SBCL worker image ~A has no manifest." identifier)
       :operation :images
       :pathname manifest
       :stage :manifest))
    (handler-case
        (multiple-value-bind (form sole-form-p)
            (snapshot-read manifest)
          (let* ((properties (and (listp form) (rest form)))
                 (core-value (and properties (getf properties :core)))
                 (core (and (worker--non-empty-string-p core-value)
                            (pathname core-value))))
            (unless (worker-image--valid-manifest-p
                     form properties identifier directory core sole-form-p)
              (worker-image--signal-error
               (format nil "Invalid SBCL worker image manifest at ~A." manifest)
               :operation :images
               :pathname manifest
               :stage :manifest))
            (make-instance
             'sbcl-worker-image
             :identifier identifier
             :directory directory
             :core-pathname core
             :manifest-pathname manifest
             :parent-identifier (getf properties :parent)
             :note (getf properties :note)
             :sbcl-version (getf properties :sbcl-version)
             :operating-system (getf properties :operating-system)
             :operating-system-version
             (getf properties :operating-system-version)
             :architecture (getf properties :architecture)
             :source-revision (getf properties :source-commit)
             :created-at (getf properties :created-at))))
      (sbcl-worker-image-error (condition)
        (error condition))
      (error (condition)
        (worker-image--signal-error
         (format nil "Could not read SBCL worker image manifest at ~A: ~A"
                 manifest condition)
         :operation :images
         :pathname manifest
         :stage :manifest
         :cause condition)))))

(defun sbcl-worker-image-compatible-p (image)
  "Return true when IMAGE can boot under this exact SBCL host."
  (and (string= (sbcl-worker-image-sbcl-version image)
                (lisp-implementation-version))
       (string= (sbcl-worker-image-operating-system image) (software-type))
       (string= (sbcl-worker-image-operating-system-version image)
                (software-version))
       (string= (sbcl-worker-image-architecture image) (machine-type))
       t))

(defun sbcl-worker-image-scan (environment)
  "Return valid saved images and (PATHNAME . REPORT) failures."
  (let ((root (sbcl-worker-environment-image-root environment))
        (images nil)
        (failures nil))
    (when (uiop:directory-exists-p root)
      (dolist (directory (sort (uiop:subdirectories root)
                               #'string<
                               :key #'namestring))
        (let ((identifier
                (first (last (pathname-directory
                              (uiop:ensure-directory-pathname directory))))))
          (handler-case
              (push (sbcl-worker-image-load environment (string identifier))
                    images)
            (error (condition)
              (push (cons directory (princ-to-string condition)) failures))))))
    (values (sort images #'string< :key #'sbcl-worker-image-identifier)
            (nreverse failures))))
