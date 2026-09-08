(in-package #:sbcl-workers)

(defparameter *worker-evaluation-package-name* "CL-USER"
  "The package used to read and evaluate client forms in this worker.")

(defparameter *worker-source-root-environment-variable* "SBCL_SOURCE_ROOT"
  "The environment variable naming source matching this SBCL runtime.")

(defun worker--evaluation-package ()
  "Return the configured evaluation package or signal a worker condition."
  (or (find-package *worker-evaluation-package-name*)
      (worker--signal-error
       (format nil "No Common Lisp package named ~S exists."
               *worker-evaluation-package-name*)
       :operation :runtime
       :stage :configuration)))

(defun worker--read-form (source &key (read-eval t))
  "Read exactly one Common Lisp form from SOURCE."
  (let ((*read-eval* read-eval)
        (*package* (worker--evaluation-package))
        (end-marker (cons nil nil)))
    (multiple-value-bind (form position)
        (read-from-string source t nil)
      (let ((remainder (read-from-string source nil end-marker :start position)))
        (unless (eq remainder end-marker)
          (worker--signal-error
           "Expected exactly one Common Lisp form."
           :operation :read)))
      form)))

(defun worker--bounded-string (value &key (limit 12000))
  "Return VALUE as a string bounded near LIMIT characters.

Both ends of a long value usually carry the signal: a backtrace leads
with the condition, evaluation output ends with the conclusion. The
bound keeps the head and the tail and names the dropped middle."
  (let ((string (if (stringp value) value (princ-to-string value))))
    (if (<= (length string) limit)
        string
        (let ((head (ceiling limit 2))
              (tail (floor limit 2)))
          (format nil "~A~%[... ~D characters dropped ...]~%~A"
                  (subseq string 0 head)
                  (- (length string) limit)
                  (subseq string (- (length string) tail)))))))

(defparameter +worker-source-kinds+
  '(:class :compiler-macro :condition :constant :function :generic-function
    :macro :method :method-combination :package :setf-expander :structure
    :symbol-macro :type :alien-type :variable :declaration :optimizer
    :source-transform :transform :vop :ir1-convert)
  "SB-INTROSPECT definition kinds accepted by the source operation.")

(defun worker-source--kind (name)
  "Return the supported definition kind named NAME, or NIL for every kind."
  (when (worker--non-empty-string-p name)
    (let ((kind (find (string-upcase name)
                      +worker-source-kinds+
                      :key #'symbol-name
                      :test #'string=)))
      (unless kind
        (worker--signal-error
         (format nil "Unknown SBCL definition kind ~S. Choose one of ~{~(~A~)~^, ~}."
                 name +worker-source-kinds+)
         :operation :source))
      kind)))

(defun worker-source--name (source)
  "Read and validate one definition name from SOURCE."
  (let ((name (worker--read-form source)))
    (unless (or (symbolp name)
                (stringp name)
                (and (consp name)
                     (eq (first name) 'setf)
                     (symbolp (second name))
                     (null (rest (rest name)))))
      (worker--signal-error
       "The source operation needs a symbol, package string, or (SETF symbol) name."
       :operation :source))
    name))

(defun worker-source--root ()
  "Return the source root matching this exact SBCL runtime."
  (let ((source-root
          (uiop:getenv *worker-source-root-environment-variable*)))
    (unless (and (worker--non-empty-string-p source-root)
                 (uiop:directory-exists-p source-root))
      (worker--signal-error
       (format nil "Matching SBCL source is unavailable; set ~A to its root."
               *worker-source-root-environment-variable*)
       :operation :source))
    (uiop:ensure-directory-pathname (truename source-root))))

(defun worker-source--relative-pathname (pathname)
  "Map a recorded SBCL source PATHNAME into its archive-relative path."
  (let* ((directories
           (mapcar #'string-downcase
                   (remove-if-not #'stringp (pathname-directory pathname))))
         (start
           (position-if
            (lambda (component)
              (member component '("src" "contrib" "tests" "tools")
                      :test #'string=))
            directories))
         (name (pathname-name pathname))
         (type (pathname-type pathname)))
    (unless (and start name)
      (worker--signal-error
       (format nil "Cannot map recorded SBCL source pathname ~S." pathname)
       :operation :source
       :pathname pathname))
    (pathname
     (format nil "~{~A/~}~A~@[.~A~]"
             (subseq directories start)
             (string-downcase name)
             (and type (string-downcase type))))))

(defun worker-source--pathname (recorded-pathname)
  "Resolve RECORDED-PATHNAME only within the configured source tree."
  (let* ((source-root (worker-source--root))
         (pathname
           (merge-pathnames
            (worker-source--relative-pathname recorded-pathname)
            source-root)))
    (unless (and (uiop:subpathp pathname source-root)
                 (probe-file pathname))
      (worker--signal-error
       (format nil "Recorded source ~S is absent from matching SBCL source."
               recorded-pathname)
       :operation :source
       :pathname pathname))
    (truename pathname)))

(defun worker-source--line-number (source offset)
  "Return the one-based line number containing OFFSET in SOURCE."
  (1+ (count #\Newline source :end (min offset (length source)))))

(defun worker-source--line-window (source offset)
  "Return a numbered source window surrounding OFFSET."
  (let* ((target-line (worker-source--line-number source offset))
         (first-line (max 1 (- target-line 12)))
         (last-line (+ target-line 28)))
    (with-output-to-string (output)
      (with-input-from-string (input source)
        (loop for line = (read-line input nil nil)
              for line-number from 1
              while line
              when (<= first-line line-number last-line)
                do (format output "~5D  ~A~%" line-number line)
              when (> line-number last-line)
                do (return))))))

(defun worker-source--complete-form (source offset)
  "Return the complete readable top-level form at OFFSET when possible."
  (handler-case
      (let ((*package* (find-package '#:cl-user))
            (*read-eval* nil))
        (multiple-value-bind (form end)
            (read-from-string source t nil :start offset)
          (declare (ignore form))
          (subseq source offset end)))
    (error ()
      nil)))

(defun worker-source--fallback-offset (name source)
  "Return a useful textual location for NAME when debug data lacks an offset."
  (let* ((symbol
           (if (and (consp name) (eq (first name) 'setf))
               (second name)
               name))
         (needle
           (etypecase symbol
             (symbol (symbol-name symbol))
             (string symbol))))
    (or (search needle source :test #'char-equal) 0)))

(defun worker-source--render-location (source-location kind name)
  "Render one SB-INTROSPECT SOURCE-LOCATION from matching source."
  (let* ((recorded
           (uiop:symbol-call '#:sb-introspect
                             '#:definition-source-pathname
                             source-location))
         (pathname (and recorded (worker-source--pathname recorded)))
         (source (and pathname (uiop:read-file-string pathname)))
         (recorded-offset
           (uiop:symbol-call '#:sb-introspect
                             '#:definition-source-character-offset
                             source-location))
         (offset (and source
                      (or recorded-offset
                          (worker-source--fallback-offset name source)))))
    (unless (and pathname source offset)
      (worker--signal-error
       "SBCL recorded no readable file location for this definition."
       :operation :source))
    (let ((complete-form
            (and recorded-offset
                 (worker-source--complete-form source offset))))
      (with-output-to-string (output)
        (format output "Kind: ~(~A~)~%Path: ~A~%Line: ~D~%"
                kind
                (enough-namestring pathname (worker-source--root))
                (worker-source--line-number source offset))
        (if complete-form
            (write-string (worker--bounded-string complete-form :limit 5000)
                          output)
            (write-string (worker-source--line-window source offset) output))))))

(defun sbcl-worker-source (name-source kind-source)
  "Return matching source locations for NAME-SOURCE and optional KIND-SOURCE."
  (require :sb-introspect)
  (let* ((name (worker-source--name name-source))
         (selected-kind (worker-source--kind kind-source))
         (kinds (if selected-kind
                    (list selected-kind)
                    +worker-source-kinds+))
         (locations nil)
         (seen (make-hash-table :test #'equal)))
    (dolist (kind kinds)
      (dolist (source-location
               (uiop:symbol-call '#:sb-introspect
                                 '#:find-definition-sources-by-name
                                 name
                                 kind))
        (let ((key
                (list kind
                      (uiop:symbol-call '#:sb-introspect
                                        '#:definition-source-pathname
                                        source-location)
                      (uiop:symbol-call '#:sb-introspect
                                        '#:definition-source-form-path
                                        source-location))))
          (unless (gethash key seen)
            (setf (gethash key seen) t)
            (push (cons kind source-location) locations)))))
    (unless locations
      (worker--signal-error
       (format nil "No ~:[SBCL ~;~(~A~) ~]definition source was found for ~S."
               selected-kind selected-kind name)
       :operation :source))
    (values
     nil
     (worker--bounded-string
      (with-output-to-string (output)
        (loop for (kind . source-location) in (nreverse locations)
              for index from 1 to 8
              do (when (> index 1)
                   (format output "~%~%"))
                 (write-string
                  (worker-source--render-location source-location kind name)
                  output)
              finally
                 (when (> (length locations) 8)
                   (format output "~%~%~D additional source locations omitted."
                           (- (length locations) 8)))))
      :limit 12000))))
