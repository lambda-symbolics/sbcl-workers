(in-package #:sbcl-workers)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(sbcl-worker-host-request sbcl-worker-host-call sbcl-worker-host-context
            sbcl-worker-host-error sbcl-worker-host-error-code
            sbcl-worker-host-error-identity sbcl-worker-host-error-remote-type)))

(define-condition sbcl-worker-host-error (sbcl-worker-error)
  ((code :initarg :code :reader sbcl-worker-host-error-code
         :documentation "Machine-readable callback failure classification.")
   (identity :initarg :identity :initform nil :reader sbcl-worker-host-error-identity
             :documentation "Exact session, evaluation and callback identity.")
   (remote-type :initarg :remote-type :initform nil
                :reader sbcl-worker-host-error-remote-type
                :documentation "The host condition class name, when available."))
  (:documentation "A bounded host-call protocol, cancellation or dispatch failure."))

(defun host--fail (code &optional identity remote-type)
  "Signal a typed failure without copying arbitrary condition reports."
  (error 'sbcl-worker-host-error :code code :identity identity
         :remote-type remote-type :operation :host-call :stage :host-callback
         :message (format nil "Worker host callback failed: ~A" code)))

(defclass host-bounded-output (trivial-gray-streams:fundamental-character-output-stream)
  ((stream :initform (make-string-output-stream) :reader host-output--stream)
   (limit :initarg :limit :reader host-output--limit)
   (count :initform 0 :accessor host-output--count))
  (:documentation "A readable printer sink which refuses excess allocation."))

(defmethod trivial-gray-streams:stream-write-char ((stream host-bounded-output) char)
  (when (> (incf (host-output--count stream)) (host-output--limit stream))
    (host--fail :bound))
  (write-char char (host-output--stream stream)))

(defun host--portable-p (value)
  "Accept acyclic portable trees bounded to 64 levels and 65536 nodes."
  (let ((seen (make-hash-table :test #'eq)) (nodes 0))
    (labels ((walk (object depth)
               (when (or (> (incf nodes) 65536) (> depth 64))
                 (return-from host--portable-p nil))
               (cond
                 ((or (null object) (eq object t) (keywordp object)
                      (stringp object) (rationalp object)) t)
                 ((typep object '(or single-float double-float))
                  (and (ignore-errors
                        (<= (- most-positive-double-float) object
                            most-positive-double-float)) t))
                 ((consp object)
                  (let ((marked nil) (tail object))
                    (unwind-protect
                         (progn
                           (loop while (consp tail)
                                 do (when (or (> (incf nodes) 65536)
                                              (gethash tail seen))
                                      (return-from host--portable-p nil))
                                    (setf (gethash tail seen) t)
                                    (push tail marked)
                                    (unless (walk (first tail) (1+ depth))
                                      (return-from host--portable-p nil))
                                    (setf tail (rest tail)))
                           (null tail))
                      (dolist (cell marked) (remhash cell seen)))))
                 ((typep object 'simple-vector)
                  (when (gethash object seen) (return-from host--portable-p nil))
                  (setf (gethash object seen) t)
                  (unwind-protect
                       (every (lambda (item) (walk item (1+ depth))) object)
                    (remhash object seen)))
                 (t nil))))
      (and (walk value 0) t))))

(defun host--encode (value limit)
  "Validate and print portable VALUE without exceeding LIMIT characters."
  (unless (host--portable-p value) (host--fail :non-portable))
  (let ((stream (make-instance 'host-bounded-output :limit limit))
        (*print-readably* nil) (*print-escape* t) (*print-array* t)
        (*print-pretty* nil) (*print-circle* nil)
        (*print-level* nil) (*print-length* nil) (*package* (find-package :cl))
        (*print-base* 10) (*print-radix* nil) (*print-case* :upcase)
        (*read-default-float-format* 'single-float) (*readtable* (copy-readtable nil)))
    (write value :stream stream)
    (get-output-stream-string (host-output--stream stream))))

(defclass host-bounded-input (trivial-gray-streams:fundamental-character-input-stream)
  ((stream :initarg :stream :reader host-input--stream)
   (limit :initarg :limit :reader host-input--limit)
   (poll :initarg :poll :initform nil :reader host-input--poll)
   (count :initform 0 :accessor host-input--count)
   (depth :initform 0 :accessor host-input--depth)
   (quoted :initform nil :accessor host-input--quoted)
   (escaped :initform nil :accessor host-input--escaped)
   (previous :initform nil :accessor host-input--previous))
  (:documentation "A bounded reader with optional supervised polling."))

(defmethod trivial-gray-streams:stream-read-char ((stream host-bounded-input))
  (let ((input (host-input--stream stream)) (poll (host-input--poll stream)))
    (when poll
      (loop until (listen input) do (funcall poll) (sleep 0.01))
      (funcall poll))
    (when (> (incf (host-input--count stream)) (host-input--limit stream))
      (host--fail :bound))
    (let ((char (read-char input nil :eof)))
      (setf (host-input--previous stream)
            (list (host-input--depth stream) (host-input--quoted stream)
                  (host-input--escaped stream)))
      (unless (eq char :eof)
        (cond
          ((host-input--escaped stream) (setf (host-input--escaped stream) nil))
          ((eql char #\\) (setf (host-input--escaped stream) t))
          ((host-input--quoted stream)
           (when (eql char (host-input--quoted stream))
             (setf (host-input--quoted stream) nil)))
          ((find char '(#\" #\|)) (setf (host-input--quoted stream) char))
          ((eql char #\() (when (> (incf (host-input--depth stream)) 64)
                            (host--fail :bound)))
          ((eql char #\)) (decf (host-input--depth stream)))))
      char)))

(defmethod trivial-gray-streams:stream-unread-char ((stream host-bounded-input) char)
  (decf (host-input--count stream))
  (setf (host-input--depth stream) (first (host-input--previous stream))
        (host-input--quoted stream) (second (host-input--previous stream))
        (host-input--escaped stream) (third (host-input--previous stream)))
  (unread-char char (host-input--stream stream))
  nil)

(defun host--read (stream limit &optional poll)
  "Read one bounded portable packet with reader evaluation disabled."
  (let ((*read-eval* nil) (*package* (find-package :cl)) (*read-base* 10)
        (*read-default-float-format* 'single-float) (*readtable* (copy-readtable nil)))
    ;; Only unadorned vectors are valid dispatch syntax. Reject reader labels,
    ;; sized arrays/vectors and all evaluation/implementation-specific syntax.
    (set-macro-character
     #\# (lambda (input char)
            (declare (ignore char))
            (unless (eql (read-char input) #\() (host--fail :non-portable))
            (coerce (read-delimited-list #\) input t) 'simple-vector)))
    (handler-case
        (let ((value (read (make-instance 'host-bounded-input
                                         :stream stream :limit limit :poll poll))))
          (unless (host--portable-p value) (host--fail :non-portable))
          value)
      (end-of-file () (host--fail :disconnected)))))

(defun host--write (value stream limit)
  "Write a fully validated bounded packet to the existing protocol stream."
  (write-string (host--encode value limit) stream)
  (terpri stream)
  (finish-output stream))

(defvar *host-runtime* nil "Dynamic worker-side callback envelope.")
(defvar *host-runtime-call-id* 0 "Monotonic callback sequence within one evaluation.")
(defvar *host-runtime-in-call* nil "True while awaiting a callback result.")
(defvar *host-session* nil "Dynamic host-side callback envelope.")
(defvar *host-active-workers* (make-hash-table :test #'eq)
  "Workers currently dispatching callbacks, protected by the registry lock.")
(defvar *host-registry-lock* (make-lock "Host callback registry")
  "Lock for callback reentrancy admission.")

(defun sbcl-worker-host-context ()
  "Return optional portable context inherited by the current worker request."
  (getf *host-runtime* :worker-context))

(defun host--identity (session parent-id call-id)
  "Construct the exact correlation tuple used in both directions."
  (list :session (copy-seq session) :request-id parent-id :call-id call-id))

(defun sbcl-worker-host-call (payload)
  "Invoke the injected host dispatcher once and return one portable value.
Sequential calls are supported. Nested calls and calls from spawned worker
threads are rejected. Host EOF raises a typed disconnected condition."
  (unless *host-runtime* (host--fail :unavailable))
  (when *host-runtime-in-call* (host--fail :reentrancy))
  (host--encode payload (getf *host-runtime* :request-limit))
  (let* ((*host-runtime-in-call* t)
         (identity (host--identity (getf *host-runtime* :session)
                                  (getf *host-runtime* :request-id)
                                  (incf *host-runtime-call-id*)))
         (input (getf *host-runtime* :input))
         (output (getf *host-runtime* :output))
         (request-limit (getf *host-runtime* :request-limit))
         (result-limit (getf *host-runtime* :result-limit)))
    (host--write (list :host-call :identity identity :payload payload)
                 output (+ request-limit 4096))
    (let ((reply (host--read input (+ result-limit 4096))))
      (unless (and (eq (first reply) :host-result)
                   (equal (getf (rest reply) :identity) identity))
        (host--fail :identity identity))
      (if (eq (getf (rest reply) :status) :ok)
          (let ((value (getf (rest reply) :value)))
            (host--encode value result-limit)
            value)
          (host--fail (getf (rest reply) :code) identity
                      (getf (rest reply) :remote-type))))))

(defmethod worker--invoke-request ((operation (eql :host-request)) arguments request-id)
  (let ((*host-runtime* (append (list :input *standard-input* :output *standard-output*
                                    :request-id request-id) arguments))
        (*host-runtime-call-id* 0) (*host-runtime-in-call* nil))
    (worker--dispatch (getf arguments :operation) (getf arguments :arguments))))

(defmethod worker--request-allowed :before ((worker sbcl-worker))
  (with-lock-held (*host-registry-lock*)
    (when (gethash worker *host-active-workers*) (host--fail :reentrancy))))

(defun host--poll (worker session)
  "Propagate cancellation and detect worker death during blocked host work."
  (when (and (getf session :cancel-p) (funcall (getf session :cancel-p)))
    (host--fail :cancelled))
  (unless (sbcl-worker-running-p worker) (host--fail :worker-dead)))

(defmethod worker--emit-response ((operation (eql :host-request)) arguments response)
  (handler-case
      (host--write response *standard-output* (+ 4096 (getf arguments :result-limit)))
    (sbcl-worker-host-error (condition)
      (host--write (list :response :id (getf (rest response) :id) :status :error
                        :condition-type "SBCL-WORKER-HOST-ERROR"
                        :message (sbcl-worker-error-message condition))
                   *standard-output* 4096))))

(defun host--dispatch (worker payload identity session)
  "Run one supervised dispatcher, stopping it if cancellation or worker death occurs."
  (let ((thread nil) (done nil) (result nil) (failure nil)
        (lock (make-lock "Host dispatch completion")) (cancelled nil))
    (with-lock-held (*host-registry-lock*)
      (setf (gethash worker *host-active-workers*) t))
    (unwind-protect
         (progn
           (sb-sys:without-interrupts
             (setf thread
                   (make-thread
                    (lambda ()
                      (let ((returned nil))
                        (unwind-protect
                             (progn
                               (handler-case
                                   (setf result
                                         (funcall (getf session :dispatcher) payload
                                                  :context (getf session :context)
                                                  :identity (host--identity
                                                             (getf identity :session)
                                                             (getf identity :request-id)
                                                             (getf identity :call-id))
                                                  :cancelled-p
                                                  (lambda () (with-lock-held (lock) cancelled))))
                                 (error (condition) (setf failure condition)))
                               (setf returned t))
                          (unless returned
                            (setf failure (make-condition 'sbcl-worker-host-error
                                                         :code :host-aborted
                                                         :message "Host dispatcher aborted.")))
                          (with-lock-held (lock) (setf done t)))))
                    :name "Supervised worker host callback")))
           (loop
             (host--poll worker session)
             (when (with-lock-held (lock) done) (return))
             (sleep 0.01))
           (when failure (error failure))
           result)
      (with-lock-held (lock) (setf cancelled t))
      (when thread
        (when (thread-alive-p thread) (bordeaux-threads:destroy-thread thread))
        (ignore-errors (bordeaux-threads:join-thread thread)))
      (with-lock-held (*host-registry-lock*) (remhash worker *host-active-workers*)))))

(defmethod worker--read-response :around ((worker sbcl-worker) request-id)
  (unless (and *host-session* (eq worker (getf *host-session* :worker)))
    (return-from worker--read-response (call-next-method)))
  (let ((session *host-session*) (call-id 0))
    (loop
      (let ((packet (host--read (worker--output worker)
                                (+ 4096 (max (getf session :request-limit)
                                             (getf session :result-limit)))
                                (lambda () (host--poll worker session)))))
        (when (eq (first packet) :response) (return packet))
        (let ((identity (host--identity (getf session :session) request-id (incf call-id)))
              (payload (getf (rest packet) :payload)))
          (unless (and (eq (first packet) :host-call)
                       (equal (getf (rest packet) :identity) identity))
            (host--fail :identity identity))
          (host--encode payload (getf session :request-limit))
          (let ((reply
                  (handler-case
                      (let ((value (host--dispatch worker payload identity session)))
                        (host--encode value (getf session :result-limit))
                        (list :host-result :identity identity :status :ok :value value))
                    (error (condition)
                      (when (and (typep condition 'sbcl-worker-host-error)
                                 (member (sbcl-worker-host-error-code condition)
                                         '(:cancelled :worker-dead :disconnected)))
                        (error condition))
                      (list :host-result :identity identity :status :error
                            :code (if (typep condition 'sbcl-worker-host-error)
                                      (sbcl-worker-host-error-code condition) :host-failure)
                            :remote-type (string (type-of condition)))))))
            (host--write reply (worker--input worker) (+ 4096 (getf session :result-limit)))))))))

(defun sbcl-worker-host-request
    (worker operation arguments &key dispatcher context worker-context cancel-p
                                  (request-limit 1048576) (result-limit 1048576))
  "Execute an ordinary operation with optional synchronous worker host calls.
DISPATCHER receives PAYLOAD and :CONTEXT, :IDENTITY, :CANCELLED-P keyword arguments.
CONTEXT is opaque host-local data; WORKER-CONTEXT is a portable inherited value.
Limits count printed characters, not bytes, and must be 256..16777216.
CANCEL-P is polled while awaiting worker or host work. Cancellation destroys
this worker process and interrupts the supervised dispatcher with unwinding.
Same-worker nested requests are rejected; other workers may be called.
Any nonlocal exit cleans up this evaluation and its process."
  (unless (and (functionp dispatcher)
               (or (null cancel-p) (functionp cancel-p))
               (typep request-limit '(integer 256 16777216))
               (typep result-limit '(integer 256 16777216)))
    (host--fail :configuration))
  (worker--request-allowed worker)
  (host--encode arguments request-limit)
  (host--encode worker-context request-limit)
  (with-recursive-lock-held ((worker--lock worker))
    ;; Bootstrap the optional runtime through ordinary evaluation, including pristine cores.
    (let* ((path (asdf:system-source-file :sbcl-workers/host-callbacks))
           (reply (sbcl-worker-request worker :eval
                    (list :forms (list (format nil "(asdf:load-asd ~S)" (namestring path))
                                       "(asdf:load-system :sbcl-workers/host-callbacks)")))))
      (unless (eq (getf (rest reply) :status) :ok) (host--fail :bootstrap)))
    (let* ((nonce (format nil "~36R-~A" (get-universal-time) (gensym "HOST-")))
           (*host-session* (list :worker worker :session nonce :dispatcher dispatcher
                                 :context context :cancel-p cancel-p
                                 :request-limit request-limit :result-limit result-limit))
           (complete nil))
      (unwind-protect
           (prog1
               (sbcl-worker-request worker :host-request
                 (list :session nonce :operation operation :arguments arguments
                       :worker-context worker-context :request-limit request-limit
                       :result-limit result-limit))
             (setf complete t))
        (unless complete (sbcl-worker-cancel-request worker))))))
