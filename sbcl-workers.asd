(asdf:defsystem #:sbcl-workers
  :description "Persistent isolated SBCL workers and immutable heap images"
  :author "Lukas Hozda"
  :license "ISC"
  :version "0.1.0"
  :serial t
  :depends-on (#:bordeaux-threads
               #:ls-compat/posix
               #:sexp-store
               #+sbcl #:sb-posix)
  :components ((:module "source"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "environment")
                             (:file "images")
                             (:file "worker-source")
                             (:file "runtime")
                             (:file "runtime-fork" :if-feature (:not :win32))
                             (:file "runtime-restart" :if-feature :win32)
                             (:file "workers"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:sbcl-workers/tests))))

(asdf:defsystem #:sbcl-workers/tests
  :description "Tests for sbcl-workers"
  :depends-on (#:sbcl-workers)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:sbcl-workers/tests '#:run-tests)))


(asdf:defsystem #:sbcl-workers/read-eval-fixture
  :description "Fixture containing standard reader evaluation syntax"
  :components ((:file "tests/read-eval-fixture")))
