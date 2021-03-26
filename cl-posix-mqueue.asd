(asdf:defsystem "cl-posix-mqueue"
  :version "0.1.0"
  :author "Valeriy Litkovskyy"
  :license ""
  :depends-on ("cffi" "alexandria" "babel" "local-time")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "queue")
                 (:file "types")
                 (:file "translation")
                 (:file "spec")
                 (:file "condition")
                 (:file "lib"))))
  :description "POSIX message queue bindings for Common Lisp"
  :in-order-to ((test-op (test-op "cl-posix-mqueue-tests")))
  :around-compile
  (lambda (next)
    (proclaim '(optimize (compilation-speed 0) (debug 0) (safety 1) (space 3) (speed 3)))
    (funcall next))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.org")))
