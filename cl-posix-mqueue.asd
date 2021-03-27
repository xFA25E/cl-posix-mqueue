(asdf:defsystem "cl-posix-mqueue"
  :version "0.1.0"
  :author "Valeriy Litkovskyy"
  :license ""
  :depends-on ("cffi" "alexandria" "babel" "local-time")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "condition")
                 (:file "lib" :depends-on ("condition" "queue" "spec" "types"))
                 (:file "queue")
                 (:file "spec" :depends-on ("queue" "translation" "types"))
                 (:file "translation" :depends-on ("queue" "types"))
                 (:file "types"))))
  :description "POSIX message queue bindings for Common Lisp"
  :in-order-to ((test-op (test-op "cl-posix-mqueue-tests")))
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.org"))

  ;; Development Options
  :around-compile
  (lambda (next)
    (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))
    (funcall next))
  )
