(asdf:defsystem "cl-posix-mqueue"
  :version "0.1.2"
  :author "Valeriy Litkovskyy <vlr.ltkvsk@protonmail.com>"
  :license "GPL3"
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
  :long-description "Common Lisp bindings to POSIX message queues.

POSIX message queue is an IPC (Inter-Process Communication) method that is easy
to use and quick to setup.

This library uses https://common-lisp.net/project/local-time library for
timestamps.

Other dependencies are: alexandria, babel and cffi.  Cffi should be able to find
librt."

  ;; Development Options
  ;; :around-compile
  ;; (lambda (next)
  ;;   (proclaim '(optimize (compilation-speed 0) (debug 3) (safety 3) (space 0) (speed 0)))
  ;;   (funcall next))
  )
