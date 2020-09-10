(defpackage cl-posix-mqueue
  (:nicknames :mq)
  (:use :cl)
  (:import-from #:mq.spec #:mq-attr #:mq-open #:*errno* #:strerror)
  (:import-from #:cffi #:with-foreign-string #:with-foreign-object))
(in-package :cl-posix-mqueue)

(defclass posix-mqueue ()
  ((name
    :initarg :name
    :reader name)
   (file-descriptor
    :initarg :file-descriptor
    :reader file-descriptor)
   (attributes
    :initarg :attributes
    :reader attributes)
   (buffer
    :accessor buffer)
   (closed-p
    :accessor closed-p)
   (unlinked-p
    :accessor unlinked-p)))

(defun make (name &key
                    (flags '(:read-only))
                    (mode '(:user-read :user-write))
                    max-messages
                    message-size)
  (with-foreign-string (cname name)
    (let ((attributes (mq.attr:make :max-messages max-messages
                                    :message-size message-size)))
      (with-foreign-object (cattributes '(:struct mq-attr))
        ;; (populate-cstruct attributes cattributes)
        (let ((file-descriptor (mq-open cname flags mode cattributes)))
          (if (zerop file-descriptor)
              (progn
                (print *errno*)
                (print (strerror *errno*)))
              (make-instance 'posix-mqueue :name name :file-descriptor file-descriptor)))))))

;; (defvar *temp-mdq* nil)

;; (with-foreign-string (name "/hello")
;;   (print (setf *temp-mdq* (cl-posix-mqueue.spec:mq-open name (logior #o1 #o2) #o600 (cffi:null-pointer))))
;;   (print *errno*)
;;   (print (strerror *errno*)))

;; (let ((msg "wong"))
;;   (cffi:with-foreign-string (cmsg msg)
;;     (unless (zerop (print (cl-posix-mqueue.spec:mq-send *temp-mdq* cmsg (length msg) 0)))
;;       (print cl-posix-mqueue.spec:*errno*)
;;       (print (cl-posix-mqueue.spec:strerror cl-posix-mqueue.spec:*errno*)))))


;; (cffi:with-foreign-objects ((cmsg :char 8192) (msg-prio :int))
;;   (let ((len (cl-posix-mqueue.spec:mq-receive *temp-mdq* cmsg 8192 msg-prio)))
;;     (if (minusp (print len))
;;         (progn
;;           (print cl-posix-mqueue.spec:*errno*)
;;           (print (cl-posix-mqueue.spec:strerror cl-posix-mqueue.spec:*errno*)))
;;         (print (cffi:foreign-string-to-lisp cmsg :count len)))))

;; (print (cl-posix-mqueue.spec:mq-close *temp-mdq*))


;; (make "/hello" )
