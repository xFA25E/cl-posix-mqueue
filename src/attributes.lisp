(defpackage cl-posix-mqueue.attributes
  (:nicknames :mq.attr)
  (:use :cl)
  (:import-from #:alexandria #:xor #:make-keyword)
  (:import-from #:cffi
                #:null-pointer
                #:null-pointer-p
                #:foreign-alloc
                #:foreign-free
                #:with-foreign-slots
                #:foreign-bitfield-symbol-list
                #:foreign-bitfield-symbols
                #:foreign-slot-value
                #:translate-to-foreign
                #:free-translated-object)
  (:import-from #:mq.spec
                #:mq-flags
                #:mq-maxmsg
                #:mq-msgsize
                #:mq-curmsgs
                #:mq-attr
                #:mq-attr-ptr-type
                #:mq-attr-ptr-type-get
                #:mq-attr-ptr-type-set
                #:oflag)
  (:export #:attributes #:make))
(in-package :cl-posix-mqueue.attributes)

(defclass attributes ()
  ((non-blocking-p
    :initarg :non-blocking-p
    :reader non-blocking-p)
   (max-messages
    :initarg :max-messages
    :reader max-messages)
   (message-size
    :initarg :message-size
    :reader message-size)
   (current-messages
    :initarg :current-messages
    :initform 0
    :reader current-messages)))

(defun make (&key flags max-messages message-size (current-messages 0))
  (assert (and (listp flags)
               (let ((flags (mapcar #'make-keyword flags)))
                 (equal
                  (intersection (cons :read-only (foreign-bitfield-symbol-list 'oflag)) flags)
                  flags))))
  (assert (or (and (null max-messages) (null message-size))
              (and (integerp max-messages) (integerp message-size)
                   (plusp max-messages) (plusp message-size))))
  (assert (integerp current-messages))
  (make-instance 'attributes
                 :flags flags
                 :max-messages max-messages
                 :message-size message-size
                 :current-messages current-messages))

;; (defmethod translate-to-foreign ((attributes attributes) (type mq-attr-ptr-type))
;;   (with-slots (flags max-messages message-size current-messages) attributes
;;     (if (and (null max-messages) (null message-size))
;;         (null-pointer)
;;         (let ((cattributes (foreign-alloc '(:struct mq-attr))))
;;           (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs)
;;                                cattributes (:struct mq-attr))
;;             (setf mq-flags flags
;;                   mq-maxmsg max-messages
;;                   mq-msgsize message-size
;;                   mq-curmsgs current-messages)
;;             cattributes)))))

;; (defmethod free-translated-object (pointer (type mq-attr-ptr-type) param)
;;   (declare (ignore param))
;;   (unless (null-pointer-p pointer)
;;     (foreign-free pointer)))

;; (defmethod translate-to-foreign ((attributes attributes) (type mq-attr-ptr-type-get))
;;   (values (foreign-alloc '(:struct mq-attr)) attributes))

;; (defmethod free-translated-object (pointer (type mq-attr-ptr-type-get) param)
;;   (unless (null-pointer-p pointer)
;;     (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs) pointer (:struct mq-attr))
;;       (with-slots (flags max-messages message-size current-messages) param
;;         (setf flags mq-flags
;;               max-messages mq-maxmsg
;;               message-size mq-msgsize
;;               current-messages mq-curmsgs)))
;;     (foreign-free pointer)))

;; (defmethod translate-to-foreign ((attributes attributes) (type mq-attr-ptr-type-set))
;;   (let ((cattributes (foreign-alloc '(:struct mq-attr))))
;;     (setf (foreign-slot-value cattributes '(:struct mq-attr) 'mq-flags) (flags attributes))
;;     cattributes))
