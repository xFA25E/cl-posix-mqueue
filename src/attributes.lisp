(defpackage posix-mqueue.attributes
  (:nicknames :mq.attr)
  (:use :cl)
  (:import-from #:alexandria #:once-only)
  (:import-from #:cffi
                #:expand-to-foreign-dyn
                #:foreign-alloc
                #:foreign-free
                #:free-translated-object
                #:null-pointer
                #:null-pointer-p
                #:translate-to-foreign
                #:with-foreign-object
                #:with-foreign-slots)
  (:import-from #:mq.spec
                #:mq-attr
                #:mq-attr-get-type
                #:mq-attr-type
                #:mq-curmsgs
                #:mq-flags
                #:mq-maxmsg
                #:mq-msgsize)
  (:export
   #:current-messages
   #:make
   #:make-set-non-blocking
   #:max-messages
   #:message-size
   #:non-blocking-p))
(in-package :posix-mqueue.attributes)

(defclass attributes ()
  ((non-blocking-p
    :initarg :non-blocking-p
    :initform nil
    :reader non-blocking-p)
   (max-messages
    :initarg :max-messages
    :initform 0
    :reader max-messages)
   (message-size
    :initarg :message-size
    :initform 0
    :reader message-size)
   (current-messages
    :initarg :current-messages
    :initform 0
    :reader current-messages)))

(defun make (&key non-blocking-p max-messages message-size)
  (assert (or (and (null max-messages) (null message-size))
              (and (integerp max-messages) (integerp message-size)
                   (plusp max-messages) (plusp message-size))))
  (make-instance 'attributes
                 :non-blocking-p non-blocking-p
                 :max-messages max-messages
                 :message-size message-size))

(defun make-set-non-blocking (non-blocking-p)
  (make-instance 'attributes :non-blocking-p non-blocking-p))

(defmethod translate-to-foreign (attributes (type mq-attr-type))
  (with-slots (non-blocking-p max-messages message-size current-messages) attributes
    (if (and max-messages message-size)
        (let ((cattributes (foreign-alloc '(:struct mq-attr))))
          (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs)
                               cattributes (:struct mq-attr))
            (setf mq-flags (when non-blocking-p '(:non-blocking))
                  mq-maxmsg max-messages
                  mq-msgsize message-size
                  mq-curmsgs current-messages))
          cattributes)
        (null-pointer))))

(defmethod free-translated-object (pointer (type mq-attr-type) param)
  (declare (ignore param))
  (unless (null-pointer-p pointer)
    (foreign-free pointer)))

(defmethod expand-to-foreign-dyn (value var body (type mq-attr-type))
  `(with-slots (non-blocking-p max-messages message-size current-messages) ,value
     (if (and max-messages message-size)
         (with-foreign-object (,var '(:struct mq-attr))
           (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs)
                                ,var (:struct mq-attr))
             (setf mq-flags (when non-blocking-p '(:non-blocking))
                   mq-maxmsg max-messages
                   mq-msgsize message-size
                   mq-curmsgs current-messages))
           ,@body)
         (null-pointer))))

(defmethod translate-to-foreign (attributes (type mq-attr-get-type))
  (values (foreign-alloc '(:struct mq-attr)) attributes))

(defmethod free-translated-object (pointer (type mq-attr-get-type) attributes)
  (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs) pointer (:struct mq-attr))
    (with-slots (non-blocking-p max-messages message-size current-messages) attributes
      (setf non-blocking-p (when mq-flags t)
            max-messages mq-maxmsg
            message-size mq-msgsize
            current-messages mq-curmsgs)))
  (foreign-free pointer))

(defmethod expand-to-foreign-dyn (value var body (type mq-attr-get-type))
  `(with-foreign-object (,var '(:struct mq-attr))
     ,@body
     (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs) ,var (:struct mq-attr))
       (with-slots (non-blocking-p max-messages message-size current-messages) ,value
         (setf non-blocking-p (when mq-flags t)
               max-messages mq-maxmsg
               message-size mq-msgsize
               current-messages mq-curmsgs)))))
