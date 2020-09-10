(defpackage cl-posix-mqueue.spec
  (:nicknames :mq.spec)
  (:use :cl)
  (:import-from #:cffi
                #:defbitfield
                #:defcfun
                #:defcstruct
                #:defctype
                #:defcvar
                #:define-foreign-library
                #:define-foreign-type
                #:use-foreign-library)
  (:export #:*errno*
           #:mq-attr
           #:mq-attr-ptr-type
           #:mq-attr-ptr-type-get
           #:mq-attr-ptr-type-set
           #:mq-close
           #:mq-curmsgs
           #:mq-flags
           #:mq-getattr
           #:mq-maxmsg
           #:mq-msgsize
           #:mq-open
           #:mq-receive
           #:mq-send
           #:mq-setattr
           #:mq-timedreceive
           #:mq-timedsend
           #:mq-unlink
           #:mqd-t
           #:oflag
           #:mode
           #:strerror
           #:time-t
           #:timespec))
(in-package :cl-posix-mqueue.spec)

(define-foreign-library lib-rt
  (:unix (:or "librt.so" "librt.so.1"))
  (t (:default "librt.so")))

(use-foreign-library lib-rt)

(defctype mqd-t :int)
(defctype time-t :ulong)

(defbitfield oflag
  (:read-only     #o0)
  (:write-only    #o1)
  (:read-write    #o2)
  (:close-on-exec #o2000000)
  (:create        #o100)
  (:exclusive     #o200)
  (:non-block     #o4000))

(defbitfield mode
  (:user-write  #o200)
  (:user-read   #o400)
  (:group-write #o020)
  (:group-read  #o040)
  (:other-write #o002)
  (:other-read  #o004))

(defcstruct mq-attr
  (mq-flags oflag)
  (mq-maxmsg :long)
  (mq-msgsize :long)
  (mq-curmsgs :long))

(define-foreign-type mq-attr-ptr-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-attr-ptr))

(define-foreign-type mq-attr-ptr-type-set ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-attr-ptr-set))

(define-foreign-type mq-attr-ptr-type-get ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-attr-ptr-get))

(defcstruct timespec
  (tv-sec time-t)
  (tv-nsec :long))

(defcfun "strerror" :string
  (errno :int))

(defcfun "mq_open" mqd-t
  (name :string)
  (oflag oflag)
  (mode mode)
  (attr mq-attr-ptr))

(defcfun "mq_close" :int
  (mqdes mqd-t))

(defcfun "mq_getattr" :int
  (mqdes mqd-t)
  (attr mq-attr-ptr-get))

(defcfun "mq_setattr" :int
  (mqdes mqd-t)
  (newattr mq-attr-ptr-set)
  (oldattr mq-attr-ptr-get))

(defcfun "mq_receive" :int
  (mqdes mqd-t)
  (msg-ptr (:pointer :char))
  (msg-len :uint)
  (msg-prio (:pointer :uint)))

(defcfun "mq_timedreceive" :int
  (mqdes mqd-t)
  (msg-ptr (:pointer :char))
  (msg-len :uint)
  (msg-prio (:pointer :uint))
  (abs-timeout (:pointer (:struct timespec))))

(defcfun "mq_send" :int
  (mqdes mqd-t)
  (msg-ptr (:pointer :char))
  (msg-len :uint)
  (msg-prio :uint))

(defcfun "mq_timedsend" :int
  (mqdes mqd-t)
  (msg-ptr (:pointer :char))
  (msg-len :uint)
  (msg-prio :uint)
  (abs-timeout (:pointer (:struct timespec))))

(defcfun "mq_unlink" :int
  (name :string))

(defcvar "errno" :int)

(let ((name "/hola") (temp-mqd -1))
  (unwind-protect
       (progn
         (cffi:with-foreign-object (cattr '(:struct mq-attr))
           (setf temp-mqd
                 (mq-open name '(:read-write :create) '(:user-read :user-write) cattr))))
    (unless (minusp temp-mqd)
      (mq-close temp-mqd)
      (mq-unlink name))))
