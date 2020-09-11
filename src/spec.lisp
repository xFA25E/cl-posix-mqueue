(defpackage posix-mqueue.spec
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
  (:export
   #:*errno*
   #:mq-attr
   #:mq-attr-type
   #:mq-attr-get-type
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
   #:mqd
   #:oflag
   #:mode
   #:strerror
   #:time-t
   #:timespec
   #:tv-sec
   #:tv-nsec))
(in-package :posix-mqueue.spec)

(define-foreign-library lib-rt
  (:unix (:or "librt.so" "librt.so.1"))
  (t (:default "librt.so")))

(use-foreign-library lib-rt)

(defctype time-t :ulong)

(defbitfield oflag
  (:read-only     #o0)
  (:write-only    #o1)
  (:read-write    #o2)
  (:close-on-exec #o2000000)
  (:create        #o100)
  (:exclusive     #o200)
  (:non-blocking  #o4000))

(defbitfield mode
  (:user-write  #o200)
  (:user-read   #o400)
  (:group-write #o020)
  (:group-read  #o040)
  (:other-write #o002)
  (:other-read  #o004))

(defbitfield (mq-flags :long)
  (:non-blocking #o4000))

(defcstruct mq-attr
  (mq-flags mq-flags)
  (mq-maxmsg :long)
  (mq-msgsize :long)
  (mq-curmsgs :long))

(define-foreign-type mq-attr-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-attr-t))

(define-foreign-type mq-attr-get-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-attr-get-t))

(defcstruct timespec
  (tv-sec time-t)
  (tv-nsec :long))

(defcfun "strerror" :string
  (errno :int))

(defctype mqd :int)

(defcfun "mq_open" mqd
  (name :string)
  (oflag oflag)
  (mode mode)
  (attr mq-attr-t))

(defcfun "mq_close" :int
  (mqdes mqd))

(defcfun "mq_getattr" :int
  (mqdes mqd)
  (attr mq-attr-get-t))

(defcfun "mq_setattr" :int
  (mqdes mqd)
  (newattr mq-attr-t)
  (oldattr (:pointer (:struct mq-attr))))

(defcfun "mq_unlink" :int
  (name :string))

(defcfun "mq_receive" :int
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :uint)
  (msg-prio (:pointer :uint)))

(defcfun "mq_timedreceive" :int
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :uint)
  (msg-prio (:pointer :uint))
  (abs-timeout (:pointer (:struct timespec))))

(defcfun "mq_send" :int
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :uint)
  (msg-prio :uint))

(defcfun "mq_timedsend" :int
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :uint)
  (msg-prio :uint)
  (abs-timeout (:pointer (:struct timespec))))

(defcvar "errno" :int)
