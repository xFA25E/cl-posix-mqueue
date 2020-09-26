(defpackage posix-mqueue.spec
  (:nicknames :mq.spec)
  (:use :cl)
  (:import-from #:cffi
                #:defcfun
                #:define-foreign-library
                #:foreign-pointer
                #:use-foreign-library)
  (:import-from #:local-time #:timestamp)
  (:import-from #:mq.attr #:attributes)
  (:import-from #:mq.queue #:queue)
  (:import-from #:mq.spec.types
                #:mode
                #:mq-attr-get-t
                #:mq-attr-t
                #:mqd
                #:oflag
                #:result
                #:timespec-t))
(in-package :posix-mqueue.spec)

(define-foreign-library lib-rt
  (:unix (:or "librt.so" "librt.so.1"))
  (t (:default "librt.so")))

(use-foreign-library lib-rt)

(declaim (ftype (function (string mq.spec.types::oflags mq.spec.types::mode attributes)
                          (values (or keyword (unsigned-byte 32)) &optional))
                mq-open))
(defcfun "mq_open" result
  "Open POSIX message queue.  See mq_open(3) for more details."
  (name :string)
  (oflag oflag)
  (mode mode)
  (attr mq-attr-t))


(declaim (ftype (function (queue) (values (or keyword (integer 0 0)) &optional)) mq-close))
(defcfun "mq_close" result
  "Close POSIX message queue.  See mq_close(3) for more details."
  (mqdes mqd))

(declaim (ftype (function (queue attributes) (values (or keyword (integer 0 0)) &optional))
                mq-getattr))
(defcfun "mq_getattr" result
  "Get POSIX message queue attributes.  See mq_getattr(3) for more details."
  (mqdes mqd)
  (attr mq-attr-get-t))

(declaim (ftype (function (queue attributes foreign-pointer)
                          (values (or keyword (integer 0 0)) &optional))
                mq-setattr))
(defcfun "mq_setattr" result
  "Set POSIX message  queue non-blocking attribute.  See  mq_setattr(3) for more
details."
  (mqdes mqd)
  (newattr mq-attr-t)
  (oldattr :pointer))

(declaim (ftype (function (string) (values (or keyword (integer 0 0)) &optional)) mq-unlink))
(defcfun "mq_unlink" result
  "Unlink POSIX message queue.  See mq_unlink(3) for more details."
  (name :string))

(declaim (ftype (function (queue foreign-pointer (unsigned-byte 64) foreign-pointer)
                          (values (or keyword (unsigned-byte 64)) &optional))
                mq-receive))
(defcfun "mq_receive" result
  "Receive  a message  from POSIX  message  queue.  See  mq_receive(3) for  more
details."
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :ulong)
  (msg-prio (:pointer :uint)))

(declaim (ftype (function (queue foreign-pointer (unsigned-byte 64) foreign-pointer timestamp)
                          (values (or keyword (unsigned-byte 64)) &optional))
                mq-timedreceive))
(defcfun "mq_timedreceive" result
  "Receive a message from POSIX  message queue.  See mq_timedreceive(3) for more
details."
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :ulong)
  (msg-prio (:pointer :uint))
  (abs-timeout timespec-t))

(declaim (ftype (function (queue foreign-pointer (unsigned-byte 64) (unsigned-byte 32))
                          (values (or keyword (integer 0 0)) &optional))
                mq-send))
(defcfun "mq_send" result
  "Send a message to POSIX message queue.  See mq_send(3) for more details."
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :ulong)
  (msg-prio :uint))

(declaim (ftype (function (queue
                           foreign-pointer
                           (unsigned-byte 64)
                           (unsigned-byte 32)
                           timestamp)
                          (values (or keyword (integer 0 0)) &optional))
                mq-timedsend))
(defcfun "mq_timedsend" result
  "Send a message to POSIX message queue.  See mq_timedsend(3) for more details."
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :ulong)
  (msg-prio :uint)
  (abs-timeout timespec-t))
