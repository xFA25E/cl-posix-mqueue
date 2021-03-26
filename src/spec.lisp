(in-package :posix-mqueue)

(define-foreign-library lib-rt
  (:unix (:or "librt.so" "librt.so.1"))
  (t (:default "librt.so")))

(use-foreign-library lib-rt)

(defcfun "mq_open" result
  "Open POSIX message queue.  See mq_open(3) for more details."
  (name :string)
  (oflag oflag)
  (mode mode)
  (attr mq-size-attr-t))

(defcfun "mq_close" result
  "Close POSIX message queue.  See mq_close(3) for more details."
  (mqdes mqd))

(defcfun "mq_getattr" result
  "Get POSIX message queue attributes.  See mq_getattr(3) for more details."
  (mqdes mqd)
  (attr mq-get-attr-t))

(defcfun "mq_setattr" result
  "Set POSIX message  queue non-blocking attribute.  See  mq_setattr(3) for more
details."
  (mqdes mqd)
  (newattr mq-non-blocking-attr-t)
  (oldattr :pointer))

(defcfun "mq_unlink" result
  "Unlink POSIX message queue.  See mq_unlink(3) for more details."
  (name :string))

(defcfun "mq_receive" result
  "Receive  a message  from POSIX  message  queue.  See  mq_receive(3) for  more
details."
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :ulong)
  (msg-prio (:pointer :uint)))

(defcfun "mq_timedreceive" result
  "Receive a message from POSIX  message queue.  See mq_timedreceive(3) for more
details."
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :ulong)
  (msg-prio (:pointer :uint))
  (abs-timeout timespec-t))

(defcfun "mq_send" result
  "Send a message to POSIX message queue.  See mq_send(3) for more details."
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :ulong)
  (msg-prio :uint))

(defcfun "mq_timedsend" result
  "Send a message to POSIX message queue.  See mq_timedsend(3) for more details."
  (mqdes mqd)
  (msg-ptr (:pointer :char))
  (msg-len :ulong)
  (msg-prio :uint)
  (abs-timeout timespec-t))
