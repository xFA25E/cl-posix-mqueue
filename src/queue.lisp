(in-package #:posix-mqueue)

(defclass queue ()
  ((mqd
    :initarg :mqd
    :type (unsigned-byte 32)
    :reader mqd
    :documentation "Message queue's file descriptor.")
   (buffer
    :initarg :buffer
    :type (array (unsigned-byte 8))
    :reader buffer
    :documentation "Buffer used to receive messages form queue."))
  (:documentation
   "Main type used to interact with POSIX message queues.  It contains a queue's
file descriptor (MQD) and a BUFFER used to receive messages."))

(defclass attributes ()
  ((non-blocking-p
    :initarg :non-blocking-p
    :type boolean
    :reader non-blocking-p
    :documentation "Whether the receive/send operations would block")
   (max-messages
    :initarg :max-messages
    :type (unsigned-byte 64)
    :reader max-messages
    :documentation "Max possible number of messages.")
   (message-size
    :initarg :message-size
    :type (unsigned-byte 64)
    :reader message-size
    :documentation "Message size.")
   (current-messages
    :initarg :current-messages
    :type (unsigned-byte 64)
    :reader current-messages
    :documentation "Number of messages currently on queue."))
  (:documentation
   "POSIX message queue attributes."))
