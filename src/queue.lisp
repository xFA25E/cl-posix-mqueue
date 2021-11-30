(in-package #:posix-mqueue)

(defclass queue ()
  ((mqd :reader mqd
        :initarg :mqd
        :type (unsigned-byte 32)
        :documentation "Message queue's file descriptor.")
   (buffer :reader buffer
           :initarg :buffer
           :type (array (unsigned-byte 8))
           :documentation "Buffer used to receive messages form queue."))
  (:documentation "Main type used to interact with POSIX message queues.

It contains a queue's file descriptor (MQD) and a BUFFER used to receive
messages."))

(defclass attributes ()
  ((non-blocking-p :reader non-blocking-p
                   :initarg :non-blocking-p
                   :type boolean
                   :documentation "Whether the receive/send operations would block")
   (max-messages :reader max-messages
                 :initarg :max-messages
                 :type (unsigned-byte 64)
                 :documentation "Max possible number of messages.")
   (message-size :reader message-size
                 :initarg :message-size
                 :type (unsigned-byte 64)
                 :documentation "Message size.")
   (current-messages :reader current-messages
                     :initarg :current-messages
                     :type (unsigned-byte 64)
                     :documentation "Number of messages currently on queue."))
  (:documentation "POSIX message queue attributes."))
