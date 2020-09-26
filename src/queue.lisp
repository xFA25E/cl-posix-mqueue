(defpackage posix-mqueue.queue
  (:nicknames :mq.queue)
  (:documentation
   "Package that contains  just a definition of  QUEUE class.  It is  defined here because
you have to define a class before you can use it in translations.")
  (:use :cl))
(in-package #:posix-mqueue.queue)

(defclass queue ()
  ((mqd
    :initarg :mqd
    :accessor mqd
    :type (unsigned-byte 32)
    :documentation "Message queue's file descriptor.")
   (buffer
    :accessor buffer
    :type (array (unsigned-byte 8))
    :documentation "Buffer used to receive messages form queue."))
  (:documentation
   "Main class  used to interact  with POSIX message queues.   It contains a  queue's file
descriptor (MQD) and a BUFFER used to receive messages."))
