(defpackage posix-mqueue.attributes
  (:nicknames :mq.attr)
  (:documentation "Package with attributes definition and creation functions.")
  (:use :cl)
  (:export #:current-messages #:max-messages #:message-size #:non-blocking-p))
(in-package :posix-mqueue.attributes)

(defclass attributes ()
  ((non-blocking-p
    :initarg :non-blocking-p
    :initform nil
    :reader non-blocking-p
    :type boolean
    :documentation "Whether the receive/send operations would block")
   (max-messages
    :initarg :max-messages
    :initform 0
    :reader max-messages
    :type (or null (unsigned-byte 64))
    :documentation "Queue's max number of messages.")
   (message-size
    :initarg :message-size
    :initform 0
    :reader message-size
    :type (or null (unsigned-byte 64))
    :documentation "Queue's Message size")
   (current-messages
    :initform 0
    :reader current-messages
    :type (unsigned-byte 64)
    :documentation "Current messages count on queue"))
  (:documentation "POSIX message queue attributes"))

(declaim (ftype (function (&key (:max-messages (or null (unsigned-byte 64)))
                                (:message-size (or null (unsigned-byte 64))))
                          (values attributes &optional))
                make))
(defun make (&key max-messages message-size)
  "Make  attributes, with  MAX-MESSAGES  and MESSAGE-SIZE  parameters.  This  is
during the creation of the queue."
  (assert (or (and (null max-messages) (null message-size))
              (and (integerp max-messages) (integerp message-size)
                   (plusp max-messages) (plusp message-size))))
  (make-instance 'attributes :max-messages max-messages :message-size message-size))

(declaim (ftype (function (boolean) (values attributes &optional)) make-set-non-blocking))
(defun make-set-non-blocking (non-blocking-p)
  "Make attributes with NON-BLOCKING-P parameter.  This used when changing queue
attributes dynamically.   This is the only  attribute that can be  changed after
the queue was created."
  (make-instance 'attributes :non-blocking-p non-blocking-p))
