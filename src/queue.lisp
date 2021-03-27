(in-package #:posix-mqueue)

(defstruct (queue (:copier nil) (:conc-name nil))
  "Main type used to interact with POSIX message queues.  It contains a queue's
file descriptor (MQD) and a BUFFER used to receive messages.

It has a MQD slot: message queue's file descriptor.  And a BUFFER slot: buffer
used to receive messages form queue."
  (mqd (error "MQueue descriptor was not provided") :type (unsigned-byte 32))
  (buffer (make-array 0 :element-type '(unsigned-byte 8)) :type (array (unsigned-byte 8))))

(defstruct (attributes (:conc-name))
  "POSIX message queue attributes.

Slot NON-BLOCKING-P indicates whether the receive/send operations would block.
Slot MAX-MESSAGES shows queue's max number of messages.  MESSAGE-SIZE is queue's
message size.  CURRENT-MESSAGES shows how much messages there are on queue now."
  (non-blocking-p nil :type boolean)
  (max-messages 0 :type (unsigned-byte 64))
  (message-size 0 :type (unsigned-byte 64))
  (current-messages 0 :type (unsigned-byte 64)))
