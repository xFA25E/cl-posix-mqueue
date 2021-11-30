(in-package #:posix-mqueue)

(defvar *retry-on-interrupt-p* t
  "Whether or not to retry send/receive operation on interrupt.")

(defun random-queue-name (&key (length 25) (start 97) (end 123))
  "Generate random queue name with specified LENGTH, with characters starting
from START to END.  With slash at the beginning."
  (check-type length (integer 1))
  (let ((result (make-string (+ 1 length))))
    (setf (aref result 0) #\/)
    (loop :for i :from 1 :to length
          :for generated-char = (code-char (+ start (random (- end start))))
          :do (setf (aref result i) generated-char))
    result))

(defun default-sizes ()
  "Return default sizes of a queue in a form (MAX-MESSAGES . MESSAGE-SIZE).
This is done by creating a queue with a random name and by extracting its
attributes.  By using a 255 length name, we protect ourselves from name
collision."
  (handler-case
      (let* ((name (random-queue-name :length 255))
             (mqd (mq-open name '(:read-only :create :exclusive)
                           '(:user-read :user-write) nil)))
        (unwind-protect
             (progn
               (check-type mqd (signed-byte 32))
               (with-foreign-object (cattrs '(:struct mq-attr))
                 (mq-getattr mqd cattrs)
                 (with-foreign-slots ((mq-maxmsg mq-msgsize) cattrs (:struct mq-attr))
                   (cons mq-maxmsg mq-msgsize))))
          (ignore-errors
           (mq-unlink name)
           (mq-close mqd))))
    (file-exists (c)
      (declare (ignore c))
      (default-sizes))))

(defun open-queue (name &key (open-flags '(:read-only))
                          (create-modes '(:user-read :user-write))
                          max-messages message-size)
  "Create a new POSIX message queue or open an existing queue.

NAME is a string that identifies a queue.  It MUST start with a slash (\"/\")
and MUST NOT contain other slashes.  Example: \"/myqueue\".

OPEN-FLAGS is a list of flags that control the operation of queue. Exactly one
of the following must be specified in OPEN-FLAGS:

  :read-only

    Open the queue to receive messages only.

  :write-only

    Open the queue to send messages only.

  :read-write

    Open the queue to both send and receive messages.

Zero or more of the following flags:

  :close-on-exec

    Set the close-on-exec flag for the message queue descriptor.  See open(2)
    for a discussion of why this flag is useful.

  :create

    Create the message queue if it does not exist.  The owner (user ID) of the
    message queue is set to the effective user ID of the calling process.  The
    group ownership (group ID) is set to the effective group ID of the calling
    process.

  :exclusive

    If :create was specified in OPEN-FLAGS, and a queue with the given name
    already exists, then fail signaling FILE-EXISTS condition.

  :non-blocking

    Open the queue in nonblocking mode.  In circumstances where RECEIVE and SEND
    operations would normally block, these operations will return :try-again
    instead.

If :create is specified in OPEN-FLAGS, then three additional arguments can be
supplied.  The MODE argument specifies the permissions to be placed on the new
queue.  It is a list of the following possible flags:

  :user-read :user-write :group-read :group-write :other-read :other-read

In addition, MAX-MESSAGES and MESSAGE-SIZE specify the maximum number of
messages and the maximum size of messages that the queue will allow.  Usually,
they default to their maximum values, 10 and 8192 respectively, but these values
can be changes through /proc/sys/fs/mqueue/ interface.  They must be provided in
pair, as in the mq_open(3), but DEFAULT-SIZES function is provided to get
default sizes of a queue.

This function can signal the following conditions:

  ACCESS-DENIED-PERMISSION

    The queue exists, but the caller does not have permission to open it in the
    specified mode.

  ACCESS-DENIED-SLASHES

    NAME contained more than one slash.

  FILE-EXISTS

    Both :create and :exclusive were specified in OPEN-FLAGS, but a queue with
    this NAME already exists.

  INVALID-ARGUMENT-NAME

    NAME doesn't follow the format described in mq_overview(7).

  INVALID-ARGUMENT-SIZES

    :create was specified in OPEN-FLAGS, but MAX-MESSAGES or MESSAGE-SIZE were
    invalid. Both of these fields must be greater than zero.  In a process that
    is unprivileged (does not have the CAP_SYS_RESOURCE capability),
    MAX-MESSAGES must be less than or equal to the msg_max limit, and
    MESSAGE-SIZE must be less than or equal to the msgsize_max limit.  In
    addition, even in a privileged process, MAX-MESSAGES cannot exceed the
    HARD_MAX limit.  (See mq_overview(7) for details of these limits.).

    Both of these limits can be changed through the /proc/sys/fs/mqueue/
    interface.

  TOO-MANY-OPEN-FILES

    The per-process limit on the number of open file and message queue
    descriptors has been reached (see the description of RLIMIT_NOFILE in
    getrlimit(2)).

  NAME-TOO-LONG

    NAME was too long.

  FILE-TABLE-OVERFLOW

    The system-wide limit on the total number of open files and message queues
    has been reached.

  NO-FILE-OR-DIRECTORY-JUST-SLASH

    NAME was just \"/\" followed by no other characters.

  NO-FILE-OR-DIRECTORY-NO-CREATE

    The :create flag was not specified in OPEN-FLAGS, and no queue with this
    NAME exists.

  OUT-OF-MEMORY

    Insufficient memory.

  NO-SPACE-LEFT-ON-DEVICE

    Insufficient space for the creation of a new message queue.  This probably
    occurred because the queues_max limit was encountered; see mq_overview(7).

  SIMPLE-ERROR

    This one can be signalled if the OPEN-FLAGS or the MODE are invalid.

  BAD-FILE-DESCRIPTOR

    The message queue file descriptor (MQD) is invalid.  This is an internal
    error that should not happen, it is mainly for the writer of this library."

  (check-type open-flags open-flags)
  (check-type create-modes create-modes)
  (assert (or (and max-messages message-size) (not (or max-messages message-size))))
  (check-type max-messages (or null (integer 1 4294967295)))
  (check-type message-size (or null (integer 1 4294967295)))

  (let* ((sizes (when (and max-messages message-size)
                  (cons max-messages message-size)))
         (mqd (mq-open name open-flags create-modes sizes)))
    (case mqd
      (:access-denied
       (if (= 1 (count #\/ name))
           (error 'access-denied-permission)
           (error 'access-denied-slashes)))
      (:file-exists
       (error 'file-exists))
      (:invalid-argument
       (if (starts-with #\/ name)
           (error 'invalid-argument-sizes)
           (error 'invalid-argument-name)))
      (:too-many-open-files
       (error 'too-many-open-files))
      (:name-too-long
       (error 'name-too-long))
      (:file-table-overflow
       (error 'file-table-overflow))
      (:no-file-or-directory
       (if (string= name "/")
           (error 'no-file-or-directory-just-slash)
           (error 'no-file-or-directory-no-create)))
      (:out-of-memory
       (error 'out-of-memory))
      (:no-space-left-on-device
       (error 'no-space-left-on-device))
      (t
       (let ((buffer-size
               (with-foreign-object (cattrs '(:struct mq-attr))
                 (mq-getattr mqd cattrs)
                 (cffi:foreign-slot-value cattrs '(:struct mq-attr) 'mq-msgsize))))
         (make-instance
          'queue
          :mqd mqd
          :buffer (make-array buffer-size :element-type '(unsigned-byte 8))))))))

(defun unlink (name)
  "Remove the specified message queue NAME.  The message queue NAME is removed
immediately.  The queue itself is destroyed once any other processes that have
the queue open close their descriptors referring to the queue.

Conditions:

  ACCESS-DENIED-ON-UNLINK

    The caller does not have permission to unlink this message queue.

  NAME-TOO-LONG

    NAME was too long.

  NO-FILE-OR-DIRECTORY-ON-UNLINK

    There is no message queue with the given NAME."
  (ecase (mq-unlink name)
    (:access-denied (error 'access-denied-on-unlink))
    (:name-too-long (error 'name-too-long))
    (:no-file-or-directory (error 'no-file-or-directory-on-unlink))
    (0 nil)))

(defun close-queue (queue)
  "Close the message queue.

Conditions:

  BAD-FILE-DESCRIPTOR-INVALID

    The message queue file descriptor (MQD) is invalid."
  (ecase (mq-close queue)
    (:bad-file-descriptor (error 'bad-file-descriptor-invalid))
    (0 nil)))

(defun attributes (queue)
  "Retrieve attributes of the message queue.

Conditions:

  BAD-FILE-DESCRIPTOR-INVALID

    The message queue file descriptor (MQD) is invalid."
  (with-foreign-object (cattrs '(:struct mq-attr))
    (ecase (mq-getattr queue cattrs)
      (:bad-file-descriptor
       (error 'bad-file-descriptor-invalid))

      (0
       (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs) cattrs (:struct mq-attr))
         (make-instance
          'attributes
          :non-blocking-p (when mq-flags t)
          :max-messages mq-maxmsg
          :message-size mq-msgsize
          :current-messages mq-curmsgs))))))

(defun set-non-blocking (queue non-blocking-p)
  "Modify NON-BLOCKING-P attribute of the message queue.

Conditions:

  BAD-FILE-DESCRIPTOR-INVALID

    The message queue file descriptor (MQD) is invalid.

  INVALID-ARGUMENT-ATTRIBUTES

    mq-flags contained flags other than :non-blocking. This is an internal error
    that should not happen, it is mainly for the writer of this library."
  (ecase (mq-setattr queue non-blocking-p (cffi:null-pointer))
    (:bad-file-descriptor (error 'bad-file-descriptor-invalid))
    (:invalid-argument (error 'invalid-argument-attributes))
    (0 non-blocking-p)))

(defmacro %receive (receive-fn current-fn return-form &rest current-fn-args)
  "Macro used for generating various receive functions.

RECEIVE-FN is a function called to receive a message.  CURRENT-FN is a function
which will be called on interrupt.  RETURN-FORM is a form placed at the end of
the macro.  It has access to BUFFER, LENGTH (of received message) and
PRIORITY (of received message).  CURRENT-FN-ARGS are additional arguments placed
at the end of RECEIVE-FN and CURRENT-FN."
  `(let ((buffer (buffer queue)))
     (with-foreign-object (priority :uint)
       (let ((length (with-pointer-to-vector-data (ptr buffer)
                       (,receive-fn queue ptr (length buffer) priority ,@current-fn-args))))
         (case length
           (:connection-timed-out :connection-timed-out)
           (:try-again :try-again)
           (:invalid-argument (error 'invalid-argument-on-send-receive))
           (:message-too-long (error 'message-too-long-on-receive))
           (:bad-file-descriptor (error 'bad-file-descriptor-on-receive))
           (:interrupted-system-call
            (if *retry-on-interrupt-p*
                (,current-fn queue ,@current-fn-args)
                (restart-case (error 'interrupted-system-call)
                  (retry-on-interrupt ()
                    :report "Call to receive was interrupted.  Retry the call."
                    (,current-fn queue ,@current-fn-args)))))
           (t ,return-form))))))

(defun receive (queue)
  "Remove the oldest message with the highest priority from the message QUEUE
and return it as '(ARRAY (UNSIGNED-BYTE 8)).  Return the priority associated
with the received message as second value.  Return the message LENGTH as a third
value.  Message length could be less than the returned BUFFER length.  In fact,
this is the same buffer used internally in queue to receive all messages.  This
function is provided for better control of the message data.  Most library users
would like to use RECEIVE-STRING, or RECEIVE-BUFFER, or RECEIVE-DISPLACED,
instead.

If the queue is empty, then, by default, RECEIVE blocks until a message becomes
available, or the call is interrupted by a signal handler.  If the :non-blocking
OPEN-FLAG is enabled for the message queue, then the call instead returns
immediately with :try-again.

Conditions:

  BAD-FILE-DESCRIPTOR-INVALID

    The file descriptor specified MQD was invalid or not opened for reading.

  INTERRUPTED-SYSTEM-CALL

    The call was interrupted by a signal handler; see signal(7).

  MESSAGE-TOO-LONG-ON-RECEIVE

    Message length was less than the :message-size attribute of the message
    queue.  This is an intarnal error that should not happen, it is mainly for
    the writer of this library.

Restarts:

  RETRY-ON-INTERRUPT

    If the call was interrupted by a signal handler, you can restart the call."
  (%receive mq-receive receive (values buffer (mem-aref priority :uint) length)))

(defun receive-buffer (queue)
  "Behaves just luke RECEIVE, except that it creates a new buffer with ONLY
message data."
  (%receive mq-receive receive-buffer (values (subseq buffer 0 length) (mem-aref priority :uint))))

(defun receive-string (queue)
  "Behaves just like RECEIVE, except that it tries to convert received message
to string."
  (%receive mq-receive receive-string
            (values (octets-to-string buffer :end length) (mem-aref priority :uint))))

(defun receive-displaced (queue)
  "Behaves just like RECEIVE, except that it tries to return a displaced array
from internal buffer.  You should not use it in a thread, unless protected by a
lock."
  (%receive mq-receive receive-displaced
            (values (make-array length :element-type '(unsigned-byte 8) :displaced-to buffer)
                    (mem-aref priority :uint))))

(defun timed-receive (queue timestamp)
  "Behaves just like RECEIVE, except that if the queue is empty and the
:non-blocking OPEN-FLAG is not enabled for the message queue, then the TIMESTAMP
specifies how long the call will block.  The TIMESTAMP is absolute, not
relative.  If no message is available, and the timeout has already expired by
the time of the call, TIMED-RECEIVE returns immediately with
:connection-timed-out.

Look LOCAL-TIME package for more information on timestamps.

Additional conditions:

  INVALID-ARGUMENT-ON-SEND-RECEIVE

    The call would have blocked, and timeout arguments were invalid, either
    because :sec was less than zero, or because :nsec was less than zero or
    greater than 1000 million."
  (check-type timestamp timestamp)
  (%receive mq-timedreceive timed-receive
            (values buffer (mem-aref priority :uint) length)
            timestamp))

(defun timed-receive-buffer (queue timestamp)
  "Behaves just like TIMED-RECEIVE, except that it creates a new buffer with
ONLY message data."
  (check-type timestamp timestamp)
  (%receive mq-timedreceive timed-receive-buffer
            (values (subseq buffer 0 length) (mem-aref priority :uint))
            timestamp))

(defun timed-receive-string (queue timestamp)
  "Behaves just like TIMED-RECEIVE, except that it tries to convert received
message to string."
  (check-type timestamp timestamp)
  (%receive mq-timedreceive timed-receive-string
            (values (octets-to-string buffer :end length) (mem-aref priority :uint))
            timestamp))

(defun timed-receive-displaced (queue timestamp)
  "Behaves just like TIMED-RECEIVE, except that it tries to return a displaced
array from internal buffer.  You should not use it in a thread, unless protected
by a lock."
  (check-type timestamp timestamp)
  (%receive mq-timedreceive timed-receive-displaced
            (values (make-array length :element-type '(unsigned-byte 8) :displaced-to buffer)
                    (mem-aref priority :uint))
            timestamp))

(defmacro %send (send-fn current-fn &rest send-fn-args)
  "A macro used to generate send functions.  SEND-FN is a function used to send
the actual message.  CURRENT-FN is a function which is called on interrupt.
SEND-FN-ARGS are additional arguments placed at the end of SEND-FN and
CURRENT-FN call."
  `(ecase (with-pointer-to-vector-data (ptr message-buffer)
            (,send-fn queue ptr length priority ,@send-fn-args))
     (:connection-timed-out :connection-timed-out)
     (:try-again :try-again)
     (:invalid-argument (error 'invalid-argument-on-send-receive))
     (:message-too-long (error 'message-too-long-on-send))
     (:bad-file-descriptor (error 'bad-file-descriptor-on-send))
     (:interrupted-system-call
      (if *retry-on-interrupt-p*
          (,current-fn queue message-buffer priority ,@send-fn-args length)
          (restart-case (error 'interrupted-system-call)
            (retry-on-interrupt ()
              :report "Call to send was interrupted.  Retry the call."
              (,current-fn queue message-buffer priority ,@send-fn-args length)))))
     (0 nil)))

(defun send (queue message-buffer priority &optional (length (length message-buffer)))
  "Adds the MESSAGE-BUFFER to the message QUEUE.  MESSAGE-BUFFER length must be
less than or equal to the QUEUE's :message-size attribute.  Zero-length messages
are allowed.  MESSAGE-BUFFER must be an '(array (unsigned-byte 8)).  Additional
LENGTH argument can be provided to limit the message being sent.  By default, it
is equal to MESSAGE-BUFFER length.

The PRIORITY argument is a nonnegative integer that specifies the priority of
new message.  Messages are placed on the QUEUE in decreasing order of priority,
with newer messages of the same priority being placed after older messages with
the same priority.  See mq_overview(7) for details on the range for the message
priority.

If the message QUEUE is already full (i.e., the number of messages on the QUEUE
equals the QUEUE's :max-messages attribute), then, by default, SEND blocks until
sufficient space becomes available to allow the message to be queued, or until
the call is interrupted by a signal handler.  If the :non-blocking flag is
enabled for the message QUEUE, then the call instead returns :try-again.

Note: if you don't want to create a new buffer for sending to save space, you
can reuse QUEUE's buffer.  Use BUFFER function on a QUEUE to get it.  Remember,
that its data will be overwritten on next receive call.

Conditions:

  BAD-FILE-DESCRIPTOR-ON-SEND

    The file descriptor specified MQD was invalid or not opened for writing.

  INTERRUPTED-SYSTEM-CALL

    The call was interrupted by a signal handler; see signal(7).

  MESSAGE-TOO-LONG-ON-SEND

    MESSAGE length was greater than the :message-size attribute of the message
    QUEUE.

Restarts:

  RETRY-ON-INTERRUPT

    If the call was interrupted by a signal handler, you can restart the call."
  (%send mq-send send))

(defun send-string (queue message-string priority)
  "Behaves just like SEND, except that it sends a string, not an
'(array (unsigned-byte 8)) "
  (send queue (string-to-octets message-string) priority))

(defun timed-send (queue message-buffer priority timestamp &optional (length (length message-buffer)))
  "Behaves just like SEND, except that if the QUEUE is full and the
:non-blocking flag is not enabled for the message queue, then TIMESTAMP
specifies how long the call will block.  The TIMESTAMP is absolute, not
relative. If the message queue is full, and the timeout has already expired by
the time of the call, TIMED-SEND returns immediately with :connection-timed-out.

Look LOCAL-TIME package for more information on timestamps."
  (check-type timestamp timestamp)
  (%send mq-timedsend timed-send timestamp))

(defun timed-send-string (queue message-string priority timestamp)
  "Behaves just like TIMED-SEND, except that it sends a string, not an
'(array (unsigned-byte 8)) "
  (timed-send queue (string-to-octets message-string) priority timestamp))

(defmacro with-open-queue ((var name &rest options) &body body)
  "A macro that automatically closes opened queue, even when condition is
signaled.  For OPTIONS see OPEN-QUEUE.

Example:

(with-open-queue (mqueue \"/myqueue\" :open-flags '(:read-write :create))
  (do-something-with mqueue))"
  `(let ((,var (open-queue ,name ,@options)))
     (unwind-protect (progn ,@body)
       (close-queue ,var))))
