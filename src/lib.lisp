(in-package :posix-mqueue)

(defvar *retry-on-interrupt-p* t
  "Whether or not to retry send/receive operation on interrupt.")

(defun random-queue-name (&key (length 25) (start 97) (end 123))
  (declare (type (integer 1 4294967295) length start end))
  (assert (typep length '(integer 1 4294967295)))
  (let ((result (make-string (+ 1 length))))
    (declare (type string result))
    (setf (aref result 0) #\/)
    (loop :for i :from 1 :to length
          :do (setf (aref result i)
                    (code-char (+ start (random (- end start))))))
    result))

(defun default-attributes ()
  (let* ((name (random-queue-name :length 255))
         (mqd (mq-open name '(:create) '(:user-read :user-write) nil))
         (queue (make-queue :mqd mqd)))
    (declare (type string name) (type (unsigned-byte 32) mqd) (type queue queue))
    (unwind-protect (attributes queue)
      (mq-close queue)
      (mq-unlink name))))

(defun open-queue (name &key
                          (open-flags '(:read-only))
                          (create-modes '(:user-read :user-write))
                          max-messages message-size without-buffer)
  "Create a new POSIX message queue or open an existing queue.

NAME is a  string that identifies a  queue.  It MUST start with  a slash (\"/\")
and MUST NOT contain other slashes.  Example: \"/myqueue\".

OPEN-FLAGS is a list  of flags that control the operation  of queue. Exactly one
of the following must be specified in OPEN-FLAGS:

  :read-only

    Open the queue to receive messages only.

  :write-only

    Open the queue to send messages only.

  :read-write

    Open the queue to both send and receive messages.

Zero or more of the following flags:

  :close-on-exec

    Set the  close-on-exec flag for  the message queue descriptor.   See open(2)
    for a discussion of why this flag is useful.

  :create

    Create the message queue  if it does not exist.  The owner  (user ID) of the
    message queue is set  to the effective user ID of  the calling process.  The
    group ownership (group ID)  is set to the effective group  ID of the calling
    process.

  :exclusive

    If :create  was specified  in OPEN-FLAGS,  and a queue  with the  given name
    already exists, then fail signaling FILE-EXISTS condition.

  :non-blocking

    Open the queue in nonblocking mode.  In circumstances where RECEIVE and SEND
    would normally block, these functions instead return :try-again.

If :create  is specified in OPEN-FLAGS,  then three additional arguments  can be
supplied.  The MODE  argument specifies the permissions to be  placed on the new
queue.  It is a list of the following possible flags:

  :user-read :user-write :group-read :group-write :other-read :other-read

In  addition,  MAX-MESSAGES  and  MESSAGE-SIZE specify  the  maximum  number  of
messages and the  maximum size of messages that the  queue will allow.  Usually,
they default to their maximum values, 10 and 8192 respectively, but these values
can be changes through /proc/sys/fs/mqueue/ interface.  Note, the XOR principlie
applies here, both of these values must be specified or none of them.

This function can signal conditions.  Conditions:

  ACCESS-DENIED-PERMISSION

    The queue exists, but the caller does  not have permission to open it in the
    specified mode.

  ACCESS-DENIED-SLASHES

    NAME contained more than one slash.

  FILE-EXISTS

    Both :create and  :exclusive were specified in OPEN-FLAGS, but  a queue with
    this NAME already exists.

  INVALID-ARGUMENT-NAME

    NAME doesn't follow the format in mq_overview(7).

  INVALID-ARGUMENT-SIZES

    :create was  specified in OPEN-FLAGS,  but MAX-MESSAGES or  MESSAGE-SIZE was
    invalid. Both of these fields must be  greater than zero.  In a process that
    is   unprivileged  (does   not   have   the  CAP_SYS_RESOURCE   capability),
    MAX-MESSAGES  must  be  less  than  or  equal  to  the  msg_max  limit,  and
    MESSAGE-SIZE  must be  less  than or  equal to  the  msgsize_max limit.   In
    addition,  even in  a privileged  process, :max-messages  cannot exceed  the
    HARD_MAX limit.  (See mq_overview(7) for details of these limits.).

    Both  of  these  limits  can be  changed  through  the  /proc/sys/fs/mqueue/
    interface.

  TOO-MANY-OPEN-FILES

    The  per-process  limit  on  the  number of  open  file  and  message  queue
    descriptors  has  been reached  (see  the  description of  RLIMIT_NOFILE  in
    getrlimit(2)).

  NAME-TOO-LONG

    NAME was too long.

  FILE-TABLE-OVERFLOW

    The system-wide limit  on the total number of open  files and message queues
    has been reached.

  NO-FILE-OR-DIRECTORY-JUST-SLASH

    NAME was just \"/\" followed by no other characters.

  NO-FILE-OR-DIRECTORY-NO-CREATE

    The :create  flag was not  specified in OPEN-FLAGS,  and no queue  with this
    NAME exists.

  OUT-OF-MEMORY

    Insufficient memory.

  NO-SPACE-LEFT-ON-DEVICE

    Insufficient space for the creation of a new message queue.  This probably
    occurred because the queues_max limit was encountered; see mq_overview(7).

  SIMPLE-ERROR

    This one can be signalled if the OPEN-FLAGS or the MODE is invalid.

  BAD-FILE-DESCRIPTOR

    The message  queue file descriptor  (MQD) is  invalid.  This is  an internal
    error that should not happen, it is mainly for the writer of this library."

  (declare (optimize (safety 3))
           (type string name)
           (type open-flags open-flags)
           (type create-modes create-modes)
           (type (or null (integer 1 4294967295)) max-messages message-size)
           (type boolean without-buffer))

  (assert (typep open-flags 'open-flags))
  (assert (typep create-modes 'create-modes))
  (assert (typep max-messages '(or null (integer 1 4294967295))))
  (assert (typep message-size '(or null (integer 1 4294967295))))

  (let* ((sizes (when (or max-messages message-size)
                  (cons (or max-messages (max-messages (default-attributes)))
                        (or message-size (message-size (default-attributes))))))
         (mqd (mq-open name open-flags create-modes sizes)))
    (declare (type (or null (cons (integer 1 4294967295) (integer 1 4294967295))) sizes)
             (type (or keyword integer) mqd))

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
       (make-queue
        :mqd mqd
        :buffer (unless without-buffer
                  (make-array message-size :element-type '(unsigned-byte 8))))))))

(defun unlink (name)
  "Remove the specified  message queue NAME.  The message queue  NAME is removed
immediately.  The queue  itself is destroyed once any other  processes that have
the queue open close their descriptors referring to the queue.

Conditions:

  ACCESS-DENIED-ON-UNLINK

    The caller does not have permission to unlink this message queue.

  NAME-TOO-LONG

    NAME was too long.

  NO-FILE-OR-DIRECTORY-ON-UNLINK

    There is no message queue with the given NAME."
  (declare (type string name))
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
  (declare (type queue queue))
  (ecase (mq-close queue)
    (:bad-file-descriptor (error 'bad-file-descriptor-invalid))
    (0 nil)))

(defun attributes (queue)
  "Retrieve attributes of the message queue.

Conditions:

  BAD-FILE-DESCRIPTOR-INVALID

    The message queue file descriptor (MQD) is invalid."
  (declare (type queue queue))
  (let ((attributes (make-attributes)))
    (declare (type attributes attributes))
    (ecase (mq-getattr queue attributes)
      (:bad-file-descriptor (error 'bad-file-descriptor-invalid))
      (0 attributes))))

(defun set-non-blocking (queue non-blocking-p)
  "Modify NON-BLOCKING-P attribute of the message queue.

Conditions:

  BAD-FILE-DESCRIPTOR-INVALID

    The message queue file descriptor (MQD) is invalid.

  INVALID-ARGUMENT-ATTRIBUTES

    mq-flags contained flags other than :non-blocking. This is an internal error
    that should not happen, it is mainly for the writer of this library."
  (declare (type queue queue) (type boolean non-blocking-p))
  (ecase (mq-setattr queue non-blocking-p (cffi:null-pointer))
    (:bad-file-descriptor (error 'bad-file-descriptor-invalid))
    (:invalid-argument (error 'invalid-argument-attributes))
    (0 non-blocking-p)))

(defun %receive (queue &optional (callback #'values) (fn #'mq-receive) (buffer (buffer queue)) &rest args)
  "Low-level function used  by all high-level functions internally.   It takes a
FN, applies QUEUE  and ARGS on it  and returns by calling  a CALLBACK.  CALLBACK
must be a function that accepts three arguments: BUFFER, LENGTH and PRIORITY.

BUFFER is an '(array (unsigned-byte 8))

LENGTH is a length of the message.  It can be smaller that BUFFER length."

  (with-foreign-object (priority :uint)
    (with-pointer-to-vector-data (ptr buffer)
      (let* ((length (apply fn queue ptr (length buffer) priority args)))
        (case length
          ((:connection-timed-out :try-again) length)
          (:invalid-argument (error 'invalid-argument-on-send-receive))
          (:message-too-long (error 'message-too-long-on-receive))
          (:bad-file-descriptor (error 'bad-file-descriptor-on-receive))
          (:interrupted-system-call
           (if *retry-on-interrupt-p*
               (apply #'%receive queue callback fn args)
               (restart-case (error 'interrupted-system-call)
                 (retry-on-interrupt ()
                   :report "Call to receive was interrupted.  Retry the call."
                   (apply #'%receive queue callback fn args)))))
          (t (funcall callback buffer length (mem-aref priority :uint))))))))

(defun %timed-receive (queue &optional (callback #'values) (timestamp (now)) (buffer (buffer queue)))
  "Behaves   like    %RECEIVE,   except   that    it   can   also    receive   a
LOCAL-TIME:TIMESTAMP."
  (%receive queue callback #'mq-timedreceive buffer timestamp))

(defun subsequence (buffer length priority)
  "Return  sub-sequence with  LENGTH length  of  BUFFER and  PRIORITY as  second
value."
  (values (subseq buffer 0 length) priority))

(defun subsequence-string (buffer length priority)
  "Return sub-string with LENGTH length of BUFFER and PRIORITY as second value."
  (values (octets-to-string buffer :end length) priority))

(defun subsequence-displaced (buffer length priority)
  "Return a displaced sub-sequence with LENGTH  length of BUFFER and PRIORITY as
second value."
  (values (make-array length :element-type '(unsigned-byte 8) :displaced-to buffer) priority))

(defun receive (queue &key (buffer (buffer queue)))
  "Remove the  oldest message with the  highest priority from the  message QUEUE
and return  it as  '(ARRAY (UNSIGNED-BYTE 8)).   Return the  priority associated
with the received message as second value.

If the queue is empty, then, by  default, RECEIVE blocks until a message becomes
available, or the call is interrupted by a signal handler.  If the :non-blocking
OPEN-FLAG  is enabled  for  the message  queue, then  the  call instead  returns
immediately with :try-again.

Conditions:

  BAD-FILE-DESCRIPTOR-INVALID

    The file descriptor specified MQD was invalid or not opened for reading.

  INTERRUPTED-SYSTEM-CALL

    The call was interrupted by a signal handler; see signal(7).

  MESSAGE-TOO-LONG-ON-RECEIVE

    Message  length was  less than  the :message-size  attribute of  the message
    queue.  This is an  intarnal error that should not happen,  it is mainly for
    the writer of this library.

Restarts:

  RETRY-ON-INTERRUPT

    If the call was interrupted by a signal handler, you can restart the call."
  (%receive queue #'subsequence #'mq-receive buffer))

(defun receive-string (queue)
  "Behaves just like  RECEIVE, except that it tries to  convert received message
to string."
  (%receive queue #'subsequence-string))

(defun receive-displaced (queue)
  "Behaves just like  RECEIVE, except that it tries to  return a displaced array
from internal buffer.  You should not use  it in a thread, unless protected by a
lock."
  (%receive queue #'subsequence-displaced))

(defun timed-receive (queue &key (timestamp (now)) (buffer (buffer queue)))
  "Behaves  just  like RECEIVE,  except  that  if the  queue  is  empty and  the
:non-blocking OPEN-FLAG  is not  enabled for the  message queue,  then TIMESTAMP
specifies  how  long the  call  will  block.   The  TIMESTAMP is  absolute,  not
relative.  If  no message is available,  and the timeout has  already expired by
the    time   of    the   call,    TIMED-RECEIVE   returns    immediately   with
:connection-timed-out.

Look LOCAL-TIME package for more information on timestamps.

Additional conditions:

  INVALID-ARGUMENT-ON-SEND-RECEIVE

    The call would have blocked, and  timeout arguments were invalid, either because
    :sec was less than zero, or because :nsec was less than zero or greater than
    1000 million."
  (%timed-receive queue #'subsequence timestamp buffer))

(defun timed-receive-string (queue &key (timestamp (now)))
  "Behaves just  like TIMED-RECEIVE,  except that it  tries to  convert received
message to string."
  (%timed-receive queue #'subsequence-string timestamp))

(defun timed-receive-displaced (queue &key (timestamp (now)))
  "Behaves just like  TIMED-RECEIVE, except that it tries to  return a displaced
array from internal buffer.  You should not use it in a thread, unless protected
by a lock."
  (%timed-receive queue #'subsequence-displaced (buffer queue) timestamp))

(defun %send (queue message &optional (priority 0) (fn #'mq-send) &rest args)
  "Low-level function used  by all high-level functions internally.   It takes a
FN and applies QUEUE, PRIORITY and ARGS on it."
  (check-type message (array (unsigned-byte 8)))
  (with-pointer-to-vector-data (ptr message)
    (ecase (apply fn queue ptr (length message) priority args)
      (:connection-timed-out :connection-timed-out)
      (:try-again :try-again)
      (:invalid-argument (error 'invalid-argument-on-send-receive))
      (:message-too-long (error 'message-too-long-on-send))
      (:bad-file-descriptor (error 'bad-file-descriptor-on-send))
      (:interrupted-system-call
       (if *retry-on-interrupt-p*
           (apply #'%send queue message priority fn args)
           (restart-case (error 'interrupted-system-call)
             (retry-on-interrupt ()
               :report "Call to send was interrupted.  Retry the call."
               (apply #'%send queue message priority fn args)))))
      (0 nil))))

(defun send (queue message &key (priority 0))
  "Adds the MESSAGE to  the message QUEUE.  MESSAGE length must  be less than or
equal to the QUEUE's :message-size attribute.  Zero-length messages are allowed.
MESSAGE must be an '(array (unsigned-byte 8)).

The PRIORITY  argument is a nonnegative  integer that specifies the  priority of
this MESSAGE.  Messages are placed on the QUEUE in decreasing order of priority,
with newer messages of the same  priority being placed after older messages with
the same priority.  See mq_overview(7) for  details on the range for the message
priority.

If the message QUEUE is already full  (i.e., the number of messages on the QUEUE
equals the QUEUE's :max-messages attribute), then, by default, SEND blocks until
sufficient space becomes  available to allow the message to  be queued, or until
the  call is  interrupted by  a signal  handler.  If  the :non-blocking  flag is
enabled for the message QUEUE, then  the call instead returns :try-again.

Conditions:

  BAD-FILE-DESCRIPTOR-ON-SEND

    The file descriptor specified MQD was invalid or not opened for writing.

  INTERRUPTED-SYSTEM-CALL

    The call was interrupted by a signal handler; see signal(7).

  MESSAGE-TOO-LONG-ON-SEND

    MESSAGE length was  greater than the :message-size attribute  of the message
    QUEUE.

Restarts:

  RETRY-ON-INTERRUPT

    If the call was interrupted by a signal handler, you can restart the call."

  (%send queue message priority))

(defun send-string (queue string &key (priority 0))
  "Behaves   just    like   SEND,   except    that   it   sends    string,   not
'(array (unsigned-byte 8)) "
  (%send queue (string-to-octets string) priority))

(defun timed-send (queue message &key (timestamp (now)) (priority 0))
  "Behaves  just  like  SEND,  except  that   if  the  QUEUE  is  full  and  the
:non-blocking  flag  is  not  enabled  for the  message  queue,  then  TIMESTAMP
specifies  how  long the  call  will  block.   The  TIMESTAMP is  absolute,  not
relative. If the message  queue is full, and the timeout  has already expired by
the    time    of    the    call,   TIMED-SEND    returns    immediately    with
:connection-timed-out."
  (%send queue message priority #'mq-timedsend timestamp))

(defun timed-send-string (queue string &key (timestamp (now)) (priority 0))
  "Behaves   just  like   TIMED-SEND,   except  that   it   sends  string,   not
'(array (unsigned-byte 8)) "
  (timed-send queue (string-to-octets string) :timestamp timestamp :priority priority))

(defmacro with-open-queue ((var name &rest options) &body body)
  "A macro that automatically closes opened queue, even when condition is
signaled.  For OPTIONS see OPEN-QUEUE.

Example:

(with-open-queue (mqueue \"/myqueue\" :open-flags '(:read-write :create))
  (do-something-with mqueue))"
  `(let ((,var (open-queue ,name ,@options)))
     (declare (type queue ,var))
     (unwind-protect (progn ,@body)
       (close-queue ,var))))
