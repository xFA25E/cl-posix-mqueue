(defpackage posix-mqueue
  (:nicknames :mq)
  (:documentation
   "This is the main  file of the library, which exports  all the functions that
work  with POSIX  message  queue.   For the  remaining  exported functions  look
POSIX-MQUEUE.ATTRIBUTES (or MQ.ATTR) package.")
  (:use :cl)
  (:shadow #:close)
  (:import-from #:alexandria #:starts-with)
  (:import-from #:babel #:string-to-octets #:octets-to-string)
  (:import-from #:cffi
                #:foreign-pointer
                #:mem-aref
                #:with-foreign-object
                #:with-pointer-to-vector-data)
  (:import-from #:local-time #:now #:timestamp)
  (:import-from #:mq.attr #:attributes #:message-size)
  (:import-from #:mq.cond
                #:access-denied
                #:bad-file-descriptor
                #:file-exists
                #:file-table-overflow
                #:invalid-argument
                #:interrupted-system-call
                #:message-too-long
                #:name-too-long
                #:no-file-or-directory
                #:no-space-left-on-device
                #:out-of-memory
                #:too-many-open-files)
  (:import-from #:mq.queue #:buffer #:queue)
  (:import-from #:mq.spec
                #:mq-open
                #:mq-receive
                #:mq-timedreceive
                #:mq-send
                #:mq-timedsend
                #:mq-getattr
                #:mq-setattr
                #:mq-close
                #:mq-unlink)
  (:export
   #:*retry-on-interrupt-p*
   #:attributes
   #:close
   #:make
   #:non-blocking
   #:receive
   #:receive-displaced
   #:receive-string
   #:send
   #:send-string
   #:timed-receive
   #:timed-receive-displaced
   #:timed-receive-string
   #:timed-send
   #:timed-send-string
   #:unlink
   #:with-queue))
(in-package :posix-mqueue)

(declaim (type boolean *retry-on-interrupt-p*))
(defvar *retry-on-interrupt-p* t
  "Whether or not to retry send/receive operation on interrupt.")

(declaim (ftype (function (string &key
                                  (:open-flags mq.spec.types::oflags)
                                  (:mode mq.spec.types::mode)
                                  (:max-messages (or null (unsigned-byte 64)))
                                  (:message-size (or null (unsigned-byte 64))))
                          (values queue &optional))
                make))
(defun make (name &key (open-flags '(:read-only)) (mode '(:user-read :user-write))
                    max-messages message-size)
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

This  function can  signal  conditions.   Some of  them  can  be inspected  with
POSIX-MQUEUE.CONDITION:KIND (or MQ.COND:KIND).  Conditions:

  ACCESS-DENIED :open-mode

    The queue exists, but the caller does  not have permission to open it in the
    specified mode.

  ACCESS-DENIED :open-name

    NAME contained more than one slash.

  FILE-EXISTS

    Both :create and  :exclusive were specified in OPEN-FLAGS, but  a queue with
    this NAME already exists.

  INVALID-ARGUMENT :open-name

    NAME doesn't follow the format in mq_overview(7).

  INVALID-ARGUMENT :open-create

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

  NO-FILE-OR-DIRECTORY :open

    NAME was just \"/\" followed by no other characters.

  NO-FILE-OR-DIRECTORY :open-create

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

  (assert (typep open-flags 'mq.spec.types::oflags) nil
          "Exactly one of the following must be specified in OPEN-FLAGS:
:read-only :write-only :read-write")

  (let* ((attrs (mq.attr::make :max-messages max-messages :message-size message-size))
         (mqd (mq-open name open-flags mode attrs)))
    (case mqd
      (:access-denied
       (error 'access-denied :kind (if (= 1 (count #\/ name)) :open-mode :open-name)))
      (:file-exists
       (error 'file-exists))
      (:invalid-argument
       (error 'invalid-argument :kind (if (starts-with #\/ name) :open-name :open-create)))
      (:too-many-open-files
       (error 'too-many-open-files))
      (:name-too-long
       (error 'name-too-long))
      (:file-table-overflow
       (error 'file-table-overflow))
      (:no-file-or-directory
       (error 'no-file-or-directory :kind (if (string= name "/") :open :open-create)))
      (:out-of-memory
       (error 'out-of-memory))
      (:no-space-left-on-device
       (error 'no-space-left-on-device))
      (t
       (let ((queue (make-instance 'mq.queue::queue :mqd mqd)))
         (ecase (mq-getattr queue attrs)
           (:bad-file-descriptor (error 'bad-file-descriptor :kind :invalid))
           (0 (let* ((size (message-size attrs))
                     (buffer (make-array size :element-type '(unsigned-byte 8))))
                (setf (buffer queue) buffer)
                queue))))))))

(declaim (ftype (function (string) (values null &optional)) unlink))
(defun unlink (name)
  "Remove the specified  message queue NAME.  The message queue  NAME is removed
immediately.  The queue  itself is destroyed once any other  processes that have
the queue open close their descriptors referring to the queue.

Conditions:

  ACCESS-DENIED

    The caller does not have permission to unlink this message queue.

  NAME-TOO-LONG

    NAME was too long.

  NO-FILE-OR-DIRECTORY

    There is no message queue with the given NAME."

  (ecase (mq-unlink name)
    (:access-denied (error 'access-denied :kind :unlink))
    (:name-too-long (error 'name-too-long))
    (:no-file-or-directory (error 'no-file-or-directory :kind :unlink))
    (0 nil)))

(declaim (ftype (function (queue) (values null &optional)) close))
(defun close (queue)
  "Close the message queue.

Conditions:

  BAD-FILE-DESCRIPTOR

    The message queue file descriptor (MQD) is invalid."

  (ecase (mq-close queue)
    (:bad-file-descriptor (error 'bad-file-descriptor :kind :invalid))
    (0 nil)))

(declaim (ftype (function (queue) (values attributes &optional)) attributes))
(defun attributes (queue)
  "Retrieve attributes of the message queue.

Conditions:

  BAD-FILE-DESCRIPTOR

    The message queue file descriptor (MQD) is invalid."
  (let ((attributes (mq.attr::make)))
    (ecase (mq-getattr queue attributes)
      (:bad-file-descriptor (error 'bad-file-descriptor :kind :invalid))
      (0 attributes))))

(declaim (ftype (function (boolean queue) (values boolean &optional)) (setf non-blocking)))
(defun (setf non-blocking) (non-blocking queue)
  "Modify NON-BLOCKING-P attribute of the message queue.

Conditions:

  BAD-FILE-DESCRIPTOR

    The message queue file descriptor (MQD) is invalid.

  INVALID-ARGUMENT

    mq-flags contained flags other than :non-blocking. This is an internal error
    that should not happen, it is mainly for the writer of this library."

  (let ((attrs (mq.attr::make-set-non-blocking non-blocking)))
    (ecase (mq-setattr queue attrs (cffi:null-pointer))
      (:bad-file-descriptor (error 'bad-file-descriptor :kind :invalid))
      (:invalid-argument (error 'invalid-argument :kind :setattr))
      (0 non-blocking))))

(declaim (ftype (function (queue
                           &optional
                           (function ((array (unsigned-byte 8))
                                      (unsigned-byte 64)
                                      (unsigned-byte 32)))
                           (function (queue foreign-pointer (unsigned-byte 64) foreign-pointer
                                            &rest *)
                                     (values (or keyword (unsigned-byte 64)) &optional))
                           &rest *))
                %receive))
(defun %receive (queue &optional (callback #'values) (fn #'mq-receive) &rest args)
  "Low-level function used  by all high-level functions internally.   It takes a
FN, applies QUEUE  and ARGS on it  and returns by calling  a CALLBACK.  CALLBACK
must be a function that accepts three arguments: BUFFER, LENGTH and PRIORITY.

BUFFER is an '(array (unsigned-byte 8))

LENGTH is a length of the message.  It can be smaller that BUFFER length."

  (with-foreign-object (priority :uint)
    (with-pointer-to-vector-data (ptr (buffer queue))
      (let* ((length (apply fn queue ptr (length (buffer queue)) priority args)))
        (case length
          ((:connection-timed-out :try-again) length)
          (:invalid-argument (error 'invalid-argument :kind :receive-send))
          (:message-too-long (error 'message-too-long :kind :receive))
          (:bad-file-descriptor (error 'bad-file-descriptor :kind :receive))
          (:interrupted-system-call
           (if *retry-on-interrupt-p*
               (apply #'%receive queue callback fn args)
               (restart-case (error 'interrupted-system-call)
                 (retry-on-interrupt ()
                   :report "Call to receive was interrupted.  Retry the call."
                   (apply #'%receive queue callback fn args)))))
          (t (funcall callback (buffer queue) length (mem-aref priority :uint))))))))

(declaim (ftype (function (queue &optional (function ((array (unsigned-byte 8))
                                                      (unsigned-byte 64)
                                                      (unsigned-byte 32)))
                                 timestamp))
                %timed-receive))
(defun %timed-receive (queue &optional (callback #'values) (timestamp (now)))
  "Behaves   like    %RECEIVE,   except   that    it   can   also    receive   a
LOCAL-TIME:TIMESTAMP."
  (%receive queue callback #'mq-timedreceive timestamp))

(declaim (ftype (function ((array (unsigned-byte 8)) (unsigned-byte 64) (unsigned-byte 32))
                          (values (array (unsigned-byte 8)) (unsigned-byte 32) &optional))
                subsequence subsequence-displaced)
         (inline subsequence subsequence-displaced))
(defun subsequence (buffer length priority)
  "Return  sub-sequence with  LENGTH length  of  BUFFER and  PRIORITY as  second
value."
  (values (subseq buffer 0 length) priority))

(declaim (ftype (function ((array (unsigned-byte 8)) (unsigned-byte 64) (unsigned-byte 32))
                          (values string (unsigned-byte 32) &optional))
                subsequence-string)
         (inline subsequence-string))
(defun subsequence-string (buffer length priority)
  "Return sub-string with LENGTH length of BUFFER and PRIORITY as second value."
  (values (octets-to-string buffer :end length) priority))

(defun subsequence-displaced (buffer length priority)
  "Return a displaced sub-sequence with LENGTH  length of BUFFER and PRIORITY as
second value."
  (values (make-array length :element-type '(unsigned-byte 8) :displaced-to buffer) priority))

(declaim (ftype (function
                 (queue)
                 (values (or keyword (array (unsigned-byte 8)))
                         (or null (unsigned-byte 32))
                         &optional))
                receive receive-displaced))
(defun receive (queue)
  "Remove the  oldest message with the  highest priority from the  message QUEUE
and return  it as  '(ARRAY (UNSIGNED-BYTE 8)).   Return the  priority associated
with the received message as second value.

If the queue is empty, then, by  default, RECEIVE blocks until a message becomes
available, or the call is interrupted by a signal handler.  If the :non-blocking
OPEN-FLAG  is enabled  for  the message  queue, then  the  call instead  returns
immediately with :try-again.

Conditions:

  BAD-FILE-DESCRIPTOR

    The file descriptor specified MQD was invalid or not opened for reading.

  INTERRUPTED-SYSTEM-CALL

    The call was interrupted by a signal handler; see signal(7).

  MESSAGE-TOO-LONG

    Message  length was  less than  the :message-size  attribute of  the message
    queue.  This is an  intarnal error that should not happen,  it is mainly for
    the writer of this library.

Restarts:

  RETRY-ON-INTERRUPT

    If the call was interrupted by a signal handler, you can restart the call."
  (%receive queue #'subsequence))

(declaim (ftype (function (queue) (values (or keyword string)
                                          (or null (unsigned-byte 32))
                                          &optional))
                receive-string))
(defun receive-string (queue)
  "Behaves just like  RECEIVE, except that it tries to  convert received message
to string."
  (%receive queue #'subsequence-string))

(defun receive-displaced (queue)
  "Behaves just like  RECEIVE, except that it tries to  return a displaced array
from internal buffer.  You should not use  it in a thread, unless protected by a
lock."
  (%receive queue #'subsequence-displaced))

(declaim (ftype (function
                 (queue &key (:timestamp timestamp))
                 (values (or keyword (array (unsigned-byte 8)))
                         (or null (unsigned-byte 32))
                         &optional))
                timed-receive timed-receive-displaced))
(defun timed-receive (queue &key (timestamp (now)))
  "Behaves  just  like RECEIVE,  except  that  if the  queue  is  empty and  the
:non-blocking OPEN-FLAG  is not  enabled for the  message queue,  then TIMESTAMP
specifies  how  long the  call  will  block.   The  TIMESTAMP is  absolute,  not
relative.  If  no message is available,  and the timeout has  already expired by
the    time   of    the   call,    TIMED-RECEIVE   returns    immediately   with
:connection-timed-out.

Look LOCAL-TIME package for more information on timestamps.

Additional conditions:

  INVALID-ARGUMENT

    The call would have blocked, and  timeout arguments were invalid, either because
    :sec was less than zero, or because :nsec was less than zero or greater than
    1000 million."
  (%timed-receive queue #'subsequence timestamp))

(declaim (ftype (function
                 (queue &key (:timestamp timestamp))
                 (values (or keyword string) (or null (unsigned-byte 32)) &optional))
                timed-receive-string))
(defun timed-receive-string (queue &key (timestamp (now)))
  "Behaves just  like TIMED-RECEIVE,  except that it  tries to  convert received
message to string."
  (%timed-receive queue #'subsequence-string timestamp))

(defun timed-receive-displaced (queue &key (timestamp (now)))
  "Behaves just like  TIMED-RECEIVE, except that it tries to  return a displaced
array from internal buffer.  You should not use it in a thread, unless protected
by a lock."
  (%timed-receive queue #'subsequence-displaced timestamp))

(declaim (ftype (function (queue
                           (array (unsigned-byte 8))
                           &optional
                           (unsigned-byte 32)
                           (function (queue foreign-pointer (unsigned-byte 32) &rest *)
                                     (values (or keyword (integer 0 0)) &optional))
                           &rest *))))
(defun %send (queue message &optional (priority 0) (fn #'mq-send) &rest args)
  "Low-level function used  by all high-level functions internally.   It takes a
FN and applies QUEUE, PRIORITY and ARGS on it."
  (check-type message (array (unsigned-byte 8)))
  (with-pointer-to-vector-data (ptr message)
    (ecase (apply fn queue ptr (length message) priority args)
      (:connection-timed-out :connection-timed-out)
      (:try-again :try-again)
      (:invalid-argument (error 'invalid-argument :kind :receive-send))
      (:message-too-long (error 'message-too-long :kind :send))
      (:bad-file-descriptor (error 'bad-file-descriptor :kind :send))
      (:interrupted-system-call
       (if *retry-on-interrupt-p*
           (apply #'%send queue message priority fn args)
           (restart-case (error 'interrupted-system-call)
             (retry-on-interrupt ()
               :report "Call to send was interrupted.  Retry the call."
               (apply #'%send queue message priority fn args)))))
      (0 nil))))

(declaim (ftype (function (queue
                           (array (unsigned-byte 8))
                           &key (:priority (unsigned-byte 32)))
                          (values (or keyword null) &optional))
                send))
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

  BAD-FILE-DESCRIPTOR

    The file descriptor specified MQD was invalid or not opened for writing.

  INTERRUPTED-SYSTEM-CALL

    The call was interrupted by a signal handler; see signal(7).

  MESSAGE-TOO-LONG

    MESSAGE length was  greater than the :message-size attribute  of the message
    QUEUE.

Restarts:

  RETRY-ON-INTERRUPT

    If the call was interrupted by a signal handler, you can restart the call."

  (%send queue message priority))

(declaim (ftype (function (queue string &key (:priority (unsigned-byte 32)))
                          (values (or keyword null) &optional))
                send-string))
(defun send-string (queue string &key (priority 0))
  "Behaves   just    like   SEND,   except    that   it   sends    string,   not
'(array (unsigned-byte 8)) "
  (%send queue (string-to-octets string) priority))

(declaim (ftype (function (queue
                           (array (unsigned-byte 8))
                           &key
                           (:priority (unsigned-byte 32))
                           (:timestamp timestamp))
                          (values (or keyword null) &optional))
                timed-send))
(defun timed-send (queue message &key (timestamp (now)) (priority 0))
  "Behaves  just  like  SEND,  except  that   if  the  QUEUE  is  full  and  the
:non-blocking  flag  is  not  enabled  for the  message  queue,  then  TIMESTAMP
specifies  how  long the  call  will  block.   The  TIMESTAMP is  absolute,  not
relative. If the message  queue is full, and the timeout  has already expired by
the    time    of    the    call,   TIMED-SEND    returns    immediately    with
:connection-timed-out."
  (%send queue message priority #'mq-timedsend timestamp))

(declaim (ftype (function (queue
                           string
                           &key
                           (:priority (unsigned-byte 32))
                           (:timestamp timestamp))
                          (values (or keyword null) &optional))
                timed-send-string))
(defun timed-send-string (queue string &key (timestamp (now)) (priority 0))
  "Behaves   just  like   TIMED-SEND,   except  that   it   sends  string,   not
'(array (unsigned-byte 8)) "
  (timed-send queue (string-to-octets string) :timestamp timestamp :priority priority))

(defmacro with-queue ((var name
                       &rest options
                       &key (open-flags '(:read-only)) (mode '(:user-read :user-write))
                         max-messages message-size)
                      &body body)
  "A macro that automatically closes opened queue, even when condition is signaled.

Example:

(with-queue (mqueue \"/myqueue\" :open-flags '(:read-write :create))
  (do-something-with mqueue))"
  (declare (ignore open-flags mode max-messages message-size))
  `(let ((,var (make ,name ,@options)))
     (declare (type queue ,var))
     (unwind-protect (progn ,@body)
       (close ,var))))
