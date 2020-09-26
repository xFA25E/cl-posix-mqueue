(defpackage posix-mqueue.condition
  (:nicknames :mq.cond)
  (:documentation "Conditions used in MQ")
  (:use :cl)
  (:import-from #:alexandria #:when-let* #:when-let)
  (:export
   #:kind
   #:access-denied
   #:bad-file-descriptor
   #:file-exists
   #:file-table-overflow
   #:interrupted-system-call
   #:invalid-argument
   #:message-too-long
   #:name-too-long
   #:no-file-or-directory
   #:no-space-left-on-device
   #:out-of-memory
   #:too-many-open-files))
(in-package :posix-mqueue.condition)

(defun error-messages-p (thing)
  (and (listp thing)
       (every (lambda (pair) (and (keywordp (car pair)) (stringp (cdr pair)))) thing)))

(deftype error-messages ()
  '(and list (satisfies error-messages-p)))

(define-condition generic (error)
  ((error-string
    :reader error-string
    :type string
    :documentation "Error string from CFFI's strerror.")
   (kind
    :initarg :kind
    :initform nil
    :reader kind
    :type (or null keyword)
    :documentation "Error KIND used to dispatch on MESSAGES.")
   (messages
    :initform nil
    :reader messages
    :type error-messages
    :documentation
    "Alist with keys as kinds and strings with error messages as values."))

  (:report
   (lambda (condition stream)
     (when-let ((error-string (error-string condition)))
       (format stream "~a.~&" error-string))
     (when-let* ((kind (kind condition))
                 (message (cdr (assoc kind (messages condition)))))
       (format stream "~a" message))))

  (:documentation
   "Generic error  used as the base  for all conditions. It  has an ERROR-STRING
which is always reported.  MESSAGES is an alist with kinds as key and strings as
values.  When condition is signaled, it looks it's own KIND and prints its error
message.  The idea is that use inheritance to overwrite error messages."))

(define-condition out-of-memory (generic)
  ((error-string :initform "Cannot allocate memory" :type string)
   (kind :initform :out-of-memory :type keyword)
   (messages :initform '((:out-of-memory . "Insufficient memory.")) :type error-messages)))

(define-condition file-exists (generic)
  ((error-string :initform "File exists" :type string)
   (kind :initform :file-exists :type keyword)
   (messages
    :initform
    '((:file-exists .
       "Both :create  and :exclusive  were specified  in OPEN-FLAGS,but  a queue
with this NAME already exists."))
    :type error-messages)))

(define-condition file-table-overflow (generic)
  ((error-string :initform "Too many open files in system" :type string)
   (kind :initform :file-table-overflow :type keyword)
   (messages
    :initform
    '((:file-table-overflow .
       "The system-wide  limit on  the total  number of  open files  and message
queues has been reached."))
    :type error-messages)))

(define-condition too-many-open-files (generic)
  ((error-string :initform "Too many open files" :type string)
   (kind :initform :too-many-open-files :type keyword)
   (messages
    :initform
    '((:too-many-open-files .
       "The  per-process limit  on the  number of  open file  and message  queue
descriptors  has  been   reached  (see  the  description   of  RLIMIT_NOFILE  in
getrlimit(2))."))
    :type error-messages)))

(define-condition no-space-left-on-device (generic)
  ((error-string :initform "No space left on device" :type string)
   (kind :initform :no-space-left-on-device :type keyword)
   (messages
    :initform
    '((:no-space-left-on-device .
       "Insufficient  space for  the  creation  of a  new  message queue.   This
probably   occurred  because   the   queues_max  limit   was  encountered;   see
mq_overview(7)."))
    :type error-messages)))

(define-condition name-too-long (generic)
  ((error-string :initform "File name too long" :type string)
   (kind :initform :open-unlink :type keyword)
   (messages :initform '((:open-unlink . "NAME was too long.")) :type error-messages)))

(define-condition interrupted-system-call (generic)
  ((error-string :initform "Interrupted system call" :type string)
   (kind :initform :interrupted-system-call :type keyword)
   (messages
    :initform
    '((:interrupted-system-call .
       "The call was interrupted by a signal handler; see signal(7)."))
    :type error-messages)))

(define-condition no-file-or-directory (generic)
  ((error-string :initform "No such file or directory" :type string)
   (messages
    :initform
    '((:open . "NAME was just \"/\" followed by no other characters.")
      (:open-create .
       "The :create flag was not specified in OPEN-FLAGS, and no queue with this
NAME exists.")
      (:unlink . "There is no message queue with the given NAME."))
    :type error-messages)))

(define-condition bad-file-descriptor (generic)
  ((error-string :initform "Bad file descriptor" :type string)
   (messages
    :initform
    '((:invalid . "The message queue file descriptor (MQD) is invalid.")
      (:receive . "The file descriptor specified MQD was invalid or not opened for reading.")
      (:send . "The file descriptor specified MQD was invalid or not opened for writing."))
    :type error-messages)))

(define-condition access-denied (generic)
  ((error-string :initform "Permission denied" :type string)
   (messages
    :initform
    '((:open-mode .
       "The queue exists, but the caller does  not have permission to open it in
the specified mode.")
      (:open-name . "NAME contained more than one slash.")
      (:unlink . "The caller does not have permission to unlink this message queue."))
    :type error-messages)))

(define-condition invalid-argument (generic)
  ((error-string :initform "Invalid argument" :type string)
   (messages
    :initform
    '((:open-name . "NAME doesn't follow the format in mq_overview(7).")
      (:open-create .
       ":create was  specified in  OPEN-FLAGS, but MAX-MESSAGES  or MESSAGE-SIZE
was invalid. Both of these fields must  be greater than zero.  In a process that
is unprivileged  (does not  have the CAP_SYS_RESOURCE  capability), MAX-MESSAGES
must be less than  or equal to the msg_max limit, and  MESSAGE-SIZE must be less
than  or equal  to the  msgsize_max limit.   In addition,  even in  a privileged
process, :max-messages  cannot exceed  the HARD_MAX limit.   (See mq_overview(7)
for details of these limits.).

Both  of   these  limits  can   be  changed  through   the  /proc/sys/fs/mqueue/
interface.")
      (:setattr . "mq-flags contained flags other than :non-blocking.")
      (:unlink . "The caller does not have permission to unlink this message queue.")
      (:receive-send .
       "The call would have blocked,  and timeout arguments were invalid, either
because :sec was less than zero, or  because :nsec was less than zero or greater
than 1000 million."))
    :type error-messages)))

(define-condition message-too-long (generic)
  ((error-string :initform "Message too long" :type string)
   (messages
    :initform
    '((:receive .
       "message length was less than the :message-size attribute of the message queue.")
      (:send .
       "message  length was  greater  than the  :message-size  attribute of  the
message queue."))
    :type error-messages)))
