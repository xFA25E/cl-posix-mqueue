(in-package #:posix-mqueue)

(define-condition generic (error)
  ((strerror :reader strerror
             :initarg :strerror
             :type string
             :documentation "Error string from CFFI's strerror.")
   (message :reader message
            :initarg :message
            :type string
            :documentation "More specific message string."))
  (:report (lambda (condition stream)
             (format stream "~A.~&~A" (strerror condition) (message condition))))
  (:documentation
   "Generic error used as the base for all conditions.  Must contain STRERROR
   and MESSAGE."))

(define-condition out-of-memory (generic) ()
  (:default-initargs
   :strerror "Cannot allocate memory"
   :message "Insufficient memory."))

(define-condition file-exists (generic) ()
  (:default-initargs
   :strerror "File exists"
   :message "Both :create and :exclusive were specified in OPEN-FLAGS, but a
queue with this NAME already exists."))

(define-condition file-table-overflow (generic) ()
  (:default-initargs
   :strerror "Too many open files in system"
   :message "The system-wide limit on the total number of open files and message
queues has been reached."))

(define-condition too-many-open-files (generic) ()
  (:default-initargs
   :strerror "Too many open files"
   :message "The per-process limit on the number of open file and message queue
descriptors has been reached (see the description of RLIMIT_NOFILE in
getrlimit(2))."))

(define-condition no-space-left-on-device (generic) ()
  (:default-initargs
   :strerror "No space left on device"
   :message "Insufficient space for the creation of a new message queue.  This
probably occurred because the queues_max limit was encountered; see
mq_overview(7)."))

(define-condition name-too-long (generic) ()
  (:default-initargs
   :strerror "File name too long"
   :message "NAME was too long."))

(define-condition interrupted-system-call (generic) ()
  (:default-initargs
   :strerror "Interrupted system call"
   :message "The call was interrupted by a signal handler; see signal(7)."))

(define-condition no-file-or-directory (generic) ()
  (:default-initargs :strerror "No such file or directory"))

(define-condition no-file-or-directory-just-slash (no-file-or-directory) ()
  (:default-initargs
   :message "NAME was just \"/\" followed by no other characters."))

(define-condition no-file-or-directory-no-create (no-file-or-directory) ()
  (:default-initargs
   :message "The :create flag was not specified in OPEN-FLAGS, and no queue with
this NAME exists."))

(define-condition no-file-or-directory-on-unlink (no-file-or-directory) ()
  (:default-initargs
   :message "There is no message queue with the given NAME."))

(define-condition bad-file-descriptor (generic) ()
  (:default-initargs :strerror "Bad file descriptor"))

(define-condition bad-file-descriptor-invalid (bad-file-descriptor) ()
  (:default-initargs
   :message "The message queue file descriptor (MQD) is invalid."))

(define-condition bad-file-descriptor-on-receive (bad-file-descriptor) ()
  (:default-initargs
   :message "The file descriptor specified MQD was invalid or not opened for
reading."))

(define-condition bad-file-descriptor-on-send (bad-file-descriptor) ()
  (:default-initargs
   :message "The file descriptor specified MQD was invalid or not opened for
writing."))

(define-condition access-denied (generic) ()
  (:default-initargs :strerror "Permission denied"))

(define-condition access-denied-permission (access-denied) ()
  (:default-initargs
   :message "The queue exists, but the caller does not have permission to open
it in the specified mode."))

(define-condition access-denied-slashes (access-denied) ()
  (:default-initargs
   :message "NAME contained more than one slash."))

(define-condition access-denied-on-unlink (access-denied) ()
  (:default-initargs
   :message "The caller does not have permission to unlink this message
queue."))

(define-condition invalid-argument (generic) ()
  (:default-initargs :strerror "Invalid argument"))

(define-condition invalid-argument-name (invalid-argument) ()
  (:default-initargs
   :message "NAME doesn't follow the format described in mq_overview(7)."))

(define-condition invalid-argument-sizes (invalid-argument) ()
  (:default-initargs
   :message ":create was specified in OPEN-FLAGS, but MAX-MESSAGES or
MESSAGE-SIZE was invalid. Both of these fields must be greater than zero.  In a
process that is unprivileged (does not have the CAP_SYS_RESOURCE capability),
MAX-MESSAGES must be less than or equal to the msg_max limit, and MESSAGE-SIZE
must be less than or equal to the msgsize_max limit.  In addition, even in a
privileged process, :max-messages cannot exceed the HARD_MAX limit.  (See
mq_overview(7) for details of these limits.).
Both of these limits can be changed through the /proc/sys/fs/mqueue/
interface."))

(define-condition invalid-argument-attributes (invalid-argument) ()
  (:default-initargs
   :message "mq-flags contained flags other than :non-blocking."))

(define-condition invalid-argument-on-unlink (invalid-argument) ()
  (:default-initargs
   :message "The caller does not have permission to unlink this message
queue."))

(define-condition invalid-argument-on-send-receive (invalid-argument) ()
  (:default-initargs
   :message "The call would have blocked, and timeout arguments were invalid,
either because :sec was less than zero, or because :nsec was less than zero or
greater than 1000 million."))

(define-condition message-too-long (generic) ()
  (:default-initargs :strerror "Message too long"))

(define-condition message-too-long-on-receive (message-too-long) ()
  (:default-initargs
   :message "Message length was less than the :message-size attribute of the
message queue."))

(define-condition message-too-long-on-send (message-too-long) ()
  (:default-initargs
   :message "Message length was greater than the :message-size attribute of the
message queue."))
