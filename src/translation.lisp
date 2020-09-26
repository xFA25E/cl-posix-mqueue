(defpackage posix-mqueue.translation
  (:nicknames :mq.trans)
  (:documentation
   "A package that defines translation from/to cffi objects to/from lisp objects.")
  (:use :cl)
  (:import-from #:alexandria #:alist-hash-table #:once-only)
  (:import-from #:cffi
                #:defcvar
                #:expand-from-foreign
                #:expand-to-foreign
                #:expand-to-foreign-dyn
                #:foreign-alloc
                #:foreign-free
                #:foreign-pointer
                #:free-translated-object
                #:null-pointer
                #:null-pointer-p
                #:translate-from-foreign
                #:translate-to-foreign
                #:with-foreign-object
                #:with-foreign-slots)
  (:import-from #:local-time #:nsec-of #:timestamp-to-unix #:timestamp)
  (:import-from #:mq.attr
                #:attributes
                #:current-messages
                #:max-messages
                #:message-size
                #:non-blocking-p)
  (:import-from #:mq.queue #:mqd #:queue)
  (:import-from #:mq.spec.types
                #:mq-attr
                #:mq-attr-get-type
                #:mq-attr-type
                #:mq-curmsgs
                #:mq-flags
                #:mq-maxmsg
                #:mq-msgsize
                #:mqd-type
                #:result-type
                #:timespec-type
                #:timespec
                #:tv-nsec
                #:tv-sec))
(in-package :posix-mqueue.translation)

(declaim (type (unsigned-byte 32) *errno*))
(defcvar "errno" :int "Global C variable used to get errors of functions.")

(declaim (type hash-table *errors*))
(defvar *errors*
  (alist-hash-table
   '((2   . :no-file-or-directory)       ; ENOENT
     (4   . :interrupted-system-call)    ; EINTR
     (9   . :bad-file-descriptor)        ; EBADF
     (11  . :try-again)                  ; EAGAIN
     (12  . :out-of-memory)              ; ENOMEM
     (13  . :access-denied)              ; EACCES
     (17  . :file-exists)                ; EEXIST
     (22  . :invalid-argument)           ; EINVAL
     (23  . :file-table-overflow)        ; ENFILE
     (24  . :too-many-open-files)        ; EMFILE
     (28  . :no-space-left-on-device)    ; ENOSPC
     (36  . :name-too-long)              ; ENAMETOOLONG
     (90  . :message-too-long)           ; EMSGSIZE
     (110 . :connection-timed-out))      ; ETIMEDOUT
   :test #'eq :size 14)
  "Maps C error codes to keyword representation.")

(defmethod translate-from-foreign (result (type result-type))
  "If  CFFI  function returns  -1,  then  return  keyword  representation of  error  code,
otherwise, return CFFI function's returned value."
  (declare (type integer result))
  (if (= -1 result)
      (gethash *errno* *errors*)
      result))

(defmethod expand-from-foreign (result (type result-type))
  "Look TRANSLATE-FROM-FOREIGN (result (type result-type))"
  (once-only (result)
    `(if (= -1 ,result)
         (gethash *errno* *errors*)
         ,result)))

(defmethod translate-to-foreign (timestamp (type timespec-type))
  "Translate LOCAL-TIME:TIMESTAMP to MQ.SPEC.TYPES::MQ-TIMESPEC."
  (declare (type timestamp timestamp))
  (let ((sec (timestamp-to-unix timestamp))
        (nsec (nsec-of timestamp)))
    (let ((ctimespec (foreign-alloc '(:struct timespec))))
      (with-foreign-slots ((tv-sec tv-nsec) ctimespec (:struct timespec))
        (setf tv-sec sec tv-nsec nsec))
      ctimespec)))

(defmethod free-translated-object (pointer (type timespec-type) param)
  "Free MQ.SPEC.TYPES:MQ-TIMESPEC."
  (declare (type foreign-pointer pointer) (ignore param))
  (unless (null-pointer-p pointer)
    (foreign-free pointer)))

(defmethod expand-to-foreign-dyn (value var body (type timespec-type))
  "Translate LOCAL-TIME:TIMESTAMP to  MQ.SPEC.TYPES::MQ-TIMESPEC dynamically.  On some
implementations the value can be allocated on stack."
  `(let ((sec (timestamp-to-unix ,value))
         (nsec (nsec-of ,value)))
     (with-foreign-object (,var '(:struct timespec))
       (with-foreign-slots ((tv-sec tv-nsec) ,var (:struct timespec))
         (setf tv-sec sec tv-nsec nsec))
       ,@body)))

(defmethod translate-to-foreign (attributes (type mq-attr-type))
  "Translate  MQ.ATTR::ATTRIBUTES  to  MQ.SPEC.TYPES::MQ-ATTR.   If  MAX-MESSAGES  and
MESSAGE-SIZE are NULL, return NULL-POINTER"
  (declare (type attributes attributes))
  (with-slots (non-blocking-p max-messages message-size) attributes
    (if (and max-messages message-size)
        (let ((cattributes (foreign-alloc '(:struct mq-attr))))
          (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize) cattributes (:struct mq-attr))
            (setf mq-flags (when non-blocking-p '(:non-blocking))
                  mq-maxmsg max-messages
                  mq-msgsize message-size))
          cattributes)
        (null-pointer))))

(defmethod free-translated-object (pointer (type mq-attr-type) param)
  "Free MQ.SPEC.TYPES::MQ-ATTR."
  (declare (type foreign-pointer pointer) (ignore param))
  (unless (null-pointer-p pointer)
    (foreign-free pointer)))

(defmethod expand-to-foreign-dyn (value var body (type mq-attr-type))
  "Translate  MQ.ATTR::ATTRIBUTES  to  MQ.SPEC.TYPES::MQ-ATTR  dynamically.   On  some
implementations the value can be allocated on stack."
  `(with-slots (non-blocking-p max-messages message-size) ,value
     (if (and max-messages message-size)
         (with-foreign-object (,var '(:struct mq-attr))
           (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize) ,var (:struct mq-attr))
             (setf mq-flags (when non-blocking-p '(:non-blocking))
                   mq-maxmsg max-messages
                   mq-msgsize message-size))
           ,@body)
         (let ((,var (null-pointer)))
           ,@body))))

(defmethod translate-to-foreign (attributes (type mq-attr-get-type))
  "Alloc MQ.SPEC.TYPES::MQ-ATTR and  return a pointer to it. Return  ATTRIBUTES as the
second value."
  (declare (type attributes attributes))
  (values (foreign-alloc '(:struct mq-attr)) attributes))

(defmethod free-translated-object (pointer (type mq-attr-get-type) attributes)
  "At this point the MQ.SPEC.TYPES::MQ-ATTR  struct contains all the information about
queue.  We populate attributes with this information and free the pointer."
  (declare (type foreign-pointer pointer) (type attributes attributes))
  (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs) pointer (:struct mq-attr))
    (with-slots (non-blocking-p max-messages message-size current-messages) attributes
      (setf non-blocking-p (when mq-flags t)
            max-messages mq-maxmsg
            message-size mq-msgsize
            current-messages mq-curmsgs)))
  (foreign-free pointer))

(defmethod expand-to-foreign-dyn (value var body (type mq-attr-get-type))
  "This is the same, but the pointer is freed for us."
  `(with-foreign-object (,var '(:struct mq-attr))
     (prog1 (progn ,@body)
       (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs) ,var (:struct mq-attr))
         (with-slots (non-blocking-p max-messages message-size current-messages) ,value
           (setf non-blocking-p (when mq-flags t)
                 max-messages mq-maxmsg
                 message-size mq-msgsize
                 current-messages mq-curmsgs))))))

(defmethod translate-to-foreign (posix-mqueue (type mqd-type))
  "This one is used to extract queue file descriptor from queue object."
  (declare (type queue posix-mqueue))
  (mqd posix-mqueue))

(defmethod expand-to-foreign (posix-mqueue (type mqd-type))
  "This one is used to extract queue file descriptor from queue object."
  `(mqd ,posix-mqueue))
