(in-package :posix-mqueue)

(defcvar "errno" :int "Global C variable used to get errors of functions.")

(defmethod expand-from-foreign (result (type result-type))
  "If  CFFI  function returns  -1,  then  return  keyword  representation of  error  code,
otherwise, return CFFI function's returned value."
  (once-only (result)
    `(if (= -1 ,result)
         (ecase *errno*
           (2 :no-file-or-directory)     ; ENOENT
           (4 :interrupted-system-call)  ; EINTR
           (9  :bad-file-descriptor)     ; EBADF
           (11 :try-again)               ; EAGAIN
           (12 :out-of-memory)           ; ENOMEM
           (13 :access-denied)           ; EACCES
           (17 :file-exists)             ; EEXIST
           (22 :invalid-argument)        ; EINVAL
           (23 :file-table-overflow)     ; ENFILE
           (24 :too-many-open-files)     ; EMFILE
           (28 :no-space-left-on-device) ; ENOSPC
           (36 :name-too-long)           ; ENAMETOOLONG
           (90 :message-too-long)        ; EMSGSIZE
           (110 :connection-timed-out))  ; ETIMEDOUT
         ,result)))

(defmethod expand-to-foreign-dyn (value var body (type timespec-type))
  "VALUE is a LOCAL-TIME:TIMESTAMP. Translate VALUE to MQ-TIMESPEC dynamically.
On some implementations the value can be allocated on stack."
  `(with-foreign-object (,var '(:struct timespec))
     (with-foreign-slots ((tv-sec tv-nsec) ,var (:struct timespec))
       (setf tv-sec (timestamp-to-unix ,value) tv-nsec (nsec-of ,value)))
     ,@body))

(defmethod expand-to-foreign-dyn (value var body (type mq-size-attr-type))
  "Translate VALUE to MQ-ATTR dynamically.  On some implementations the value
can be allocated on stack. VALUE is a CONS of (MAX-MESSAGES . MESSAGE-SIZE) or
NIL.  Used in OPEN-QUEUE."
  `(if ,value
       (with-foreign-object (,var '(:struct mq-attr))
         (with-foreign-slots ((mq-maxmsg mq-msgsize) ,var (:struct mq-attr))
           (setf mq-maxmsg (car ,value) mq-msgsize (cdr ,value)))
         ,@body)
       (let ((,var (null-pointer)))
         ,@body)))

(defmethod expand-to-foreign-dyn (value var body (type mq-non-blocking-attr-type))
  "Translate NON-BLOCKING-P to MQ-ATTR dynamically.  On some implementations the
value can be allocated on stack.  It only sets MQ-FLAGS of MQ-ATTR since the
other fields are ignoret in the struct.  Used in SET-NON-BLOCKING."
  `(with-foreign-object (,var '(:struct mq-attr))
     (with-foreign-slots ((mq-flags) ,var (:struct mq-attr))
       (setf mq-flags (when ,value '(:non-blocking))))
     ,@body))

(defmethod expand-to-foreign (queue (type mqd-type))
  "This one is used to extract queue file descriptor from queue object."
  `(typecase ,queue
     (queue (mqd ,queue))
     (t ,queue)))
