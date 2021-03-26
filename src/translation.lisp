(in-package :posix-mqueue)

(defcvar "errno" :int "Global C variable used to get errors of functions.")

(defmethod translate-from-foreign (result (type result-type))
  "If  CFFI  function returns  -1,  then  return  keyword  representation of  error  code,
otherwise, return CFFI function's returned value."
  (declare (type integer result))
  (if (= -1 result)
      (case *errno*
        (2 :no-file-or-directory)       ; ENOENT
        (4 :interrupted-system-call)    ; EINTR
        (9  :bad-file-descriptor)       ; EBADF
        (11 :try-again)                 ; EAGAIN
        (12 :out-of-memory)             ; ENOMEM
        (13 :access-denied)             ; EACCES
        (17 :file-exists)               ; EEXIST
        (22 :invalid-argument)          ; EINVAL
        (23 :file-table-overflow)       ; ENFILE
        (24 :too-many-open-files)       ; EMFILE
        (28 :no-space-left-on-device)   ; ENOSPC
        (36 :name-too-long)             ; ENAMETOOLONG
        (90 :message-too-long)          ; EMSGSIZE
        (110 :connection-timed-out))    ; ETIMEDOUT
      result))

(defmethod expand-from-foreign (result (type result-type))
  "Look TRANSLATE-FROM-FOREIGN (result (type result-type))"
  (once-only (result)
    `(locally (declare (type integer ,result))
       (if (= -1 ,result)
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
           ,result))))

(defmethod translate-to-foreign (timestamp (type timespec-type))
  "Translate LOCAL-TIME:TIMESTAMP to MQ.SPEC.TYPES::MQ-TIMESPEC."
  (let ((ctimespec (foreign-alloc '(:struct timespec))))
    (declare (type foreign-pointer ctimespec))
    (with-foreign-slots ((tv-sec tv-nsec) ctimespec (:struct timespec))
      (setf tv-sec (timestamp-to-unix timestamp) tv-nsec (nsec-of timestamp)))
    ctimespec))

(defmethod free-translated-object (pointer (type timespec-type) param)
  "Free MQ.SPEC.TYPES:MQ-TIMESPEC."
  (declare (type foreign-pointer pointer) (ignore param))
  (foreign-free pointer))

(defmethod expand-to-foreign-dyn (value var body (type timespec-type))
  "Translate LOCAL-TIME:TIMESTAMP to  MQ.SPEC.TYPES::MQ-TIMESPEC dynamically.  On some
implementations the value can be allocated on stack."
  `(with-foreign-object (,var '(:struct timespec))
     (locally (declare (type foreign-pointer ,var))
       (with-foreign-slots ((tv-sec tv-nsec) ,var (:struct timespec))
         (setf tv-sec (timestamp-to-unix ,value) tv-nsec (nsec-of ,value)))
       ,@body)))

(defmethod translate-to-foreign (size-attributes (type mq-size-attr-type))
  "Translate  MQ.ATTR::ATTRIBUTES  to  MQ.SPEC.TYPES::MQ-ATTR.   If  MAX-MESSAGES  and
MESSAGE-SIZE are NULL, return NULL-POINTER"
  (declare (type (or null (cons (integer 1 4294967295)
                                (integer 1 4294967295)))
                 size-attributes))
  (if size-attributes
      (let ((cattributes (foreign-alloc '(:struct mq-attr))))
        (declare (type foreign-pointer cattributes))
        (with-foreign-slots ((mq-maxmsg mq-msgsize) cattributes (:struct mq-attr))
          (setf mq-maxmsg (car size-attributes)
                mq-msgsize (cdr size-attributes)))
        (values cattributes t))
      (values (null-pointer) nil)))

(defmethod free-translated-object (pointer (type mq-size-attr-type) freep)
  "Free MQ.SPEC.TYPES::MQ-ATTR."
  (declare (type foreign-pointer pointer) (type boolean freep))
  (when freep
    (foreign-free pointer)))

(defmethod expand-to-foreign-dyn (value var body (type mq-size-attr-type))
  "Translate  MQ.ATTR::ATTRIBUTES  to  MQ.SPEC.TYPES::MQ-ATTR  dynamically.   On  some
implementations the value can be allocated on stack."
  `(locally (declare (type (or null (cons (integer 1 4294967295)
                                          (integer 1 4294967295)))
                           ,value))
     (if ,value
         (with-foreign-object (,var '(:struct mq-attr))
           (locally (declare (type foreign-pointer ,var))
             (with-foreign-slots ((mq-maxmsg mq-msgsize) ,var (:struct mq-attr))
               (setf mq-maxmsg (car ,value) mq-msgsize (cdr ,value)))
             ,@body))
         (let ((,var (null-pointer)))
           (declare (type foreign-pointer ,var))
           ,@body))))

(defmethod translate-to-foreign (non-blocking-p (type mq-non-blocking-attr-type))
  "Translate  MQ.ATTR::ATTRIBUTES  to  MQ.SPEC.TYPES::MQ-ATTR.   If  MAX-MESSAGES  and
MESSAGE-SIZE are NULL, return NULL-POINTER"
  (declare (type boolean non-blocking-p))
  (let ((cattributes (foreign-alloc '(:struct mq-attr))))
    (declare (type foreign-pointer cattributes))
    (with-foreign-slots ((mq-flags) cattributes (:struct mq-attr))
      (setf mq-flags (when non-blocking-p '(:non-blocking))))
    cattributes))

(defmethod free-translated-object (pointer (type mq-non-blocking-attr-type) param)
  "Free MQ.SPEC.TYPES::MQ-ATTR."
  (declare (type foreign-pointer pointer) (ignore param))
  (foreign-free pointer))

(defmethod expand-to-foreign-dyn (value var body (type mq-non-blocking-attr-type))
  "Translate  MQ.ATTR::ATTRIBUTES  to  MQ.SPEC.TYPES::MQ-ATTR  dynamically.   On  some
implementations the value can be allocated on stack."
  `(locally (declare (type boolean ,value))
     (with-foreign-object (,var '(:struct mq-attr))
       (locally (declare (type foreign-pointer ,var))
         (with-foreign-slots ((mq-flags) ,var (:struct mq-attr))
           (setf mq-flags (when ,value '(:non-blocking))))
         ,@body))))

(defmethod translate-to-foreign (attributes (type mq-get-attr-type))
  "Alloc MQ.SPEC.TYPES::MQ-ATTR and  return a pointer to it. Return  ATTRIBUTES as the
second value."
  (declare (type attributes attributes))
  (values (foreign-alloc '(:struct mq-attr)) attributes))

(defmethod free-translated-object (pointer (type mq-get-attr-type) attributes)
  "At this point the MQ.SPEC.TYPES::MQ-ATTR  struct contains all the information about
queue.  We populate attributes with this information and free the pointer."
  (declare (type foreign-pointer pointer) (type attributes attributes))
  (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs) pointer (:struct mq-attr))
    (setf (non-blocking-p attributes) (when mq-flags t)
          (max-messages attributes) mq-maxmsg
          (message-size attributes) mq-msgsize
          (current-messages attributes) mq-curmsgs))
  (foreign-free pointer))

(defmethod expand-to-foreign-dyn (value var body (type mq-get-attr-type))
  "This is the same, but the pointer is freed for us."
  `(with-foreign-object (,var '(:struct mq-attr))
     (locally (declare (type foreign-pointer ,var) (type attributes ,value))
       (prog1 (progn ,@body)
         (with-foreign-slots ((mq-flags mq-maxmsg mq-msgsize mq-curmsgs) ,var (:struct mq-attr))
           (setf (non-blocking-p ,value) (when mq-flags t)
                 (max-messages ,value) mq-maxmsg
                 (message-size ,value) mq-msgsize
                 (current-messages ,value) mq-curmsgs))))))

(defmethod translate-to-foreign (queue (type mqd-type))
  "This one is used to extract queue file descriptor from queue object."
  (declare (type queue queue))
  (mqd queue))

(defmethod expand-to-foreign (queue (type mqd-type))
  "This one is used to extract queue file descriptor from queue object."
  `(locally (declare (type queue ,queue))
     (mqd ,queue)))
