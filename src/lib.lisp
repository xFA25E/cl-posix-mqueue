(defpackage posix-mqueue
  (:nicknames :mq)
  (:use :cl)
  (:shadow #:close)
  (:import-from #:cffi
                #:with-foreign-object
                #:with-foreign-objects
                #:translate-to-foreign
                #:expand-to-foreign
                #:with-pointer-to-vector-data
                #:with-foreign-slots)
  (:import-from #:mq.attr #:message-size)
  (:import-from #:mq.spec
                #:mq-open
                #:mq-receive
                #:mq-timedreceive
                #:mq-send
                #:mq-timedsend
                #:mq-getattr
                #:mq-setattr
                #:mq-close
                #:mq-unlink
                #:*errno*
                #:strerror
                #:timespec
                #:tv-sec
                #:tv-nsec
                ;; #:mqd-type
                )
  (:export #:make #:close #:unlink))
(in-package :posix-mqueue)

(defclass posix-mqueue ()
  ((name
    :initarg :name
    :reader name)
   (file-descriptor
    :initarg :file-descriptor
    :reader file-descriptor)
   (buffer
    :initarg :buffer
    :reader buffer)
   (closed-p
    :initform nil
    :accessor closed-p)))

(defun make (name &key (open-flags '(:read-only)) (mode '(:user-read :user-write))
                    max-messages message-size)
  (let* ((attributes (mq.attr:make :max-messages max-messages :message-size message-size))
         (file-descriptor (mq-open name open-flags mode attributes)))
    (if (or (= -1 file-descriptor) (= -1 (mq-getattr file-descriptor attributes)))
        (progn (print *errno*)
               (print (strerror *errno*)))
        (let ((buffer (make-array (message-size attributes)
                                  :element-type '(unsigned-byte 8))))
          (make-instance 'posix-mqueue
                         :name name
                         :file-descriptor file-descriptor
                         :buffer buffer)))))

(defun unlink (name)
  (when (= -1 (mq-unlink name))
    (print *errno*)
    (print (strerror *errno*))))

(defun close (posix-mqueue)
  (assert (not (closed-p posix-mqueue)))
  (if (= -1 (mq-close (file-descriptor posix-mqueue)))
      (progn (print *errno*)
             (print (strerror *errno*)))
      (setf (closed-p posix-mqueue) t)))

(defun attributes (posix-mqueue)
  (assert (not (closed-p posix-mqueue)))
  (let ((attributes (mq.attr:make)))
    (if (= -1 (mq-getattr (file-descriptor posix-mqueue) attributes))
        (progn (print *errno*)
               (print (strerror *errno*)))
        attributes)))

(defun (setf non-blocking) (posix-mqueue non-blocking)
  (assert (not (closed-p posix-mqueue)))
  (let ((attrs (mq.attr:make-set-non-blocking non-blocking)))
    (when (= -1 (mq-setattr (file-descriptor posix-mqueue) attrs (cffi:null-pointer)))
      (print *errno*)
      (print (strerror *errno*)))))

(defun receive (posix-mqueue)
  (assert (not (closed-p posix-mqueue)))
  (with-foreign-object (priority :uint)
    (with-pointer-to-vector-data (ptr (buffer posix-mqueue))
      (let ((length (mq-receive (file-descriptor posix-mqueue) ptr
                                (length (buffer posix-mqueue)) priority)))
        (if (= -1 length)
            (progn (print *errno*)
                   (print (strerror *errno*)))
            (values (subseq (buffer posix-mqueue) 0 length)
                    (cffi:mem-aref priority :uint)))))))

(defun timed-receive (posix-mqueue &key (seconds 0) (nano-seconds 0))
  (assert (not (closed-p posix-mqueue)))
  (assert (and (<= 0 seconds) (<= 0 1000000000)))
  (with-pointer-to-vector-data (ptr (buffer posix-mqueue))
    (with-foreign-objects ((tm '(:struct timespec)) (priority :uint))
      (with-foreign-slots ((tv-sec tv-nsec) tm (:struct timespec))
        (setf tv-sec seconds tv-nsec nano-seconds))
      (let ((length (mq-timedreceive (file-descriptor posix-mqueue) ptr
                                     (length (buffer posix-mqueue)) priority tm)))
        (if (= -1 length)
            (progn (print *errno*)
                   (print (strerror *errno*)))
            (values (subseq (buffer posix-mqueue) 0 length)
                    (cffi:mem-aref priority :uint)))))))

(defun send (posix-mqueue data &key (priority 0))
  (check-type data (simple-array (unsigned-byte 8)))
  (assert (not (closed-p posix-mqueue)))
  (with-pointer-to-vector-data (ptr data)
    (when (= -1 (mq-send (file-descriptor posix-mqueue) ptr (length data) priority))
      (print *errno*)
      (print (strerror *errno*)))))

(defun timed-send (posix-mqueue data &key (priority 0) (seconds 0) (nano-seconds 0))
  (check-type data (simple-array (unsigned-byte 8)))
  (assert (not (closed-p posix-mqueue)))
  (assert (and (<= 0 seconds) (<= 0 1000000000)))
  (with-pointer-to-vector-data (ptr data)
    (with-foreign-object (tm '(:struct timespec))
      (with-foreign-slots ((tv-sec tv-nsec) tm (:struct timespec))
        (setf tv-sec seconds tv-nsec nano-seconds))
      (when (= -1 (mq-timedsend (file-descriptor posix-mqueue) ptr (length data) priority tm))
        (print *errno*)
        (print (strerror *errno*))))))

;; (defmethod translate-to-foreign (posix-mqueue (type mqd-type))
;;   (slot-value posix-mqueue 'file-descriptor))

;; (defmethod expand-to-foreign (posix-mqueue (type mqd-type))
;;   `(slot-value ,posix-mqueue 'file-descriptor))
