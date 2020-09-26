(defpackage posix-mqueue.spec.types
  (:nicknames :mq.spec.types)
  (:use :cl)
  (:import-from #:cffi
                #:defbitfield
                #:defcstruct
                #:defctype
                #:define-foreign-type
                #:foreign-bitfield-symbol-list))
(in-package :posix-mqueue.spec.types)

(defctype time-t :long "Type used to describe seconds since unix epoch")

(define-foreign-type mqd-type ()
  ()
  (:actual-type :int)
  (:simple-parser mqd)
  (:documentation
   "Type used to describe POSIX message  queue file descriptor.  Also, there are
translations defined for this type (:int) from MQ.QUEUE::QUEUE class."))

(define-foreign-type result-type ()
  ()
  (:actual-type :int)
  (:simple-parser result)
  (:documentation
   "Type used to  describe C-style result of functions.  There  is a translation
that maps -1 to keyword representation of the error through the errno."))

(define-foreign-type mq-attr-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-attr-t)
  (:documentation
   "Type used  to pass MQ.ATTR::ATTRIBUTES as  C-function argument.  Translation
maps MQ.ATTR::ATTRIBUTES to MQ-ATTR CStruct."))

(define-foreign-type mq-attr-get-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-attr-get-t)
  (:documentation
   "Type used to get attributes through a  pointer.  To fill a CStruct through a
pointer passed to function.  Translation for this type does exactly this, at the
end of the function call, it fills Lisp class with values from MQ-ATTR."))

(define-foreign-type timespec-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser timespec-t)
  (:documentation "Type used to pass LOCAL-TIME:TIMESTAMP as C timespec."))

(defbitfield oflag
  "OPEN-FLAGS bitfield, used at opening (or creating) a queue with MQ:MAKE."
  (:read-only     #o0)
  (:write-only    #o1)
  (:read-write    #o2)
  (:close-on-exec #o2000000)
  (:create        #o100)
  (:exclusive     #o200)
  (:non-blocking  #o4000))

(defbitfield mode
  "MODE bitfield, used at queue creation."
  (:user-write  #o200)
  (:user-read   #o400)
  (:group-write #o020)
  (:group-read  #o040)
  (:other-write #o002)
  (:other-read  #o004))

(defbitfield (mq-flags :long)
  "Flag  used  in  MQ.ATTR::ATTRIBUTES  when retrieving  queue  attributes  with
mq-getattr."
  (:non-blocking #o4000))

(defcstruct mq-attr
  "CStruct  with POSIX  message queue  attributes. MQ-FLAGS  can only  contain a
non-blocking flag."
  (mq-flags mq-flags)
  (mq-maxmsg :long)
  (mq-msgsize :long)
  (mq-curmsgs :long))

(defcstruct timespec
  "Timespec CStruct that specifies time in seconds and nanosecands."
  (tv-sec time-t)
  (tv-nsec :long))

(defun oflagsp (thing)
  (let ((flags '(:close-on-exec :create :exclusive :non-blocking))
        (single-flags '(:read-only :write-only :read-write))
        (result nil))
    (and (listp thing)
         (every (lambda (open-flag)
                  (if (member open-flag single-flags :test #'eq)
                      (push open-flag result)
                      (member open-flag flags :test #'eq)))
                (remove-duplicates thing :test #'eq))
         (= 1 (length result)))))

(deftype oflags ()
  '(and list (satisfies oflagsp)))

(defun modep (thing)
  (let ((modes (foreign-bitfield-symbol-list 'mode)))
    (and (listp thing)
         (every (lambda (mode) (member mode modes :test #'eq)) thing))))

(deftype mode ()
  '(and list (satisfies modep)))
