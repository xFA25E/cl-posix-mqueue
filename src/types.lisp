(in-package :posix-mqueue)

(defctype time-t :long "Type used to describe seconds since unix epoch")

(define-foreign-type mqd-type ()
  ()
  (:actual-type :int)
  (:simple-parser mqd)
  (:documentation
   "Type used to describe POSIX message queue file descriptor.  Also, there are
translations defined for this type (:int) from QUEUE class."))

(define-foreign-type result-type ()
  ()
  (:actual-type :int)
  (:simple-parser result)
  (:documentation
   "Type used to describe C-style result of functions.  There is a translation
that maps -1 to keyword representation of the error through the errno."))

(define-foreign-type mq-size-attr-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-size-attr-t)
  (:documentation
   "Type used to pass ATTRIBUTES as C-function argument.  Translation maps
ATTRIBUTES to MQ-ATTR CStruct."))

(define-foreign-type mq-non-blocking-attr-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser mq-non-blocking-attr-t)
  (:documentation
   "Type used to get attributes through a pointer.  To fill a CStruct through a
pointer passed to function.  Translation for this type does exactly this, at the
end of the function call, it fills Lisp class with values from MQ-ATTR."))

(define-foreign-type timespec-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser timespec-t)
  (:documentation "Type used to pass LOCAL-TIME:TIMESTAMP as C timespec."))

(defbitfield oflag
  "OPEN-FLAGS bitfield, used at opening (or creating) a queue with MAKE."
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
  "Flag used in ATTRIBUTES when retrieving queue attributes with mq-getattr."
  (:non-blocking #o4000))

(defcstruct mq-attr
  "CStruct with POSIX message queue attributes. MQ-FLAGS can only contain a
non-blocking flag."
  (mq-flags mq-flags)
  (mq-maxmsg :long)
  (mq-msgsize :long)
  (mq-curmsgs :long))

(defcstruct timespec
  "Timespec CStruct that specifies time in seconds and nanosecands."
  (tv-sec time-t)
  (tv-nsec :long))

(defun open-flagsp (thing)
  "Check if THING is a list and contains only OFLAGs.  Also, check that
single-flags are present only once."
  (let ((all-flags (cons :read-only (foreign-bitfield-symbol-list 'oflag)))
        (single-flags '(:read-only :write-only :read-write)))
    (and (listp thing)
         (null (set-difference thing all-flags))
         (= 1 (count single-flags thing :test (lambda (a b) (member b a)))))))

(deftype open-flags ()
  "Type used to describe OPEN-FLAGS in OPEN-QUEUE."
  '(and list (satisfies open-flagsp)))

(defun create-modesp (thing)
  "Check if THING is a list and contains only MODEs."
  (let ((modes (foreign-bitfield-symbol-list 'mode)))
    (and (listp thing) (null (set-difference thing modes)))))

(deftype create-modes ()
  "Type used to describe CREATE-MODES in OPEN-QUEUE."
  '(and list (satisfies create-modesp)))
