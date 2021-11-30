(defpackage #:posix-mqueue.tests
  (:use #:cl #:fiveam))
(in-package #:posix-mqueue.tests)

(defmacro with-random-queue ((var random-params &rest options) &body body)
  "Create a random name queue, close and unlink it in the end."
  (let ((name (gensym "NAME")))
    `(let* ((,name (posix-mqueue::random-queue-name ,@random-params))
            (,var (posix-mqueue:open-queue ,name ,@options)))
       (unwind-protect (progn ,@body)
         (ignore-errors
          (posix-mqueue:unlink ,name)
          (posix-mqueue:close-queue ,var))))))

(def-suite posix-mqueue
  :description "Test posix-mqueue")

(def-suite* random-queue-name :in posix-mqueue)

(test random-queue-name-negative-length
  (signals type-error (posix-mqueue::random-queue-name :length (- (random 20)))))

(test random-queue-name-normal-usage
  (let ((name (posix-mqueue::random-queue-name :length 20 :start 97 :end 123)))
    (is (and (= 21 (length name)) (cl-ppcre:scan "^/[a-z]+$" name)))))

(def-suite* unlink :in posix-mqueue)

(test unlink-errors
  (signals posix-mqueue:name-too-long
      (posix-mqueue:unlink (posix-mqueue::random-queue-name :length 256)))
  (signals posix-mqueue:no-file-or-directory-on-unlink
      (posix-mqueue:unlink (posix-mqueue::random-queue-name))))

(test unlink-normal-usage
  (let ((name (posix-mqueue::random-queue-name)))
    (posix-mqueue:close-queue (posix-mqueue:open-queue name :open-flags '(:read-only :create)))
    (finishes (posix-mqueue:unlink name))))

(def-suite* close-queue :in posix-mqueue)

(test close-queue-error
  (let* ((name (posix-mqueue::random-queue-name))
         (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
    (posix-mqueue:close-queue mq)
    (signals posix-mqueue:bad-file-descriptor-invalid (posix-mqueue:close-queue mq))
    (posix-mqueue:unlink name)))

(test close-queue-normal-usage
  (let* ((name (posix-mqueue::random-queue-name))
         (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
    (finishes (posix-mqueue:close-queue mq))
    (posix-mqueue:unlink name)))

(def-suite* with-open-queue :in posix-mqueue)

(test with-open-queue-normal-usage
  (is
   (equal
    (macroexpand-1
     '(posix-mqueue:with-open-queue (mq "/hello" :open-flags '(:read-write :create))
       (posix-mqueue:send-string mq "hello" 0)))
    '(let ((mq (posix-mqueue:open-queue "/hello" :open-flags '(:read-write :create))))
      (unwind-protect (progn (posix-mqueue:send-string mq "hello" 0))
        (posix-mqueue:close-queue mq))))))

(def-suite* attributes :in posix-mqueue)

(test attributes-error
  (let ((q nil))
    (with-random-queue (mq nil :open-flags '(:read-only :create))
      (setf q mq))
    (signals posix-mqueue:bad-file-descriptor-invalid (posix-mqueue:attributes q))))

(test attributes-normal-usage
  (with-random-queue (mq nil :open-flags '(:read-only :create))
    (is (typep (posix-mqueue:attributes mq) 'posix-mqueue:attributes))))

(test attributes-non-blocking
  (with-random-queue (mq nil :open-flags '(:read-only :create :non-blocking))
    (is (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq))))

  (with-random-queue (mq nil :open-flags '(:read-only :create))
    (is-false (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))))

(test attributes-maxmsgs-msgsize
  (let ((maxmsgs 10) (msgsize 20))
    (with-random-queue (mq nil :open-flags '(:read-only :create)
                               :max-messages maxmsgs :message-size msgsize)
      (let ((attrs (posix-mqueue:attributes mq)))
        (is (= maxmsgs (posix-mqueue:max-messages attrs)))
        (is (= msgsize (posix-mqueue:message-size attrs)))))))

(test attributes-current-messages
  (with-random-queue (mq nil :open-flags '(:read-only :create))
    (is (zerop (posix-mqueue:current-messages (posix-mqueue:attributes mq))))))

(def-suite* open-queue :in posix-mqueue)

(test open-queue-errors
  (let ((name (concatenate 'string (posix-mqueue::random-queue-name) (posix-mqueue::random-queue-name))))
    (signals posix-mqueue:access-denied-slashes
        (posix-mqueue:open-queue name :open-flags '(:read-only :create))))

  (let ((name (posix-mqueue::random-queue-name)))
    (posix-mqueue:close-queue
     (posix-mqueue:open-queue name :open-flags '(:read-only :create) :create-modes '()))
    (signals posix-mqueue:access-denied-permission
        (posix-mqueue:open-queue name :open-flags '(:read-write)))
    (posix-mqueue:unlink name))

  (let ((name (posix-mqueue::random-queue-name)))
    (finishes (posix-mqueue:close-queue (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
    (signals posix-mqueue:file-exists
        (posix-mqueue:open-queue name :open-flags '(:read-only :create :exclusive)))
    (finishes (posix-mqueue:unlink name)))

  (let ((name (subseq (posix-mqueue::random-queue-name) 1)))
    (signals posix-mqueue:invalid-argument-name
        (posix-mqueue:open-queue name :open-flags '(:read-only :create))))

  (let ((name (posix-mqueue::random-queue-name)))
    (signals type-error
        (posix-mqueue:open-queue name :open-flags '(:read-only :create) :max-messages 0 :message-size 0)))

  (let ((name (posix-mqueue::random-queue-name :length 300)))
    (signals posix-mqueue:name-too-long
        (posix-mqueue:open-queue name :open-flags '(:read-only :create))))

  (signals posix-mqueue:no-file-or-directory-just-slash
      (posix-mqueue:open-queue "/" :open-flags '(:read-only :create)))

  (let ((name (posix-mqueue::random-queue-name)))
    (signals posix-mqueue:no-file-or-directory-no-create
        (posix-mqueue:open-queue name :open-flags '(:read-only))))

  (let ((name (posix-mqueue::random-queue-name)))
    (signals type-error (posix-mqueue:open-queue name :open-flags '(:bruh))))

  (let ((name (posix-mqueue::random-queue-name)))
    (signals type-error
        (posix-mqueue:open-queue name :open-flags '(:create) :create-modes '(:bruh)))))

(test attributes-default
  (let ((max-messages 5) (message-size 30))
    (destructuring-bind (default-max-messages . default-message-size) (posix-mqueue:default-sizes)
      (with-random-queue (mq nil :open-flags '(:read-only :create)
                                 :max-messages max-messages
                                 :message-size default-message-size)
        (let ((attr (posix-mqueue:attributes mq)))
          (is (= (posix-mqueue:message-size attr) default-message-size))
          (is (= (posix-mqueue:max-messages attr) max-messages))))

      (with-random-queue (mq nil :open-flags '(:read-only :create)
                                 :message-size message-size
                                 :max-messages default-max-messages)
        (let ((attr (posix-mqueue:attributes mq)))
          (is (= (posix-mqueue:message-size attr) message-size))
          (is (= (posix-mqueue:max-messages attr) default-max-messages)))))))

(test open-queue-existing
  (let* ((name (posix-mqueue::random-queue-name))
         (max-messages 5) (message-size 20)
         (orig-mq (posix-mqueue:open-queue name :open-flags '(:create :read-only)
                                      :max-messages max-messages
                                      :message-size message-size)))
    (unwind-protect
         (posix-mqueue:with-open-queue (open-mq name :open-flags '(:write-only))
           (let ((orig-attrs (posix-mqueue:attributes orig-mq))
                 (open-attrs (posix-mqueue:attributes open-mq)))
             (is (= (posix-mqueue:message-size orig-attrs)
                    (posix-mqueue:message-size open-attrs)
                    message-size))
             (is (= (posix-mqueue:max-messages orig-attrs)
                    (posix-mqueue:max-messages open-attrs)
                    max-messages))
             (is (= (length (posix-mqueue:buffer orig-mq))
                    (length (posix-mqueue:buffer open-mq))
                    message-size))))
      (ignore-errors
       (posix-mqueue:unlink name)
       (posix-mqueue:close-queue orig-mq)))))

(def-suite* non-blocking :in posix-mqueue)

(test non-blocking-error
  (with-random-queue (mq nil :open-flags '(:read-only :create))
    (posix-mqueue:close-queue mq)
    (signals posix-mqueue:bad-file-descriptor-invalid (posix-mqueue:set-non-blocking mq t))))

(test non-blocking-normal-usage
  (with-random-queue (mq nil :open-flags '(:read-only :create))
    (is-false (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))

    (is (posix-mqueue:set-non-blocking mq t))
    (is (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))

    (is-false (posix-mqueue:set-non-blocking mq nil))
    (is-false (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))))

(def-suite* send :in posix-mqueue)

(test send-errors
  (with-random-queue (mq nil :open-flags '(:read-only :create) :max-messages 1 :message-size 5)
    (signals posix-mqueue:bad-file-descriptor-on-send (posix-mqueue:send-string mq "hei" 0)))

  (with-random-queue (mq nil :open-flags '(:read-write :create) :max-messages 1 :message-size 5)
    (signals posix-mqueue:message-too-long-on-send (posix-mqueue:send-string mq "helloo" 0))

    (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec #-ccl -1 #+ccl 0)))
      (signals posix-mqueue:invalid-argument-on-send-receive
          (posix-mqueue:timed-send-string mq "hei" 0 invalid-timestamp)))

    (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec #-ccl (1+ (expt 10 9)) #+ccl 0)))
      (signals posix-mqueue:invalid-argument-on-send-receive
          (posix-mqueue:timed-send-string mq "hei" 0 invalid-timestamp)))

    (ignore-errors (posix-mqueue:close-queue mq))
    (signals posix-mqueue:bad-file-descriptor-on-send (posix-mqueue:send-string mq "hei" 0))))

(test send-normal-usage
  (let ((msg "hei") (prty 5))
    (with-random-queue (mq nil :open-flags '(:read-write :create) :max-messages 1 :message-size 5)
      (finishes (posix-mqueue:send-string mq msg prty))

      (multiple-value-bind (m p) (posix-mqueue:receive-string mq)
        (is (= p prty))
        (is (string= m msg)))

      (finishes (posix-mqueue:send-string mq msg prty))
      (is (eq :connection-timed-out (posix-mqueue:timed-send-string mq msg prty (local-time:now))))

      (posix-mqueue:set-non-blocking mq t)
      (is (eq :try-again (posix-mqueue:send-string mq msg prty))))))

(def-suite* receive :in posix-mqueue)

(test receive-error
  (with-random-queue (mq nil :open-flags '(:write-only :create) :max-messages 1 :message-size 5)
    (signals posix-mqueue:bad-file-descriptor-on-receive (posix-mqueue:receive-string mq)))

  (with-random-queue (mq nil :open-flags '(:read-write :create) :max-messages 1 :message-size 5)
    (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec #-ccl -1 #+ccl 0)))
      (signals posix-mqueue:invalid-argument-on-send-receive
          (posix-mqueue:timed-receive-string mq invalid-timestamp)))

    (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec #-ccl (1+ (expt 10 9)) #+ccl 0)))
      (signals posix-mqueue:invalid-argument-on-send-receive
          (posix-mqueue:timed-receive-string mq invalid-timestamp)))

    (ignore-errors (posix-mqueue:close-queue mq))
    (signals posix-mqueue:bad-file-descriptor-on-receive (posix-mqueue:receive-string mq))))

(test receive-normal-usage
  (let ((msg "hei") (prty 5))
    (with-random-queue (mq nil :open-flags '(:read-write :create) :max-messages 1 :message-size 5)
      (finishes (posix-mqueue:send-string mq msg prty))

      (multiple-value-bind (m p) (posix-mqueue:receive-string mq)
        (is (= p prty))
        (is (string= m msg)))

      (is (eq :connection-timed-out (posix-mqueue:timed-receive-string mq (local-time:now))))

      (posix-mqueue:set-non-blocking mq t)
      (is (eq :try-again (posix-mqueue:receive-string mq))))))
