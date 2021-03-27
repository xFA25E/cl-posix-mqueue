(defpackage :posix-mqueue.tests.main
  (:use :cl :rove))
(in-package :posix-mqueue.tests.main)

(defmacro with-random-queue ((var random-params &rest options) &body body)
  "Create a random name queue, close and unlink it in the end."
  (let ((name (gensym "NAME")))
    `(let* ((,name (posix-mqueue::random-queue-name ,@random-params))
            (,var (posix-mqueue:open-queue ,name ,@options)))
       (unwind-protect (progn ,@body)
         (ignore-errors
          (posix-mqueue:unlink ,name)
          (posix-mqueue:close-queue ,var))))))

(deftest random-name-generation
  (testing "Testing assert posix-mqueue::random-queue-name"
    (ok (signals (posix-mqueue::random-queue-name :length (- (random 20))))))

  (testing "Testing noraml posix-mqueue::random-queue-name"
    (let ((name (posix-mqueue::random-queue-name :length 20 :start 97 :end 123)))
      (ok (and (= 21 (length name)) (ppcre:scan "^/[a-z]+$" name))))))

(deftest unlink
  (testing "Testing unlink errors"
    (ok (signals (posix-mqueue:unlink (posix-mqueue::random-queue-name :length 256)) 'posix-mqueue:name-too-long))
    (ok (signals (posix-mqueue:unlink (posix-mqueue::random-queue-name)) 'posix-mqueue:no-file-or-directory-on-unlink)))

  (testing "Testing normal unlink"
    (let ((name (posix-mqueue::random-queue-name)))
      (posix-mqueue:close-queue (posix-mqueue:open-queue name :open-flags '(:read-only :create)))
      (ng (posix-mqueue:unlink name)))))

(deftest close-queue
  (testing "Testing close error"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (posix-mqueue:close-queue mq)
      (ok (signals (posix-mqueue:close-queue mq) 'posix-mqueue:bad-file-descriptor-invalid))
      (posix-mqueue:unlink name)))

  (testing "Testing normal close"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (ng (posix-mqueue:close-queue mq))
      (posix-mqueue:unlink name))))

(deftest with-open-queue
  (ok (expands
       '(posix-mqueue:with-open-queue (mq "/hello" :open-flags '(:read-write :create))
         (posix-mqueue:send-string mq "hello" 0))
       '(let ((mq (posix-mqueue:open-queue "/hello" :open-flags '(:read-write :create))))
         (unwind-protect (progn (posix-mqueue:send-string mq "hello" 0))
           (posix-mqueue:close-queue mq))))))

(deftest attributes
  (testing "Testing attributes error"
    (let ((q nil))
      (with-random-queue (mq nil :open-flags '(:read-only :create))
        (setf q mq))
      (ok (signals (posix-mqueue:attributes q) 'posix-mqueue:bad-file-descriptor-invalid))))

  (testing "Testing normal attributes"
    (with-random-queue (mq nil :open-flags '(:read-only :create))
      (ok (typep (posix-mqueue:attributes mq) 'posix-mqueue:attributes))))

  (testing "Testing non-blocking attributes"
    (with-random-queue (mq nil :open-flags '(:read-only :create :non-blocking))
      (ok (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq))))

    (with-random-queue (mq nil :open-flags '(:read-only :create))
      (ng (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))))

  (testing "Testing max/sizes attributes"
    (let ((maxmsgs 10) (msgsize 20))
      (with-random-queue (mq nil :open-flags '(:read-only :create)
                                 :max-messages maxmsgs :message-size msgsize)
        (let ((attrs (posix-mqueue:attributes mq)))
          (ok (and (= maxmsgs (posix-mqueue:max-messages attrs))
                   (= msgsize (posix-mqueue:message-size attrs))))))))

  (testing "Testing current messages attributes"
    (with-random-queue (mq nil :open-flags '(:read-only :create))
      (ok (zerop (posix-mqueue:current-messages (posix-mqueue:attributes mq)))))))

(deftest open-queue
  (testing "Open errors"
    (let ((name (concatenate 'string (posix-mqueue::random-queue-name) (posix-mqueue::random-queue-name))))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:read-only :create)) 'posix-mqueue:access-denied-slashes)))

    (let ((name (posix-mqueue::random-queue-name)))
      (posix-mqueue:close-queue (posix-mqueue:open-queue name :open-flags '(:read-only :create) :create-modes '()))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:read-write)) 'posix-mqueue:access-denied-permission))
      (posix-mqueue:unlink name))

    (let ((name (posix-mqueue::random-queue-name)))
      (ng (posix-mqueue:close-queue (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:read-only :create :exclusive))
              'posix-mqueue:file-exists))
      (ng (posix-mqueue:unlink name)))

    (let ((name (subseq (posix-mqueue::random-queue-name) 1)))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:read-only :create))
              'posix-mqueue:invalid-argument-name)))

    (let ((name (posix-mqueue::random-queue-name)))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:read-only :create)
                                                 :max-messages 0 :message-size 0))))

    (let ((name (posix-mqueue::random-queue-name :length 300)))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:read-only :create))
              'posix-mqueue:name-too-long)))

    (ok (signals (posix-mqueue:open-queue "/" :open-flags '(:read-only :create))
            'posix-mqueue:no-file-or-directory-just-slash))

    (let ((name (posix-mqueue::random-queue-name)))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:read-only))
              'posix-mqueue:no-file-or-directory-no-create)))

    (let ((name (posix-mqueue::random-queue-name)))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:baram)) 'simple-error)))

    (let ((name (posix-mqueue::random-queue-name)))
      (ok (signals (posix-mqueue:open-queue name :open-flags '(:create)
                                                 :create-modes '(:burum))
              'simple-error))))

  (testing "Default attributes"
    (let ((max-messages 5) (message-size 30))
      (destructuring-bind (default-max-messages . default-message-size) (posix-mqueue::default-sizes)

        (with-random-queue (mq nil :open-flags '(:read-only :create)
                                   :max-messages max-messages)
          (let ((attr (posix-mqueue:attributes mq)))
            (ok (= (posix-mqueue:message-size attr) default-message-size))
            (ok (= (posix-mqueue:max-messages attr) max-messages))))

        (with-random-queue (mq nil :open-flags '(:read-only :create)
                                   :message-size message-size)
          (let ((attr (posix-mqueue:attributes mq)))
            (ok (= (posix-mqueue:message-size attr) message-size))
            (ok (= (posix-mqueue:max-messages attr) default-max-messages))))))))

(deftest non-blocking
  (testing "Non blocking error"
    (with-random-queue (mq nil :open-flags '(:read-only :create))
      (posix-mqueue:close-queue mq)
      (ok (signals (posix-mqueue:set-non-blocking mq t)
              'posix-mqueue:bad-file-descriptor-invalid))))

  (testing "Non blocking normal"
    (with-random-queue (mq nil :open-flags '(:read-only :create))
      (ng (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))
      (ok (posix-mqueue:set-non-blocking mq t))
      (ok (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))
      (ng (posix-mqueue:set-non-blocking mq nil))
      (ng (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq))))))

(deftest send
  (testing "Send errors"
    (with-random-queue (mq nil :open-flags '(:read-only :create) :max-messages 1 :message-size 5)
      (ok (signals (posix-mqueue:send-string mq "hei" 0)
              'posix-mqueue:bad-file-descriptor-on-send)))

    (with-random-queue (mq nil :open-flags '(:read-write :create) :max-messages 1 :message-size 5)
      (ok (signals (posix-mqueue:send-string mq "helloo" 0)
              'posix-mqueue:message-too-long-on-send))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec -1)))
        (ok (signals (posix-mqueue:timed-send-string mq "hei" 0 invalid-timestamp)
                'posix-mqueue:invalid-argument-on-send-receive)))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec (1+ (expt 10 9)))))
        (ok (signals (posix-mqueue:timed-send-string mq "hei" 0 invalid-timestamp)
                'posix-mqueue:invalid-argument-on-send-receive)))
      (ignore-errors (posix-mqueue:close-queue mq))
      (ok (signals (posix-mqueue:send-string mq "hei" 0)
              'posix-mqueue:bad-file-descriptor-on-send))))

  (testing "normal send"
    (let ((msg "hei") (prty 5))
      (with-random-queue (mq nil :open-flags '(:read-write :create) :max-messages 1 :message-size 5)
        (ng (posix-mqueue:send-string mq msg prty))
        (multiple-value-bind (m p) (posix-mqueue:receive-string mq)
          (ok (and (= p prty) (string= m msg))))

        (ng (posix-mqueue:send-string mq msg prty))
        (ok (eq :connection-timed-out (posix-mqueue:timed-send-string mq msg prty (local-time:now))))

        (posix-mqueue:set-non-blocking mq t)
        (ok (eq :try-again (posix-mqueue:send-string mq msg prty)))))))

(deftest receive
  (testing "Error receive"
    (with-random-queue (mq nil :open-flags '(:write-only :create) :max-messages 1 :message-size 5)
      (ok (signals (posix-mqueue:receive-string mq)
              'posix-mqueue:bad-file-descriptor-on-receive)))

    (with-random-queue (mq nil :open-flags '(:read-write :create) :max-messages 1 :message-size 5)
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec -1)))
        (ok (signals (posix-mqueue:timed-receive-string mq invalid-timestamp)
                'posix-mqueue:invalid-argument-on-send-receive)))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec (1+ (expt 10 9)))))
        (ok (signals (posix-mqueue:timed-receive-string mq invalid-timestamp)
                'posix-mqueue:invalid-argument-on-send-receive)))
      (ignore-errors (posix-mqueue:close-queue mq))
      (ok (signals (posix-mqueue:receive-string mq)
              'posix-mqueue:bad-file-descriptor-on-receive))))

  (testing "Normal receive"
    (let ((msg "hei") (prty 5))
      (with-random-queue (mq nil :open-flags '(:read-write :create) :max-messages 1 :message-size 5)
        (ng (posix-mqueue:send-string mq msg prty))
        (multiple-value-bind (m p) (posix-mqueue:receive-string mq)
          (ok (and (= p prty) (string= m msg))))

        (ok (eq :connection-timed-out (posix-mqueue:timed-receive-string mq (local-time:now))))

        (posix-mqueue:set-non-blocking mq t)
        (ok (eq :try-again (posix-mqueue:receive-string mq)))))))
