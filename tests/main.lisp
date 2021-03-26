(defpackage :posix-mqueue.tests.main
  (:use :cl :rove))
(in-package :posix-mqueue.tests.main)

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

(deftest attributes
  (testing "Testing attributes error"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (posix-mqueue:close-queue mq)
      (ok (signals (posix-mqueue:attributes mq) 'posix-mqueue:bad-file-descriptor-invalid))
      (posix-mqueue:unlink name)))

  (testing "Testing normal attributes"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (ok (typep (posix-mqueue:attributes mq) 'posix-mqueue:attributes))
      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name)))

  (testing "Testing non-blocking attributes"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create :non-blocking))))
      (ok (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))
      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name))

    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (ng (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))
      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name)))

  (testing "Testing max/sizes attributes"
    (let* ((maxmsgs 10) (msgsize 20) (name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create)
                                             :max-messages maxmsgs :message-size msgsize))
           (attrs (posix-mqueue:attributes mq)))
      (ok (and (= maxmsgs (posix-mqueue:max-messages attrs))
               (= msgsize (posix-mqueue:message-size attrs))))
      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name)))

  (testing "Testing current messages attributes"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (ok (zerop (posix-mqueue:current-messages (posix-mqueue:attributes mq))))
      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name))))

(deftest open-queue
  (testing "Make errors"
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
              'posix-mqueue:no-file-or-directory-no-create)))))

(deftest non-blocking
  (testing "Non blocking error"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (posix-mqueue:close-queue mq)
      (ok (signals (posix-mqueue:set-non-blocking mq t)
              'posix-mqueue:bad-file-descriptor-invalid))
      (posix-mqueue:unlink name)))

  (testing "Non blocking normal"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create))))
      (ng (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))
      (ok (posix-mqueue:set-non-blocking mq t))
      (ok (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))
      (ng (posix-mqueue:set-non-blocking mq nil))
      (ng (posix-mqueue:non-blocking-p (posix-mqueue:attributes mq)))
      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name))))

(deftest send
  (testing "Send errors"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-only :create)
                                             :max-messages 1 :message-size 5)))
      (ok (signals (posix-mqueue:send-string mq "hei")
              'posix-mqueue:bad-file-descriptor-on-send))
      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name))

    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-write :create)
                                             :max-messages 1 :message-size 5)))
      (ok (signals (posix-mqueue:send-string mq "helloo")
              'posix-mqueue:message-too-long-on-send))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec -1)))
        (ok (signals (posix-mqueue:timed-send-string mq "hei" :timestamp invalid-timestamp)
                'posix-mqueue:invalid-argument-on-send-receive)))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec (1+ (expt 10 9)))))
        (ok (signals (posix-mqueue:timed-send-string mq "hei" :timestamp invalid-timestamp)
                'posix-mqueue:invalid-argument-on-send-receive)))
      (posix-mqueue:close-queue mq)
      (ok (signals (posix-mqueue:send-string mq "hei")
              'posix-mqueue:bad-file-descriptor-on-send))
      (posix-mqueue:unlink name)))

  (testing "normal send"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-write :create)
                                             :max-messages 1 :message-size 5))
           (msg "hei")
           (prty 5))
      (ng (posix-mqueue:send-string mq msg :priority prty))
      (multiple-value-bind (m p) (posix-mqueue:receive-string mq)
        (ok (and (= p prty) (string= m msg))))

      (ng (posix-mqueue:send-string mq msg :priority prty))
      (ok (eq :connection-timed-out (posix-mqueue:timed-send-string mq msg :priority prty)))

      (posix-mqueue:set-non-blocking mq t)
      (ok (eq :try-again (posix-mqueue:send-string mq msg :priority prty)))

      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name))))

(deftest receive
  (testing "Error receive"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:write-only :create)
                                             :max-messages 1 :message-size 5)))
      (ok (signals (posix-mqueue:receive-string mq) 'posix-mqueue:bad-file-descriptor-on-receive))
      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name))

    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-write :create)
                                             :max-messages 1 :message-size 5)))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec -1)))
        (ok (signals (posix-mqueue:timed-receive-string mq :timestamp invalid-timestamp)
                'posix-mqueue:invalid-argument-on-send-receive)))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec (1+ (expt 10 9)))))
        (ok (signals (posix-mqueue:timed-receive-string mq :timestamp invalid-timestamp)
                'posix-mqueue:invalid-argument-on-send-receive)))
      (posix-mqueue:close-queue mq)
      (ok (signals (posix-mqueue:receive-string mq)
              'posix-mqueue:bad-file-descriptor-on-receive))
      (posix-mqueue:unlink name)))

  (testing "Normal receive"
    (let* ((name (posix-mqueue::random-queue-name))
           (mq (posix-mqueue:open-queue name :open-flags '(:read-write :create)
                                             :max-messages 1 :message-size 5))
           (msg "hei")
           (prty 5))
      (ng (posix-mqueue:send-string mq msg :priority prty))
      (multiple-value-bind (m p) (posix-mqueue:receive-string mq)
        (ok (and (= p prty) (string= m msg))))

      (ok (eq :connection-timed-out (posix-mqueue:timed-receive-string mq)))

      (posix-mqueue:set-non-blocking mq t)
      (ok (eq :try-again (posix-mqueue:receive-string mq)))

      (posix-mqueue:close-queue mq)
      (posix-mqueue:unlink name))))

(deftest with-queue
  (ok (expands
       '(posix-mqueue:with-open-queue (mq "/hello" :open-flags '(:read-write :create))
         (posix-mqueue:send-string mq "hello"))
       '(let ((mq (posix-mqueue:open-queue "/hello" :open-flags '(:read-write :create))))
         (declare (type posix-mqueue:queue mq))
         (unwind-protect (progn (posix-mqueue:send-string mq "hello"))
           (posix-mqueue:close-queue mq))))))
