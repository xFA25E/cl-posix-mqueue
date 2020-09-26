(defpackage posix-mqueue/tests/main
  (:use #:cl #:rove))
(in-package :posix-mqueue/tests/main)

(defun random-name (&key (length 25) (start 97) (end 123))
  (assert (plusp length))
  (let ((result (make-string (1+ length))))
    (setf (aref result 0) #\/)
    (loop :for i :from 1 :to length
          :for new-code = (+ start (random (- end start)))
          :do (setf (aref result i) (code-char new-code)))
    result))

(deftest random-name-generation
  (testing "Testing assert random-name"
    (ok (signals (random-name :length (- (random 20))))))

  (testing "Testing noraml random-name"
    (let ((name (random-name :length 20 :start 97 :end 123)))
      (ok (and (= 21 (length name)) (ppcre:scan "^/[a-z]+$" name))))))

(deftest unlink
  (testing "Testing unlink errors"
    (ok (signals (mq:unlink (random-name :length 300)) 'mq.cond:name-too-long))
    (ok (signals (mq:unlink (random-name)) 'mq.cond:no-file-or-directory)))

  (testing "Testing normal unlink"
    (let ((name (random-name)))
      (mq:close (mq:make name :open-flags '(:read-only :create)))
      (ng (mq:unlink name)))))

(deftest close
  (testing "Testing close error"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create))))
      (mq:close mq)
      (ok (signals (mq:close mq) 'mq.cond:bad-file-descriptor))
      (mq:unlink name)))

  (testing "Testing normal close"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create))))
      (ng (mq:close mq))
      (mq:unlink name))))

(deftest attributes
  (testing "Testing attributes error"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create))))
      (mq:close mq)
      (ok (signals (mq:attributes mq) 'mq.cond:bad-file-descriptor))
      (mq:unlink name)))

  (testing "Testing normal attributes"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create))))
      (ok (typep (mq:attributes mq) 'mq.attr::attributes))
      (mq:close mq)
      (mq:unlink name)))

  (testing "Testing non-blocking attributes"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create :non-blocking))))
      (ok (mq.attr:non-blocking-p (mq:attributes mq)))
      (mq:close mq)
      (mq:unlink name))

    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create))))
      (ng (mq.attr:non-blocking-p (mq:attributes mq)))
      (mq:close mq)
      (mq:unlink name)))

  (testing "Testing max/sizes attributes"
    (let* ((maxmsgs 10) (msgsize 20) (name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create)
                             :max-messages maxmsgs :message-size msgsize))
           (attrs (mq:attributes mq)))
      (ok (and (= maxmsgs (mq.attr:max-messages attrs))
               (= msgsize (mq.attr:message-size attrs))))
      (mq:close mq)
      (mq:unlink name)))

  (testing "Testing current messages attributes"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create))))
      (ok (zerop (mq.attr:current-messages (mq:attributes mq))))
      (mq:close mq)
      (mq:unlink name))))

(deftest make
  (testing "Make errors"
    (let ((name (concatenate 'string (random-name) (random-name))))
      (ok (signals (mq:make name :open-flags '(:read-only :create)) 'mq.cond:access-denied)))

    (let ((name (random-name)))
      (mq:close (mq:make name :open-flags '(:read-only :create) :mode '()))
      (ok (signals (mq:make name :open-flags '(:read-write)) 'mq.cond:access-denied))
      (mq:unlink name))

    (let ((name (random-name)))
      (ng (mq:close (mq:make name :open-flags '(:read-only :create))))
      (ok (signals (mq:make name :open-flags '(:read-only :create :exclusive))
              'mq.cond:file-exists))
      (ng (mq:unlink name)))

    (let ((name (subseq (random-name) 1)))
      (ok (signals (mq:make name :open-flags '(:read-only :create))
              'mq.cond:invalid-argument)))

    (let ((name (random-name)))
      (ok (signals (mq:make name :open-flags '(:read-only :create)
                                 :max-messages 0 :message-size 0))))

    (let ((name (random-name :length 300)))
      (ok (signals (mq:make name :open-flags '(:read-only :create))
              'mq.cond:name-too-long)))

    (ok (signals (mq:make "/" :open-flags '(:read-only :create))
            'mq.cond:no-file-or-directory))

    (let ((name (random-name)))
      (ok (signals (mq:make name :open-flags '(:read-only)) 'mq.cond:no-file-or-directory)))))

(deftest non-blocking
  (testing "Non blocking error"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create))))
      (mq:close mq)
      (ok (signals (setf (mq:non-blocking mq) t) 'mq.cond:bad-file-descriptor))
      (mq:unlink name)))

  (testing "Non blocking normal"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create))))
      (ng (mq.attr:non-blocking-p (mq:attributes mq)))
      (ok (setf (mq:non-blocking mq) t))
      (ok (mq.attr:non-blocking-p (mq:attributes mq)))
      (ng (setf (mq:non-blocking mq) nil))
      (ng (mq.attr:non-blocking-p (mq:attributes mq)))
      (mq:close mq)
      (mq:unlink name))))

(deftest send
  (testing "Send errors"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-only :create)
                             :max-messages 1 :message-size 5)))
      (ok (signals (mq:send-string mq "hei") 'mq.cond:bad-file-descriptor))
      (mq:close mq)
      (mq:unlink name))

    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-write :create)
                             :max-messages 1 :message-size 5)))
      (ok (signals (mq:send-string mq "helloo") 'mq.cond:message-too-long))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec -1)))
        (ok (signals (mq:timed-send-string mq "hei" :timestamp invalid-timestamp)
                'mq.cond:invalid-argument)))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec (1+ (expt 10 9)))))
        (ok (signals (mq:timed-send-string mq "hei" :timestamp invalid-timestamp)
                'mq.cond:invalid-argument)))
      (mq:close mq)
      (ok (signals (mq:send-string mq "hei") 'mq.cond:bad-file-descriptor))
      (mq:unlink name)))

  (testing "normal send"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-write :create)
                             :max-messages 1 :message-size 5))
           (msg "hei")
           (prty 5))
      (ng (mq:send-string mq msg :priority prty))
      (multiple-value-bind (m p) (mq:receive-string mq)
        (ok (and (= p prty) (string= m msg))))

      (ng (mq:send-string mq msg :priority prty))
      (ok (eq :connection-timed-out (mq:timed-send-string mq msg :priority prty)))

      (setf (mq:non-blocking mq) t)
      (ok (eq :try-again (mq:send-string mq msg :priority prty)))

      (mq:close mq)
      (mq:unlink name))))

(deftest receive
  (testing "Error receive"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:write-only :create)
                             :max-messages 1 :message-size 5)))
      (ok (signals (mq:receive-string mq) 'mq.cond:bad-file-descriptor))
      (mq:close mq)
      (mq:unlink name))

    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-write :create)
                             :max-messages 1 :message-size 5)))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec -1)))
        (ok (signals (mq:timed-receive-string mq :timestamp invalid-timestamp)
                'mq.cond:invalid-argument)))
      (let ((invalid-timestamp (local-time:unix-to-timestamp -1 :nsec (1+ (expt 10 9)))))
        (ok (signals (mq:timed-receive-string mq :timestamp invalid-timestamp)
                'mq.cond:invalid-argument)))
      (mq:close mq)
      (ok (signals (mq:receive-string mq) 'mq.cond:bad-file-descriptor))
      (mq:unlink name)))

  (testing "Normal receive"
    (let* ((name (random-name))
           (mq (mq:make name :open-flags '(:read-write :create)
                             :max-messages 1 :message-size 5))
           (msg "hei")
           (prty 5))
      (ng (mq:send-string mq msg :priority prty))
      (multiple-value-bind (m p) (mq:receive-string mq)
        (ok (and (= p prty) (string= m msg))))

      (ok (eq :connection-timed-out (mq:timed-receive-string mq)))

      (setf (mq:non-blocking mq) t)
      (ok (eq :try-again (mq:receive-string mq)))

      (mq:close mq)
      (mq:unlink name))))

(deftest with-queue
  (ok (expands
       '(mq:with-queue (mq "/hello" :open-flags '(:read-write :create))
         (mq:send-string mq "hello"))
       '(let ((mq (mq:make "/hello" :open-flags '(:read-write :create))))
         (declare (type posix-mqueue.queue::queue mq))
         (unwind-protect (progn (mq:send-string mq "hello"))
           (mq:close mq))))))
