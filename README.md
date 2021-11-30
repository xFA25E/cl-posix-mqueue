# Table of Contents

1.  [Examples](#org12f7c89)
    1.  [Create a queue, send and receive a message, unlink queue.](#orgdeeb488)
    2.  [Open existing queue and wait for a message for 1 second](#orge630d0a)
    3.  [Create a queue, try to receive a message and return immediately because a call would block](#orgfd21870)
2.  [Usage](#org90b44be)
3.  [Manual](#org7f4689f)
4.  [Implementations](#orgef912a0)
    1.  [Tests succeeded](#orga45c309)
    2.  [Tests failed](#org31c5913)
        1.  [abcl](#org4487534)
        2.  [clasp](#org2a4c4db)
5.  [Todo](#org90b64d6)
    1.  [mq<sub>notify</sub>](#orge9a2ff4)
    2.  [`with-pointer-to-vector-data`](#org5e2a63e)
    3.  [publish to quicklisp and awesome-cl](#orgd046f88)

Common Lisp bindings to POSIX message queues.  Contributions and questions are
welcome.

POSIX message queue is an IPC (Inter-Process Communication) method that is easy
to use and quick to setup.


<a id="org12f7c89"></a>

# Examples


<a id="orgdeeb488"></a>

## Create a queue, send and receive a message, unlink queue.

    (with-open-queue (mq "/myqueue" :open-flags '(:read-write :create)
                                    :max-messages 5 :message-size 10)
      ;; Messages are ordered based on priority.  High priority messages are placed
      ;; at the beginning.
      (send-string mq "hello" 4)            ; 4 is priority
      (multiple-value-bind (msg-string priority) (receive-string mq)
        (when (and (string= "hello" msg-string) (= 4 priority))
          (print "It works!"))))
    (unlink "/myqueue")


<a id="orge630d0a"></a>

## Open existing queue and wait for a message for 1 second

    (with-open-queue (mq "/myqueue" :open-flags '(:read-only))
      (multiple-value-bind (msg priority)
          ;; Timestamps are absolute
          (timed-receive-string mq (local-time:timestamp+ (local-time:now) 1 :sec))
        (when (eq :connection-timed-out msg)
          (print "Timed out!"))))
    (unlink "/myqueue")


<a id="orgfd21870"></a>

## Create a queue, try to receive a message and return immediately because a call would block

    (with-open-queue (mq "/myqueue" :open-flags '(:read-only :create :non-blocking))
      (multiple-value-bind (msg priority) (receive-string mq)
        (when (eq :try-again msg)
          (print "Would block!"))))
    (unlink "/myqueue")


<a id="org90b44be"></a>

# Usage

This library uses [local-time](https://common-lisp.net/project/local-time/) library for timestamps.  All of the exported
symbols are documented in details through the internal common lisp
documentation system.


<a id="org7f4689f"></a>

# [Manual](https://xfa25e.github.io/cl-posix-mqueue/index.html)


<a id="orgef912a0"></a>

# Implementations


<a id="orga45c309"></a>

## Tests succeeded

-   sbcl
-   ecl
-   ccl
-   clisp


<a id="org31c5913"></a>

## Tests failed


<a id="org4487534"></a>

### abcl

First of all, slynk failed to load.  Quicklisp and cl-posix-mqueue, loaded
just fine.  Tests failed.  When I started using cl-posix-mqueue manually,
memory errors were printed.  After that, core dumped.  Maybe cffi should be
set differently for jvm.


<a id="org2a4c4db"></a>

### clasp

Could not build implementation on my machine


<a id="org90b64d6"></a>

# Todo


<a id="orge9a2ff4"></a>

## mq<sub>notify</sub>


<a id="org5e2a63e"></a>

## `with-pointer-to-vector-data`

It does unnecessary writes and reads on implementations where direct access
to vectors is not supported.  Maybe on receive it should only read and on
send it should only write.


<a id="orgd046f88"></a>

## publish to quicklisp and awesome-cl
