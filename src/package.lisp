(defpackage #:posix-mqueue
  (:documentation "POSIX message queue bindings.")
  (:use #:cl)
  (:import-from #:alexandria #:once-only #:starts-with #:when-let)
  (:import-from #:babel #:string-to-octets #:octets-to-string)
  (:import-from #:cffi
                #:defbitfield
                #:defcfun
                #:defcstruct
                #:defctype
                #:defcvar
                #:define-foreign-library
                #:define-foreign-type
                #:expand-from-foreign
                #:expand-to-foreign
                #:expand-to-foreign-dyn
                #:foreign-alloc
                #:foreign-bitfield-symbol-list
                #:foreign-free
                #:foreign-pointer
                #:free-translated-object
                #:mem-aref
                #:null-pointer
                #:null-pointer-p
                #:translate-from-foreign
                #:translate-to-foreign
                #:use-foreign-library
                #:with-foreign-object
                #:with-foreign-slots
                #:with-pointer-to-vector-data)
  (:import-from #:local-time #:now #:nsec-of #:timestamp #:timestamp-to-unix)
  (:export
   ;; types
   #:open-flags
   #:open-flagsp
   #:create-modes
   #:create-modesp
   ;; queue
   #:buffer
   #:queue
   #:attributes
   #:non-blocking-p
   #:current-messages
   #:max-messages
   #:message-size
   ;; condition
   #:out-of-memory
   #:file-exists
   #:file-table-overflow
   #:too-many-open-files
   #:no-space-left-on-device
   #:name-too-long
   #:interrupted-system-call
   #:no-file-or-directory-just-slash
   #:no-file-or-directory-no-create
   #:no-file-or-directory-on-unlink
   #:bad-file-descriptor-invalid
   #:bad-file-descriptor-on-receive
   #:bad-file-descriptor-on-send
   #:access-denied-permission
   #:access-denied-slashes
   #:access-denied-on-unlink
   #:invalid-argument-name
   #:invalid-argument-sizes
   #:invalid-argument-attributes
   #:invalid-argument-on-unlink
   #:invalid-argument-on-send-receive
   #:message-too-long-on-receive
   #:message-too-long-on-send
   ;; lib
   #:*retry-on-interrupt-p*
   #:default-sizes
   #:close-queue
   #:open-queue
   #:receive
   #:receive-buffer
   #:receive-displaced
   #:receive-string
   #:send
   #:send-string
   #:timed-receive
   #:timed-receive-buffer
   #:timed-receive-displaced
   #:timed-receive-string
   #:timed-send
   #:timed-send-string
   #:unlink
   #:with-open-queue
   #:set-non-blocking))
