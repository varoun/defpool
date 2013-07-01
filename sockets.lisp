;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Author: varoun (Varoun P)
;;;; Connection pooling for tcp sockets.

(in-package :socket-pool)

(defmethod valid-resource ((resource usocket:usocket))
  (handler-case (unless 
                    (read-char-no-hang (usocket:socket-stream resource))
                  t)
    (end-of-file () 
      (error 'resource-problem :text "Remote end has closed socket."))))
      
(defmethod force-terminate ((resource usocket:usocket))
  (usocket:socket-close resource))

(defmethod cleanup-resource ((resource usocket:usocket))
  (force-output (usocket:socket-stream resource)))

(defmacro with-socket-pool ((name 
                             &key host port
                             (protocol :stream)
                             (element-type 'character))
                            &body body)
  `(let ((,name 
          (acquire-from-pool 
           #'(lambda () (usocket:socket-connect 
                         ,host 
                         ,port
                         :protocol ,protocol
                         :element-type ',element-type))
           :socket)))
     (unwind-protect (progn ,@body)
       (release-to-pool sock :socket))))
