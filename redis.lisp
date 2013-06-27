;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Author: varoun (Varoun P)
;;;; Connection pooling for redis.

(in-package :pool)

(defmethod valid-resource ((resource redis:redis-connection))
  (redis::connection-open-p resource))

(defmethod force-terminate ((resource redis:redis-connection))
  (redis:close-connection resource))

(defmethod cleanup-resource ((resource redis:redis-connection))
  (force-output (redis::conn-stream resource)))

(defmacro with-redis-pool ((&key (host #(127 0 0 1)) (port 6379))
                           &body body)
  "Evaluate the body with REDIS:*CONNECTION* bound to a connection retrieved from the redis
  connection pool."
  `(let ((redis:*connection* 
          (acquire-from-pool #'(lambda () 
                                 (make-instance 'redis:redis-connection 
                                                :host ,host
                                                :port ,port))
                             :redis)))
     (unwind-protect (progn ,@body)
       (release-to-pool redis:*connection* :redis))))
