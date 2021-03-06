;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Author: varoun (Varoun P)
;;;; Package definitions for defpool

(in-package :cl-user)

(defpackage #:pool
  (:use :cl :bordeaux-threads)
  (:export #:acquire-from-pool
           #:release-to-pool
           #:valid-resource
           #:force-terminate
           #:cleanup-resource
           #:resource-problem))

(defpackage #:redis-pool
  (:use :cl :pool :redis)
  (:export #:with-redis-pool))

(defpackage #:socket-pool
  (:use :cl :pool :usocket)
  (:export #:with-socket-pool))
