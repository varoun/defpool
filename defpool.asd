;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Author: varoun (Varoun P)
;;;; System definition for defpool

(in-package :cl-user)

(defpackage #:defroute-asd
  (:use :cl :asdf))

(in-package :defroute-asd)

(defvar *defpool-version* "0.0.1"
  "The current version of defpool.")

(defsystem :defpool
    :serial t
    :version #.*defpool-version*
    :description "Defpool is a resource pooling library. It is designed to support connection
    pooling, socket pooling etc, and can be extended by defining methods for a few generic
    functions."
    :depends-on (:bordeaux-threads
                 :cl-redis
                 :usocket)
    :components ((:file "packages")
                 (:file "pool")
                 (:file "redis")
                 (:file "sockets")))

