;;;; -*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-
;;;; Author: varoun (Varoun P)
;;;; Support library for resource (threads, connections etc) pooling.

(in-package :pool)

(defparameter *resource-pool-max-free-slots* 4
  "Threshold for the maximum free resources in the pool before we throwaway a resource rather than
  returning it to the pool. Set to NIL for no limit.")

(defvar *resource-pool* (make-hash-table :test #'equal))
(defvar *resource-pool-lock* (make-lock "Resource pool lock"))

(defclass resource-pool ()
  ((resource-constructor 
    :accessor resource-constructor 
    :initarg :resource-constructor
    :documentation "A thunk that makes a new resource when funcalled.")
   (resource-type 
    :accessor pool-resource-type 
    :initarg :pool-resource-type)
   (free-resources 
    :accessor free-resources 
    :initform nil)
   (all-resources 
    :accessor all-resources 
    :initform nil)
   (lock 
    :accessor resource-pool-lock 
    :initform (make-lock "Resource pool"))))

(defun find-or-create-resource-pool (resource-thunk resource-type)
  "Find the resource pool in hash table, creates a new resource pool if not found."
  (with-lock-held (*resource-pool-lock*)
    (or (gethash resource-type *resource-pool*)
        (setf (gethash resource-type *resource-pool*)
              (make-instance 'resource-pool 
                             :resource-constructor resource-thunk
                             :pool-resource-type resource-type)))))

(define-condition resource-problem (error)
  ((text :initarg :text :reader text)))

(defgeneric valid-resource (resource)
  (:documentation "Ensure that the resource is valid, signal `resource-problem' if not."))

(defgeneric force-terminate (resource)
  (:documentation "Force termination of the resource from the pool."))

(defgeneric cleanup-resource (resource)
  (:documentation "Perform any cleanup actions needed on the resource so that it may be released
  to the pool."))

(defun acquire-from-pool (resource-thunk resource-type &optional pool)
  "Try to find a resource from the pool or create a new one if needed. When using a resource
  from the pool, ensure that it is still valid."
  (unless (typep pool 'resource-pool)
    (setf pool (find-or-create-resource-pool resource-thunk resource-type)))
  (or (loop 
         for pres = (with-lock-held ((resource-pool-lock pool))
                     (pop (free-resources pool)))
         always pres
         thereis (handler-case 
                     (progn (valid-resource pres)
                            pres)
                   (resource-problem (e) 
                     (warn "Resource ~S was not valid when acquired from pool: ~S"
                           pres
                           (text e))
                     (force-terminate pres)
                     (with-lock-held ((resource-pool-lock pool))
                       (setf (all-resources pool)
                             (delete pres (all-resources pool))))
                     nil)))
      ;; Make sure that when we create a resource, it succeeds. Keep retrying on any errors.
      (do ((res (handler-case 
                       (funcall (resource-constructor pool))
                     (error ()
                       (warn "Count not create resource!")))
                (handler-case 
                       (funcall (resource-constructor pool))
                     (error ()
                       (warn "Count not create resource!")))))
          (res (progn (with-lock-held ((resource-pool-lock pool))
                        (push res (all-resources pool)))
                      res)))))
                       
(defun release-to-pool (resource resource-type &optional pool)
  "Release the resource back to its pool, give it a change to clean up."
  (unless (typep pool 'resource-pool)
    (setf pool (gethash resource-type *resource-pool*)))
  (cond
    ;; Note that the list of free resources is read outside a lock. This should be fine as long
    ;; as the list is not destructively modified (push/pop modify the place not the
    ;; list). Multiple threads gettnig to this test at the same time would result in the pool
    ;; size growing larger.
    ((and *resource-pool-max-free-slots*
          (>= (length (free-resources pool))
              *resource-pool-max-free-slots*))
     (force-terminate resource)
     (with-lock-held ((resource-pool-lock pool))
       (setf (all-resources pool)
             (delete resource (all-resources pool)))))
    (t
     (cleanup-resource resource)
     (with-lock-held ((resource-pool-lock pool))
       (push resource (free-resources pool))))))


(defun clear-resource-pool (pool)
  "This will forcefully terminate resources in the pool without regard to whether a thread is
  currently using it."
  (with-lock-held ((resource-pool-lock pool))
    (mapc #'force-terminate (all-resources pool))
    (setf (all-resources pool) nil
          (free-resources pool) nil))
  nil)

(defun clear-resources (&optional clear)
  "Terminate all resources. When CLEAR, also deletes the pool objects."
  (with-lock-held (*resource-pool-lock*)
    (maphash #'(lambda (key pool)
                 (declare (ignore key))
                 (clear-resource-pool pool))
             *resource-pool*)
    (when clear (clrhash *resource-pool*))
    t))
