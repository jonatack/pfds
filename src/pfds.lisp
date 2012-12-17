#|
  This file is a part of pfds project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage pfds
  (:use :cl :alexandria)
  (:export
    #:suspension
    #:$
    #:force
    #:take
    #:stream-to-list
    #:make-stream
    #:append-streams
    #:drop
    #:reverse-stream
    #:empty-p
    #:snoc
    #:head
    #:tail
    #:batched-queue
    #:bankers-queue
    #:physicists-queue
    #:bottom-up-merge
    #:bottom-up-mergesort
    #:add
    #:mergesort
    #:mergesort-alt))
(in-package :pfds)

;; Chapter 1

(defclass suspension ()
  ((thunk :initarg :thunk)
   (memo)))

(defmacro $ (&body body)
  `(make-instance 'suspension
                  :thunk (lambda () ,@body)))

(defgeneric force (suspension))

(defmethod force ((suspension suspension))
  (if (not (slot-boundp suspension 'memo))
    (setf (slot-value suspension 'memo)
          (funcall (slot-value suspension 'thunk)))
    (setf (slot-value suspension 'thunk) nil))
  (slot-value suspension 'memo))

(defgeneric take (n stream))

(defmethod take ((n (eql 0)) stream)
  ($ nil))

(defmethod take ((n fixnum) stream)
  ($ (let ((firstcell (force stream)))
       (if (null firstcell)
         nil
         (cons (car firstcell)
               (take (1- n) (cdr firstcell)))))))

;; Chapter 2

;; convenience functions
(defun stream-to-list (stream)
  (let ((firstcell (force stream)))
    (if (null firstcell)
      nil
      (cons (car firstcell)
            (stream-to-list (cdr firstcell))))))

(defmacro make-stream (&body body)
  (if (null body)
    '($ nil)
    `($ (cons ,(car body) (make-stream ,@(cdr body))))))

;; ++ (monolithic)
(defun append-suspended-lists (suslist1 suslist2)
  ($ (append (force suslist1) (force suslist2))))

;; ++ (streams)
(defun append-streams (stream1 stream2)
  ($ (let ((firstcell (force stream1)))
       (if (null firstcell)
         (force stream2)
         (cons (car firstcell)
               (append-streams (cdr firstcell) stream2))))))

;; drop
(defun drop-aux (n stream)
  (if (= n 0)
    (force stream)
    (let ((firstcell (force stream)))
      (if (null firstcell)
        nil
        (drop-aux (1- n) (cdr firstcell))))))

(defun drop (n stream)
  ($ (drop-aux n stream)))

;; reverse
(defun reverse-aux (oldstream new)
  (let ((firstcell (force oldstream)))
    (if (null firstcell)
      new
      (reverse-aux (cdr firstcell) (cons (car firstcell) ($ new))))))

(defun reverse-stream (stream)
  ($ (reverse-aux stream nil)))

;; Chapter 3

;; Batched-Queue

(defclass batched-queue ()
  ((front :initform nil :initarg :front)
   (rear :initform nil :initarg :rear)))

(defgeneric empty-p (sequence))
(defgeneric snoc (sequence value))
(defgeneric head (sequence))
(defgeneric tail (sequence))

;; implementation of psuedo-contructor
(defun construct-batched-queue (front rear)
  (if (null front)
    (make-instance 'batched-queue
                   :front (reverse rear))
    (make-instance 'batched-queue
                   :front front
                   :rear rear)))

(defmethod empty-p ((queue batched-queue))
  (null (slot-value queue 'front)))

(defmethod snoc ((queue batched-queue) value)
  (with-slots (front rear) queue
    (construct-batched-queue front (cons value rear))))

(defmethod head ((queue batched-queue))
  (when (empty-p queue)
    (error "Called HEAD on an empty queue."))
  (car (slot-value queue 'front)))

(defmethod tail ((queue batched-queue))
  (when (empty-p queue)
    (error "Called TAIL on an empty queue."))
  (with-slots (front rear) queue
    (construct-batched-queue (cdr front) rear)))

;; Section 3.4.2 BankersQueue

(defclass bankers-queue ()
  ((front :initform (make-stream) :initarg :front)
   (front-length :initform 0 :initarg :front-length)
   (rear :initform (make-stream) :initarg :rear)
   (rear-length :initform 0 :initarg :rear-length)))

(defgeneric queue-length (queue))

(defmethod queue-length ((queue bankers-queue))
  (with-slots (front-length rear-length) queue
    (+ front-length rear-length)))

;; something like the psuedo-constructor on page 25
(defun construct-bankers-queue (front front-length rear rear-length)
  (if (<= rear-length front-length)
    (make-instance 'bankers-queue
                   :front front
                   :front-length front-length
                   :rear rear
                   :rear-length rear-length)
    (make-instance 'bankers-queue
                   :front (append-streams front (reverse-stream rear))
                   :front-length (+ front-length rear-length))))

(defmethod empty-p ((queue bankers-queue))
  (with-slots (front-length rear-length) queue
    (and (= 0 front-length) (= 0 rear-length))))

(defmethod snoc ((queue bankers-queue) value)
  (with-slots (front front-length rear rear-length) queue
    (construct-bankers-queue front front-length
                             ($ (cons value rear)) (1+ rear-length))))

(defmethod head ((queue bankers-queue))
  (when (empty-p queue)
    (error "Called HEAD on empty bankers-queue"))
  (car (force (slot-value queue 'front))))

(defmethod tail ((queue bankers-queue))
  (when (empty-p queue)
    (error "Called TAIL on empty bankers-queue"))
  (with-slots (front front-length rear rear-length) queue
    (construct-bankers-queue (cdr (force front)) (1- front-length)
                        rear rear-length)))

;; 3.5 PhysicistsQueue

(defclass physicists-queue ()
  ((working :initform nil :initarg :working)
   (front :initform ($ nil) :initarg :front)
   (front-length :initform 0 :initarg :front-length)
   (rear :initform nil :initarg :rear)
   (rear-length :initform 0 :initarg :rear-length)))

(defun construct-physicists-queue (working front front-length rear rear-length)
  (if (<= rear-length front-length)
    (make-instance 'physicists-queue
                   :working (if (null working) (force front) working)
                   :front front
                   :front-length front-length
                   :rear rear
                   :rear-length rear-length)
    (let* ((new-working (force front))
           (new-front ($ (append new-working (reverse rear)))))
      (make-instance 'physicists-queue
                     :working (if (null new-working)
                                (force new-front)
                                new-working)
                     :front new-front
                     :front-length (+ front-length rear-length)))))

(defmethod empty-p ((queue physicists-queue))
  (= 0 (slot-value queue 'front-length)))

(defmethod snoc ((queue physicists-queue) value)
  (with-slots (working front front-length rear rear-length) queue
    (construct-physicists-queue working front front-length
                                (cons value rear) (1+ rear-length))))

(defmethod head ((queue physicists-queue))
  (when (empty-p queue)
    (error "Called HEAD on empty physicists-queue"))
  (car (slot-value queue 'working)))

(defmethod tail ((queue physicists-queue))
  (when (empty-p queue)
    (error "Called HEAD on empty physicists-queue"))
  (with-slots (working front front-length rear rear-length) queue
    (construct-physicists-queue (cdr working) ($ (cdr (force front)))
                                (1- front-length) rear rear-length)))

;; Sortable interface
;; instead of implementing "new" interface, let make-instance handle that
;; with initarg :less
;;
;; This interface is missing any method for retrieving values

(defgeneric add (value sortable))
(defgeneric mergesort (sortable))
(defgeneric mergesort-alt (sortable))

(defclass bottom-up-mergesort ()
  ((less :initarg :less)
   (size :initform 0 :initarg :size)
   (segments :initform ($ nil) :initarg :segments)))

(defun bottom-up-merge (less xs ys)
  (cond ((null xs) ys)
        ((null ys) xs)
        (T (let ((x (car xs)) (y (car ys)))
             (if (funcall less x y)
               (cons x (bottom-up-merge less (cdr xs) ys))
               (cons y (bottom-up-merge less xs (cdr ys))))))))

(defmethod add (value (sortable bottom-up-mergesort))
  (with-slots (less size segments) sortable
    (labels ((add-seg (seg segs size)
              (if (= 0 (mod size 2))
                (cons seg segs)
                (add-seg (bottom-up-merge less seg (car segs))
                          (cdr segs) (truncate size 2)))))
      (make-instance 'bottom-up-mergesort
                     :less less
                     :size (1+ size)
                     :segments ($ (add-seg (list value) (force segments) size))))))

(defmethod mergesort ((sortable bottom-up-mergesort))
  (with-slots (less segments) sortable
   (labels ((merge-all (xs segs)
             (if (null segs)
               xs
               (merge-all (bottom-up-merge less xs (car segs)) (cdr segs)))))
     (merge-all nil (force segments)))))

(defun foldl (function value list)
  (if list (foldl function (funcall function value (car list)) (cdr list)) value))

(defmethod mergesort-alt ((sortable bottom-up-mergesort))
  (with-slots (less segments) sortable
    (foldl (curry #'bottom-up-merge less) nil (force segments))))

