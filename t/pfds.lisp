#|
  This file is a part of pfds project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage pfds-test
  (:use :cl
        :pfds
        :cl-test-more))
(in-package :pfds-test)

(plan nil)

;; side effects of a suspension only occur once
(let* ((foo 0)
       (susp ($ (incf foo))))
  (is foo 0)
  (is (force susp) 1)
  (is foo 1)
  (is (force susp) 1)
  (is foo 1))

;; test make-stream, take, stream-to-list
(let* ((foo 0)
       (strm (make-stream (incf foo) (incf foo) (incf foo)))
       (part (take 2 strm)))
  (is foo 0)
  (is (stream-to-list part) (list 1 2))
  (is foo 2)
  (is (stream-to-list strm) (list 1 2 3))
  (is foo 3))

;; append-streams
(let* ((foo 0)
       (strm1 (make-stream (incf foo) (incf foo)))
       (strm2 (make-stream (incf foo) (incf foo)))
       (strm3 (append-streams strm1 strm2)))
  (is foo 0)
  (is (stream-to-list strm3) (list 1 2 3 4))
  (is foo 4))

;; drop
(let* ((foo 0)
       (strm1 (make-stream (incf foo) (incf foo) (incf foo) (incf foo)))
       (strm2 (drop 2 strm1)))
  (is foo 0)
  (is (stream-to-list strm2) (list 3 4))
  (is foo 4))

;; reverse-stream
(let* ((foo 0)
       (strm1 (make-stream (incf foo) (incf foo) (incf foo) (incf foo)))
       (strm2 (reverse-stream strm1)))
  (is foo 0)
  (is (stream-to-list strm2) (list 4 3 2 1))
  (is foo 4))

(let ((queue-classes '(batched-queue bankers-queue physicists-queue)))
  (mapc (lambda (queue-class)
          (let ((q (make-instance queue-class)))
            (is (empty-p q) T (format nil "Test0 for ~a" queue-class))
            (let ((q (snoc (snoc q 1) 2)))
              (is (head q) 1 (format nil "Test1 for ~a" queue-class))
              (is (head (tail q)) 2 (format nil "Test2 for ~a" queue-class))))
          (let ((q (make-instance queue-class)))
            (dotimes (x 100) (setq q (snoc q x)))
            (dotimes (x 75) (setq q (tail q)))
            (is (head q) 75 (format nil "Test3 for ~a" queue-class))))
        queue-classes))

(finalize)
