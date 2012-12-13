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

(finalize)
