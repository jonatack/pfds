#|
  This file is a part of pfds project.
  Copyright (c) 2012 Stephen A. Goss (steveth45@gmail.com)
|#

(in-package :cl-user)
(defpackage pfds-test-asd
  (:use :cl :asdf))
(in-package :pfds-test-asd)

(defsystem pfds-test
  :author "Stephen A. Goss"
  :license "Modified BSD"
  :depends-on (:pfds
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "pfds"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
