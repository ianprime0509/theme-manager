;;;; theme-manager-test.asd --- ASDF system definition for theme-manager tests

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

(defsystem "theme-manager-test"
  :description "Tests for theme-manager"
  :version "0.1.0"
  :author "Ian Johnson <ianprime0509@gmail.com>"
  :licence "MIT"
  :depends-on ("fiveam" "theme-manager")
  :components
  ((:module "test"
            :pathname "test"
            :components ((:file "packages")
                         (:file "color" :depends-on ("packages"))
                         (:file "manager" :depends-on ("packages"))))))

;;;; theme-manager-test.asd ends here
