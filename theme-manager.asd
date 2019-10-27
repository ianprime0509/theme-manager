;;;; theme-manager.asd --- ASDF system definition for theme-manager

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

(defsystem "theme-manager"
  :description "A color theme manager and generator"
  :version "0.1.0"
  :author "Ian Johnson <ianprime0509@gmail.com>"
  :licence "MIT"
  :depends-on ("alexandria" "cl-mustache" "png" "uiop")
  :components ((:file "packages")
               (:file "color" :depends-on ("packages"))
               (:file "image" :depends-on ("color"))
               (:file "manager" :depends-on ("theme"))
               (:file "template" :depends-on ("color"))
               (:file "theme" :depends-on ("color" "image"))))

;;;; theme-manager.asd ends here
