;;;; packages.lisp --- test package definition for theme-manager

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

(in-package :cl-user)

(defpackage :theme-manager-test
  (:documentation "Tests for theme-manager.")
  (:use :common-lisp :theme-manager :it.bese.FiveAM))

;;;; packages.lisp ends here
