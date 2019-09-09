;;;; manager.lisp --- tests for theme management functions

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

(in-package :theme-manager-test)

(def-suite manager-suite
    :description "Theme management tests")

(in-suite manager-suite)

(test standard-template-path
  (let ((cases '((#p"theme.json.mustache" . "theme.json")
		 (#p"theme.json.mustache" . #p"theme.json")
		 (#p"directory/theme.json.mustache" . #p"directory/theme.json")
		 (#p"/home/user/theme.json.mustache" . #p"/home/user/theme.json")
		 (#p"theme.mustache" . #p"theme"))))
    (loop for (expected . input) in cases
       do (is (equal expected (standard-template-path input))))))

;;;; manager.lisp ends here
