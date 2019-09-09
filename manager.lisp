;;;; manager.lisp --- system theme management functions

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

;;;; Commentary:

;;; This file contains functions for system-wide theme management.

;;;; Code:

(in-package :theme-manager)

(defparameter *standard-template-type* "mustache"
  "The standard file type used for templates.")

(defun apply-theme (theme path-list)
  "Apply THEME to the files described by PATH-LIST.
PATH-LIST is a list of file descriptions. Each file description can be
a pair (OUTPUT-PATH . TEMPLATE-PATH) of two pathnames (or strings)
pointing to the output (rendered) file and template file.
Alternatively, it can be just OUTPUT-PATH, in which case TEMPLATE-PATH
is determined by STANDARD-TEMPLATE-PATH."
  (loop for description in path-list
     do (let ((output-path (if (consp description)
			       (car description)
			       description))
	      (template-path (if (consp description)
				 (cdr description)
				 (standard-template-path description))))
	  (render-base16-template-file theme template-path output-path))))

(defun standard-template-path (path)
  "Return the standard template path corresponding to the output path PATH.
The standard template path is just PATH with a type of
*STANDARD-TEMPLATE-TYPE* appended. For example, the output path
'/home/user/.emacs.d/themes/my-theme.el' has a standard template path
of '/home/user/.emacs.d/themes/my-theme.el.mustache'."
  (let* ((pathname (pathname path))
	(new-name (if (pathname-type pathname)
		      (concatenate 'string
				   (pathname-name pathname)
				   "."
				   (pathname-type pathname))
		      (pathname-name pathname))))
    (make-pathname :defaults pathname
		   :name new-name
		   :type *standard-template-type*)))

;;;; manager.lisp ends here
