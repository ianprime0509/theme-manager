;;;; template.lisp --- templating functions

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

;;;; Commentary:

;;; This file contains functions for working with (Mustache) template
;;; files, particularly for Base16 themes.

;;;; Code:

(in-package :theme-manager)

(defparameter *base16-color-formats*
  `(("hex" . ,#'(lambda (component)
		  (format nil "~2,'0x" component)))
    ("rgb" . ,#'(lambda (component)
		  (format nil "~d" component)))
    ("dec" . ,#'(lambda (component)
		  (format nil "~f" (/ component 255)))))
  "An alist of color formats to include in the context variables produced by BASE16-THEME-CONTEXT.")

(defun base16-theme-context (theme &key scheme-name scheme-author scheme-slug)
  "Return a Mustache template context for the Base16 theme THEME.
The SCHEME-NAME, SCHEME-AUTHOR and SCHEME-SLUG arguments can be used
to override the default generated values for those variables in the
returned context."
  (assert (= (length theme) 16) (theme) "THEME must have 16 colors.")
  (labels ((make-variable-name (index type component)
	     (format nil "base~2,'0x-~a-~a" index type component))
	   (color-variable (color index type format)
	     `((,(make-variable-name index type "r") .
		 ,(funcall format (color-red color)))
	       (,(make-variable-name index type "g") .
		 ,(funcall format (color-green color)))
	       (,(make-variable-name index type "b") .
		 ,(funcall format (color-blue color)))))
	   (color-variables (theme type format)
	     (loop for color across theme
		for i below 16
		append (color-variable color i type format))))
    `(("scheme-name" . ,(or scheme-name "Theme-manager generated theme"))
      ("scheme-author" . ,(or scheme-author "Theme-manager"))
      ("scheme-slug" . ,(or scheme-slug "theme-manager-generated"))
      ,@(loop for (type . format) in *base16-color-formats*
	   append (color-variables theme type format)))))

;;;; template.lisp ends here
