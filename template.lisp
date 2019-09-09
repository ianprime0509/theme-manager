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
  `(("hex" . ,#'(lambda (color)
		  (with-slots (red green blue) color
		    (format nil "~2,'0x~2,'0x~2,'0x" red green blue))))
    ("hex-bgr" . ,#'(lambda (color)
		      (with-slots (red green blue) color
			(format nil "~2,'0x~2,'0x~2,'0x" blue green red)))))
  "An alist of color formats, operating at the color level, to include in the context variables produced by BASE16-THEME-CONTEXT.")

(defparameter *base16-component-formats*
  `(("hex" . ,#'(lambda (component)
		  (format nil "~2,'0x" component)))
    ("rgb" . ,#'(lambda (component)
		  (format nil "~d" component)))
    ("dec" . ,#'(lambda (component)
		  (format nil "~f" (/ component 255)))))
  "An alist of color formats, operating at the component level, to include in the context variables produced by BASE16-THEME-CONTEXT.")

(defparameter *default-scheme-name* "Theme-manager generated theme"
  "The default name to use for generated Base16 themes.")

(defparameter *default-scheme-author* "Theme-manager"
  "The default author to use for generated Base16 themes.")

(defparameter *default-scheme-slug* "theme-manager-generated"
  "The default slug to use for generated Base16 themes.")

(defun render-base16-template-file (theme template-path output-path
				    &key scheme-name scheme-author scheme-slug)
  "Render the template file at TEMPLATE-PATH using THEME, writing the results to OUTPUT-PATH."
  (with-open-file (output-stream output-path :direction :output :if-exists :supersede)
    (mustache:render (pathname template-path)
		     (base16-theme-context theme
					   :scheme-name scheme-name
					   :scheme-author scheme-author
					   :scheme-slug scheme-slug)
		     output-stream)))

(defun base16-theme-context (theme &key scheme-name scheme-author scheme-slug)
  "Return a Mustache template context for the Base16 theme THEME.
The SCHEME-NAME, SCHEME-AUTHOR and SCHEME-SLUG arguments can be used
to override the default generated values for those variables in the
returned context."
  (assert (= (length theme) 16) (theme) "THEME must have 16 colors.")
  (labels ((make-variable-name (index type &optional component)
	     (if component
		 (format nil "base~2,'0x-~a-~a" index type component)
		 (format nil "base~2,'0x-~a" index type)))
	   (component-variables (color index type format)
	     (list (cons (make-variable-name index type "r")
			 (funcall format (color-red color)))
		   (cons (make-variable-name index type "g")
			 (funcall format (color-green color)))
		   (cons (make-variable-name index type "b")
			 (funcall format (color-blue color)))))
	   (color-variable (color index type format)
	     (cons (make-variable-name index type)
		   (funcall format color))))
    `(("scheme-name" . ,(or scheme-name *default-scheme-name*))
      ("scheme-author" . ,(or scheme-author *default-scheme-author*))
      ("scheme-slug" . ,(or scheme-slug *default-scheme-slug*))
      ,@(loop for (type . format) in *base16-component-formats*
	   append (loop for color across theme
		     for i below 16
		     append (component-variables color i type format)))
      ,@(loop for (type . format) in *base16-color-formats*
	   append (loop for color across theme
		     for i below 16
		     collect (color-variable color i type format))))))

;;;; template.lisp ends here
