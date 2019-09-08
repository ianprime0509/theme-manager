;;;; packages.lisp --- theme-manager package definition

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

(in-package :cl-user)

(defpackage :theme-manager
  (:documentation "Theme-manager is a project for creating and
managing color themes. It comes with facilities for generating color
themes from a set of colors and for extracting such color sets from
PNG images.")
  (:use	:common-lisp)
  (:export
   ;; color.lisp
   color
   make-color
   color-red
   color-green
   color-blue
   color-hsl
   make-color-hsl
   color-hsl-hue
   color-hsl-saturation
   color-hsl-lightness
   color-hsv
   make-color-hsv
   color-hsv-hue
   color-hsv-saturation
   color-hsv-value
   rgb-to-hsl
   rgb-to-hsv
   hsl-to-rgb
   hsv-to-rgb
   change-lightness
   clamp-saturation-and-value
   gradate-lightness
   distance
   contrast))

;;;; packages.lisp ends here
