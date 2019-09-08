;;;; color.lisp --- tests for color functions

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

(in-package :theme-manager-test)

(def-suite color-suite
    :description "Color-related tests")

(in-suite color-suite)

(defun rgb (red green blue)
  (make-color :red red :green green :blue blue))

(defun hsl (hue saturation lightness)
  (make-color-hsl :hue hue :saturation saturation :lightness lightness))

(defun hsv (hue saturation value)
  (make-color-hsv :hue hue :saturation saturation :value value))

(defparameter *color-equivalences*
  (list (list (rgb 0 0 0) (hsl 0 0 0) (hsv 0 0 0))
	(list (rgb 255 255 255) (hsl 0 0 1) (hsv 0 0 1))
	(list (rgb 255 0 0) (hsl 0 1 1/2) (hsv 0 1 1))
	(list (rgb 255 255 0) (hsl 60 1 1/2) (hsv 60 1 1))
	(list (rgb 0 255 0) (hsl 120 1 1/2) (hsv 120 1 1))
	(list (rgb 0 255 255) (hsl 180 1 1/2) (hsv 180 1 1))
	(list (rgb 0 0 255) (hsl 240 1 1/2) (hsv 240 1 1))
	(list (rgb 255 0 255) (hsl 300 1 1/2) (hsv 300 1 1))
	(list (rgb 128 128 128) (hsl 0 0 128/255) (hsv 0 0 128/255))
	(list (rgb 128 0 0) (hsl 0 1 64/255) (hsv 0 1 128/255)))
  "Colors to be considered equivalent under conversions.")

(test rgb-to-hsl
  "Test RGB to HSL conversions."
  (loop for (rgb hsl nil) in *color-equivalences*
     do (is (equalp hsl (rgb-to-hsl rgb)))))

(test rgb-to-hsv
  "Test RGB to HSV conversions."
  (loop for (rgb nil hsv) in *color-equivalences*
     do (is (equalp hsv (rgb-to-hsv rgb)))))

(test hsl-to-rgb
  "Test HSL to RGB conversions."
  (loop for (rgb hsl nil) in *color-equivalences*
     do (is (equalp rgb (hsl-to-rgb hsl)))))

(test hsv-to-rgb
  "Test HSV to RGB conversions."
  (loop for (rgb nil hsv) in *color-equivalences*
     do (is (equalp rgb (hsv-to-rgb hsv)))))

;;;; color.lisp ends here
