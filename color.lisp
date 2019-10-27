;;;; color.lisp --- general color functions

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

;;;; Commentary:

;;; This file contains the basic data types and functions for working
;;; with colors.

;;;; Code:

(in-package :theme-manager)

(defstruct color
  "A standard RGB color with each component between 0 and 255."
  (red 0 :type (integer 0 255) :read-only t)
  (green 0 :type (integer 0 255) :read-only t)
  (blue 0 :type (integer 0 255) :read-only t))

(defstruct color-hsl
  "A color in HSL form."
  (hue 0 :type (integer 0 359) :read-only t)
  (saturation 0 :type (rational 0 1) :read-only t)
  (lightness 0 :type (rational 0 1) :read-only t))

(defstruct color-hsv
  "A color in HSV form."
  (hue 0 :type (integer 0 359) :read-only t)
  (saturation 0 :type (rational 0 1) :read-only t)
  (value 0 :type (rational 0 1) :read-only t))

;;; Formulas for RGB/HSL/HSV conversion taken from
;;; https://en.wikipedia.org/wiki/HSL_and_HSV
(defmacro with-hue-components ((hue max min) color &body body)
  (let ((red (gensym))
        (green (gensym))
        (blue (gensym))
        (diff (gensym))
        (hue-raw (gensym)))
    `(let* ((,red (/ (color-red ,color) 255))
            (,green (/ (color-green ,color) 255))
            (,blue (/ (color-blue ,color) 255))
            (,max (max ,red ,green ,blue))
            (,min (min ,red ,green ,blue))
            (,diff (- ,max ,min))
            (,hue-raw (cond
                        ((= ,diff 0) 0)
                        ((= ,max ,red) (* 60 (/ (- ,green ,blue) ,diff)))
                        ((= ,max ,green) (* 60 (+ 2
                                                  (/ (- ,blue ,red) ,diff))))
                        (t (* 60 (+ 4
                                    (/ (- ,red ,green) ,diff))))))
            (,hue (mod (round ,hue-raw) 360)))
       ,@body)))

(defun rgb-to-hsl (color)
  (with-hue-components (hue max min) color
                       (cond ((= max 0) (make-color-hsl :hue 0 :saturation 0 :lightness 0))
                             ((= min 1) (make-color-hsl :hue 0 :saturation 0 :lightness 1))
                             (t (let* ((saturation (/ (- max min)
                                                      (- 1 (abs (+ max min -1)))))
                                       (lightness (/ (+ max min) 2)))
                                  (make-color-hsl :hue hue
                                                  :saturation saturation
                                                  :lightness lightness))))))

(defun rgb-to-hsv (color)
  (with-hue-components (hue max min) color
                       (if (= max 0)
                           (make-color-hsv)
                           (let* ((saturation (/ (- max min) max))
                                  (value max))
                             (make-color-hsv :hue hue
                                             :saturation saturation
                                             :value value)))))

(defun hue-chroma-min-to-rgb (hue chroma min)
  (let* ((segment (/ hue 60))
         (x (* chroma (- 1
                         (abs (1- (mod segment 2))))))
         (initial-rgb (cond
                        ((<= 0 segment 1) (list chroma x 0))
                        ((<= 1 segment 2) (list x chroma 0))
                        ((<= 2 segment 3) (list 0 chroma x))
                        ((<= 3 segment 4) (list 0 x chroma))
                        ((<= 4 segment 5) (list x 0 chroma))
                        ((<= 5 segment 6) (list chroma 0 x))))
         (red (+ min (first initial-rgb)))
         (green (+ min (second initial-rgb)))
         (blue (+ min (third initial-rgb))))
    (make-color :red (round (* 255 red))
                :green (round (* 255 green))
                :blue (round (* 255 blue)))))

(defun hsl-to-rgb (color)
  (with-slots (hue saturation lightness) color
    (let* ((chroma (* (- 1
                         (abs (- (* 2 lightness) 1)))
                      saturation))
           (min (- lightness (/ chroma 2))))
      (hue-chroma-min-to-rgb hue chroma min))))

(defun hsv-to-rgb (color)
  (with-slots (hue saturation value) color
    (let* ((chroma (* saturation value))
           (min (- value chroma)))
      (hue-chroma-min-to-rgb hue chroma min))))

(defun change-lightness (color lightness)
  "Return COLOR with its lightness set to LIGHTNESS (from 0 to 100)."
  (assert (<= 0 lightness 100) (lightness)
          "LIGHTNESS must be between 0 and 100 (got ~a)." lightness)
  (let* ((hsl (rgb-to-hsl color))
         (new-hsl (make-color-hsl :hue (color-hsl-hue hsl)
                                  :saturation (color-hsl-saturation hsl)
                                  :lightness (/ lightness 100))))
    (hsl-to-rgb new-hsl)))

(defun clamp-saturation-and-value (color min-saturation min-value &optional (max-saturation 100) (max-value 100))
  "Return COLOR with its saturation clamped between MIN-SATURATION and MAX-SATURATION and its value clamped between MIN-VALUE and MAX-VALUE (both inclusive, from 0 to 100)."
  (let* ((hsv (rgb-to-hsv color))
         (new-hsv
          (make-color-hsv :hue (color-hsv-hue hsv)
                          :saturation (alexandria:clamp (color-hsv-saturation hsv)
                                                        (/ min-saturation 100)
                                                        (/ max-saturation 100))
                          :value (alexandria:clamp (color-hsv-value hsv)
                                                   (/ min-value 100)
                                                   (/ max-value 100)))))
    (hsv-to-rgb new-hsv)))

(defun color-hex (color)
  "Return a string with COLOR in hexadecimal format."
  (with-slots (red green blue) color
    (format nil "~2,'0x~2,'0x~2,'0x" red green blue)))

(defun print-color (color)
  "For debugging purposes only: print COLOR in a readable format."
  (format t "#~a~%" (color-hex color)))

(defun print-colors (colors)
  "For debugging purposes only: print COLORS in a readable format."
  (loop for color across colors
     do (print-color color)))

(defun gradate-lightness (color start-lightness end-lightness n-colors)
  (let ((n-steps (1- n-colors))
        (gradient (make-array n-colors
                              :element-type 'color
                              :initial-element color)))
    (labels ((value-step (start end step)
               (round (+ start (* (- end start) (/ step n-steps)))))
             (gradient-step (step)
               (change-lightness color
                                 (value-step start-lightness
                                             end-lightness
                                             step))))
      (dotimes (i n-steps)
        (setf (aref gradient i) (gradient-step i))))
    gradient))

(defun color-min-distance (colors color)
  "Find the color in the vector COLORS that is the shortest distance from COLOR.
This function returns two values, first the position of the color in
COLORS and second the color itself."
  (assert (/= (length colors) 0) (colors) "COLORS is empty.")
  (let* ((min-idx 0)
         (min-distance (distance color (aref colors min-idx))))
    (loop for i from 1 below (length colors) do
         (let ((distance (distance color (aref colors i))))
           (when (< distance min-distance)
             (setf min-idx i
                   min-distance distance))))
    (values min-idx (aref colors min-idx))))

(defun distance (color1 color2)
  "Return the Euclidean distance between colors COLOR1 and COLOR2."
  (flet ((square (x) (* x x)))
    (sqrt (+ (square (- (color-red color1) (color-red color2)))
             (square (- (color-green color1) (color-green color2)))
             (square (- (color-blue color1) (color-blue color2)))))))

(defun contrast (color1 color2)
  "Return the contrast ratio between COLOR1 and COLOR2.
The formula is taken from
https://www.w3.org/TR/WCAG/#dfn-contrast-ratio."
  (let* ((l1 (relative-luminance color1))
         (l2 (relative-luminance color2))
         (min-l (min l1 l2))
         (max-l (max l1 l2)))
    (/ (+ max-l 0.05) (+ min-l 0.05))))

(defun relative-luminance (color)
  "Return the relative luminance of COLOR.
The formula is taken from
https://www.w3.org/TR/WCAG/#dfn-relative-luminance."
  (flet ((component (value)
           (if (<= value 0.03928)
               (/ value 12.92)
               (expt (/ (+ value 0.055) 1.055) 2.4))))
    (let* ((red (/ (color-red color) 255))
           (green (/ (color-green color) 255))
           (blue (/ (color-blue color) 255)))
      (+ (* 0.2126 (component red))
         (* 0.7152 (component green))
         (* 0.0722 (component blue))))))

;;;; color.lisp ends here
