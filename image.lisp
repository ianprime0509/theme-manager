;;;; image.lisp --- image analysis functions

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

;;;; Commentary:

;;; This file contains several functions for analyzing images and
;;; producing color schemes from them.

;;;; Code:

(in-package :theme-manager)

(defun colors-from-png (path)
  "Return a vector of colors from the PNG file located at PATH."
  (with-open-file (input path :element-type '(unsigned-byte 8))
    (extract-colors (png:decode input))))

(defun extract-colors (image &key (sample-step 5))
  "Return a vector of the colors in IMAGE.
Only pixels at intervals of SAMPLE-STEP (in both the x and y
directions) will be considered."
  (check-type image png:rgb-image)
  (let* ((8-bit (png:8-bit-image image))
	 (num-y (floor (/ (png:image-height 8-bit) sample-step)))
	 (num-x (floor (/ (png:image-width 8-bit) sample-step)))
	 (colors (make-array (* num-y num-x)
			     :element-type 'color
			     :initial-element (make-color))))
    (dotimes (j num-y)
      (dotimes (i num-x)
	(let* ((y (* j sample-step))
	       (x (* i sample-step))
	       (color (make-color :red (aref 8-bit y x 0)
				  :green (aref 8-bit y x 1)
				  :blue (aref 8-bit y x 2))))
	  (setf (aref colors (+ (* num-y i) j)) color))))
    colors))

;;;; image.lisp ends here
