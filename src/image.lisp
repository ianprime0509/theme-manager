;;;; image.lisp --- image analysis functions

;;;; Commentary:

;;; This file contains several functions for analyzing images and
;;; producing color schemes from them.

;;;; Code:

(in-package :theme-manager)

(defparameter *background-lightness* 15)

(defparameter *light-background-lightness* 85)

(defparameter *min-accent-saturation* 40)

(defparameter *min-accent-value* 60)

(defparameter *min-accent-contrast* 1.5)

(defparameter *min-accent-distance* 40)

(defun provide-missing-colors (colors all-colors bg-color total)
  "Return a vector of TOTAL colors consisting of the vector COLORS with additional colors from the vector ALL-COLORS.
The returned colors will satisfy the constraints on accent color
distance and contrast with BG-COLOR."
  (assert (<= (length colors) total)
	  (colors total)
	  "Requested total number of colors is ~a, less than the existing number ~a."
	  total (length colors))
  (let ((new-colors (loop for color across colors
		       collect color)))
    (flet ((min-distance (color)
	     (loop for other in new-colors
		minimize (distance color other))))
      (loop for color across all-colors
	 until (= (length new-colors) total)
	 when (and (>= (min-distance color) *min-accent-distance*)
		   (>= (contrast color bg-color) *min-accent-contrast*))
	 do (push color new-colors))
      (when (< (length new-colors) total)
	(error "Wanted ~a total colors but could only find ~a."
	       total (length new-colors)))
      (make-array total :element-type 'color :initial-contents new-colors))))

(defun unique-colors (colors bg-color)
  "Return a vector of colors from the vector COLORS satisfying the
constraints on accent color distance and contrast with BG-COLOR."
  (let ((unique ()))
    (loop for color across colors
       when (loop for other in unique
	       always (and (>= (distance color other) *min-accent-distance*)
			   (>= (contrast color bg-color) *min-accent-contrast*)))
       do (push color unique))
    (make-array (length unique)
		:element-type 'color
		:initial-contents unique)))

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

(defun theme-from-colors (colors &key (n-neutral 8) (n-accent 8))
  (let* ((primary (aref (k-means colors 1) 0))
	 (neutral (gradate-lightness primary
				     *background-lightness*
				     *light-background-lightness*
				     n-neutral))
	 ;; Base02 is used for the selection background, so contrast
	 ;; should be evaluated with reference to it
	 (bg-color (aref neutral 2))
	 (bright-colors (map 'vector
			     #'(lambda (color)
				 (clamp-saturation-and-value color
							     *min-accent-saturation*
							     *min-accent-value*))
			     colors))
	 (accent (k-means bright-colors n-accent))
	 (unique-accent (unique-colors accent bg-color))
	 (all-accent (provide-missing-colors unique-accent
					     bright-colors
					     bg-color
					     n-accent)))
    ;; By sorting the accent colors by hue, we have the best chance of
    ;; aligning with the order of other Base16 themes
    (sort all-accent #'< :key (lambda (color)
				(color-hsl-hue (rgb-to-hsl color))))
    (concatenate '(vector color) neutral all-accent)))

(defun k-means (colors n-means)
  "Return a vector of N-MEANS colors that serve as the centroids of the clusters in the vector COLORS.
THRESHOLD controls the convergence threshold; once the means converge,
the algorithm is complete."
  (assert (<= n-means (length colors)) (colors n-means)
	  "Cannot find ~a means from only ~a colors."
	  n-means (length colors))
  ;; TODO: I want this function to be deterministic, but make sure
  ;; that just choosing the initial means like this doesn't degrade
  ;; the quality of the resulting color scheme
  (let ((means (subseq colors 0 n-means)))
    (loop
       (let ((next-means (next-means colors means)))
	 (when (converged-p next-means means)
	   (return-from k-means next-means))
	 (setf means next-means)))))

(defstruct average-accumulator
  "An accumulator for several colors so that their values can be averaged."
  (red 0 :type integer)
  (green 0 :type integer)
  (blue 0 :type integer)
  (count 0 :type integer))

(defun accumulate-color (accumulator color)
  "Accumulate COLOR into ACCUMULATOR, updating its intermediate state."
  (with-slots (red green blue count) accumulator
    (setf red (+ red (color-red color)))
    (setf green (+ green (color-green color)))
    (setf blue (+ blue (color-blue color)))
    (incf count)))

(defun average-color (accumulator)
  "Return the average color value of all colors accumulated into ACCUMULATOR."
  (with-slots (red green blue count) accumulator
    (if (= count 0)
	(make-color :red 0 :green 0 :blue 0)
	(make-color :red (round (/ red count))
		    :green (round (/ green count))
		    :blue (round (/ blue count))))))

(defun next-means (colors old-means)
  "Return the next vector of means in DATA given the previous means OLD-MEANS."
  (let ((means-temp
	 ;; We accumulate the sum of the colors in each new cluster
	 ;; and the number of colors
	 (make-array (length old-means)
		     :element-type 'average-accumulator
		     :initial-element (make-average-accumulator))))
    (loop for i from 1 below (length means-temp) do
      (setf (aref means-temp i) (make-average-accumulator)))
    (loop for color across colors do
	 (let* ((cluster (color-min-distance old-means color))
		(accumulator (aref means-temp cluster)))
	   (accumulate-color accumulator color)))
    (map 'vector #'average-color means-temp)))

(defun converged-p (means1 means2)
  "Return whether the vectors MEANS1 and MEANS2 have converged."
  (equalp means1 means2))

;;;; image.lisp ends here
