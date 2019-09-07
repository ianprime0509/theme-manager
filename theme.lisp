;;;; theme.lisp --- theme generation functions

;;; Copyright 2019 Ian Johnson

;;; This file is part of theme-manager, a free software project for
;;; generating and managing color themes. It is distributed under the
;;; MIT license, a copy of which can be found in the project root
;;; directory in the LICENSE file.

;;;; Commentary:

;;; This file contains functions for generating themes from color
;;; palettes.

;;;; Code:

;;; Global configuration parameters

(in-package :theme-manager)

(defparameter *background-lightness* 15
  "The lightness to use for the primary background color, between 0 and 100.
For a dark theme, this should be close to 0; for a light theme, this
should be close to 100.")

(defparameter *light-background-lightness* 85
  "The lightness to use for the light background color, between 0 and 100.
This should be on the opposite end of the spectrum from
*BACKGROUND-LIGHTNESS*. The difference between these two parameters
determines how far apart the neutral theme colors are (the width of
the gradient).")

(defparameter *min-accent-saturation* 40
  "The minimum saturation to allow for an accent color.")

(defparameter *max-accent-saturation* 90
  "The maximum saturation to allow for an accent color.")

(defparameter *min-accent-value* 60
  "The minimum value to allow for an accent color.")

(defparameter *max-accent-value* 90
  "The maximum value to allow for an accent color.")

(defparameter *min-accent-contrast* 1.5
  "The minimum contrast ratio to allow for an accent color.
Contrast is measured against the selection background color, which is
base02 in a base16 theme.")

(defparameter *min-accent-distance* 40
  "The minimum distance to allow between two accent colors.
Distance in this context is the standard Euclidean distance between
two colors, treated as points with coordinates in the typical range of
0 through 255.")

;;; Primary functions

(defun theme-from-colors (colors &key (n-neutral 8) (n-accent 8)
				   (selection-bg-index (floor (/ n-neutral 4))))
  "Produce a theme from COLORS with N-NEUTRAL neutral colors and N-ACCENT accent colors.
The global configuration parameters (such as *BACKGROUND-LIGHTNESS*)
can be used to tweak the theme generated by this function.
Additionally, the SELECTION-BG-INDEX argument is the (zero-based)
index of the neutral color to treat as the selection background. This
is relevant for ensuring that accent colors have a readable contrast
against this background. In base16 themes, this is base02."
  (sort-by-count colors)
  (flet ()
    (let* ((primary (aref (k-means colors 1) 0))
	   (neutral (gradate-lightness primary
				       *background-lightness*
				       *light-background-lightness*
				       n-neutral))
	   ;; Base02 is used for the selection background, so contrast
	   ;; should be evaluated with reference to it
	   (bg-color (aref neutral selection-bg-index))
	   (bright-colors (brighten-colors (remove-extremes colors)))
	   (accent (k-means bright-colors n-accent))
	   (bright-accent (brighten-colors accent))
	   (unique-accent (unique-colors bright-accent bg-color))
	   (all-accent (provide-missing-colors unique-accent
					       bright-colors
					       bg-color
					       n-accent)))
      ;; By sorting the accent colors by hue, we have the best chance of
      ;; aligning with the order of other Base16 themes
      (sort all-accent #'< :key (lambda (color)
				  (color-hsl-hue (rgb-to-hsl color))))
      (concatenate '(vector color) neutral all-accent))))

;;; Color set improvement functions

(defun brighten-colors (colors)
  "Clamp the saturation and value of all colors in COLORS to lie between the globally configured parameters."
  (map 'vector
       #'(lambda (color)
	   (clamp-saturation-and-value color
				       *min-accent-saturation*
				       *min-accent-value*
				       *max-accent-saturation*
				       *max-accent-value*))
       colors))

(defun remove-extremes (colors)
  "Remove the colors from COLORS that are either too dark or too light.
Suitable colors are those whose saturation and value both lie within a
range of 0.1 through 0.9 (on a scale from 0 to 1). If this function is
not used to filter a set of colors before using BRIGHTEN-COLORS, some
unexpected colors may appear because of small, imperceptible
variations in the hue of such extreme colors."
  (remove-if-not #'(lambda (hsv)
		     (and (<= 0.1 (color-hsv-saturation hsv) 0.9)
			  (<= 0.1 (color-hsv-value hsv) 0.9)))
		 colors
		 :key #'rgb-to-hsv))

;;; Color set refinement functions

(defun sort-by-count (colors)
  "Sort COLORS so that the most common colors are first in the vector."
  (let ((counts (make-hash-table :test 'equalp)))
    (loop for color across colors do
	 (let ((count (alexandria:ensure-gethash color counts 0)))
	   (setf (gethash color counts) (1+ count))))
    (sort colors #'> :key #'(lambda (color) (gethash color counts)))))

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

;;; K-means clustering algorithm

;;; See https://en.wikipedia.org/wiki/K-means_clustering for details.
;;; In this project, k-means clustering is used to find an initial set
;;; of candidates for a theme's accent colors since it tends to find
;;; colors that are prominent in the input.

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

;;; Helper types

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

;;;; theme.lisp ends here