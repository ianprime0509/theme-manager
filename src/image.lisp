;;;; image.lisp --- image analysis functions

;;;; Commentary:

;;; This file contains several functions for analyzing images and
;;; producing color schemes from them.

;;;; Code:

(in-package :theme-manager)

(defparameter *background-color*
  (make-color :red 0 :green 0 :blue 0)
  "The ideal background color to be approximated in created themes.
Usually either black or white depending on whether a dark or light
theme (respectively) is desired.")

(defparameter *light-background-color*
  (make-color :red 255 :green 255 :blue 255)
  "The ideal light background color (base07) to be approximated in created themes.
Usually either white or black depending on whether a dark or light
theme (respectively) is desired.")

(defstruct color
  "A standard RGB color with each component between 0 and 255."
  (red 0 :type (integer 0 255) :read-only t)
  (green 0 :type (integer 0 255) :read-only t)
  (blue 0 :type (integer 0 255) :read-only t))

(defun color-hex (color)
  "Return a string with COLOR in hexadecimal format."
  (with-slots (red green blue) color
    (format nil "~2,'0x~2,'0x~2,'0x" red green blue)))

(defun print-colors (colors)
  "For debugging purposes only: print COLORS in a readable format."
  (loop for color across colors
     do (format t "#~a~%" (color-hex color))))

(defun fg-bg-colors (colors &key (n-gradations 8))
  "Return a vector of N-GRADATIONS colors to be used as foreground and background colors, using two colors from COLORS as the endpoints of the gradient."
  (let ((dark (nth-value 1 (color-min-distance colors *background-color*)))
	(light (nth-value 1 (color-min-distance colors *light-background-color*))))
    (gradate dark light n-gradations)))

(defun gradate (start end n-colors)
  "Return a vector of N-COLORS colors gradating from START to END."
  (assert (>= n-colors 2) (n-colors)
	  "Must have at least two colors in gradient.")
  (let ((n-steps (1- n-colors))
	(gradient (make-array n-colors
			      :element-type 'color
			      :initial-element start)))
    (labels ((value-step (start end step)
	       (round  (+ start (* (- end start) (/ step n-steps)))))
	     (gradient-step (step)
	       (make-color :red (value-step (color-red start)
					    (color-red end)
					    step)
			   :green (value-step (color-green start)
					      (color-green end)
					      step)
			   :blue (value-step (color-blue start)
					     (color-blue end)
					     step))))
      (loop for i from 1 to n-steps
	 do (setf (aref gradient i) (gradient-step i))))
    gradient))

(defun provide-missing-colors (colors all-colors total &key (min-distance 40))
  "Return a vector of TOTAL colors consisting of the vector COLORS with additional colors from the vector ALL-COLORS."
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
	 when (>= (min-distance color) min-distance)
	 do (push color new-colors))
      (when (< (length new-colors) total)
	(error "Wanted ~a total colors but could only find ~a."
	       total (length new-colors)))
      (make-array total :element-type 'color :initial-contents new-colors))))

(defun unique-colors (colors &key (min-distance 40))
  "Return a vector of colors from the vector COLORS such that no pair is closer together than MIN-DISTANCE."
  (let ((unique ()))
    (loop for color across colors
       when (loop for other in unique
	       always (>= (distance color other) min-distance))
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

(defun k-means (colors n-means)
  "Return a vector of N-MEANS colors that serve as the centroids of the clusters in the vector COLORS.
THRESHOLD controls the convergence threshold; once the means converge,
the algorithm is complete."
  (assert (<= n-means (length colors)) (colors n-means)
	  "Cannot find ~a means from only ~a colors."
	  n-means (length colors))
  (shuffle colors)
  (let ((means (subseq colors 0 n-means)))
    (loop
       (let ((next-means (next-means colors means)))
	 (when (converged-p next-means means)
	   (return-from k-means next-means))
	 (setf means next-means)))))

(defun shuffle (data)
  "Shuffle DATA in a random order."
  (loop
     for i from (length data) downto 1
     do (rotatef
	 (aref data (random i))
	 (aref data (1- i))))
  data)

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

(defun converged-p (means1 means2)
  "Return whether the vectors MEANS1 and MEANS2 have converged."
  (equalp means1 means2))

(defun distance (p1 p2)
  "Return the Euclidean distance between colors p1 and p2."
  (sqrt (+ (square (- (color-red p1) (color-red p2)))
	   (square (- (color-green p1) (color-green p2)))
	   (square (- (color-blue p1) (color-blue p2))))))

(defun square (x)
  "Return X squared."
  (* x x))

;;;; image.lisp ends here
