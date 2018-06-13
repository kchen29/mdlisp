;;;; do perspective
;;;we redefine the post-add functions
;;;(not the best, but least intrusive)
;;;divide x and y by z coordinates
(defun safe (x)
  (if (zerop x)
      .001
      x))

(defun perspective (matrix)
  (dotimes (i (m-cols matrix))
    (let ((z (safe (+ 500 (abs (mref matrix 2 i))))))
      (setf (mref matrix 0 i) (+ 250 (/ (* 500 (- (mref matrix 0 i) 250)) z))
            (mref matrix 1 i) (+ 250 (/ (* 500 (- (mref matrix 1 i) 250)) z))))))

(defun post-add-edges ()
  (matrix-multiply (car stack) edges)
  (perspective edges)
  (draw-lines edges '(255 0 255))
  (clear-matrix edges))

(defun post-add-polygons ()
  (matrix-multiply (car stack) polygons)
  (perspective polygons)
  (draw-polygons polygons)
  (clear-matrix polygons))
