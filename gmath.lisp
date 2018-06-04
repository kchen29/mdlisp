;;;; math functions for vectors and lighting.

;;;treating "vectors" as lists
;;closure, for fun
(let ((temp1 (make-array 3))
      (temp2 (make-array 3)))
  (defun normal (polygons index)
    "Returns the normal (fresh list) of the surface defined by POLYGONS at INDEX."
    (dotimes (x 3)
      (setf (svref temp1 x) (- (mref polygons x index)
                               (mref polygons x (1+ index)))
            (svref temp2 x) (- (mref polygons x index)
                               (mref polygons x (+ 2 index)))))
    (cross-product temp1 temp2)))

(defun cross-product (v1 v2)
  "Returns the cross-product of V1 and V2. Fresh list."
  (labels ((prod (a b)
             (* (svref v1 a)
                (svref v2 b)))
           (dif (a b)
             (- (prod a b)
                (prod b a))))
    (list (dif 1 2)
          (dif 2 0)
          (dif 0 1))))

(defun dot (v1 v2)
  "Dots V1 and V2."
  (loop for x in v1
        for y in v2
        sum (* x y)))

(defun magnitude (vector)
  "Calculates the magnitude of the vector."
  (sqrt (loop for x in vector
              sum (* x x))))

(defun normalize (vector)
  "Normalizes the vector. Destructively modifies."
  (let ((mag (magnitude vector)))
    (map-into vector (lambda (x) (/ x mag)) vector)))

;;;lighting
(defparameter ambient-color '(50 50 50))
(defparameter view '(0 0 1))
(defparameter light (normalize (list -.5 1 1)))
(defparameter light-color '(255 0 255))
(defparameter areflect '(0.2 0.2 0.2))
(defparameter dreflect '(0.7 0.7 0.7))
(defparameter sreflect '(0.8 0.8 0.8))

(declaim (inline bound rbound))
(defun bound (color)
  "Checks and fixes the values of color."
  (min 255 (max 0 color)))

(defun rbound (color)
  "Rounds and checks bounds of color."
  (bound (round color)))

(let ((ambient (mapcar (lambda (c r) (bound (* c r))) ambient-color areflect))
      (diffuse (mapcar #'* light-color dreflect))
      (specular (mapcar #'* light-color sreflect)))
  (defun calculate-color (normal)
    "Calculates the color given the NORMAL surface. Normalizes NORMAL."
    (declare (optimize (speed 3)))
    (normalize normal)
    (let* ((cos-theta (dot normal light))
           (expt-factor (expt (max 0 (loop for x in view
                                           for y in normal
                                           for z in light
                                           sum (* x (- (* 2 cos-theta y)
                                                       z))))
                              16)))
      (loop for a in ambient
            for d in diffuse
            for s in specular
            collect (rbound (+ a
                               (bound (* d cos-theta))
                               (bound (* s expt-factor))))))))
