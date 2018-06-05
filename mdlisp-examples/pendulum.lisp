;;;;simulating the physics of a rigid pendulum
;;increasing delta makes the pendulum lose energy faster
(defparameter delta .001)

(defparameter anchor-x 250)
(defparameter anchor-y 250)
;; (defparameter pos-x 470)
;; (defparameter pos-y 250)
(defparameter pos-x 269.174)
(defparameter pos-y 469.163)
(defparameter dx 0)
(defparameter dy 0)
(defparameter ddy -9.81)

(defparameter distance (magnitude (mapcar #'- (list anchor-x anchor-y) (list pos-x pos-y))))

(defun scale-v (vector scalar)
  (map-into vector (lambda (a) (* a scalar)) vector))

(defun restore ()
  "Restores the position of the pendulum to be DISTANCE away from ANCHOR.
   Projects the velocity vector to the perpendicular of the displacement vector
   and uses the projection as the new velocity vector.
   The pendulum is confined to a circle."
  (let* ((displacement (normalize (list (- pos-x anchor-x) (- pos-y anchor-y))))
         (perpendicular (list (second displacement) (- (first displacement))))
         (len (dot (list dx dy) perpendicular)))
    (scale-v displacement distance)
    (scale-v perpendicular len)
    (setf pos-x (+ anchor-x (first displacement))
          pos-y (+ anchor-y (second displacement))
          dx (first perpendicular)
          dy (second perpendicular))))

(defun next-step ()
  "Calculates the next position and velocity of the pendulum."
  (incf dy (* delta ddy))
  (incf pos-x (* delta dx))
  (incf pos-y (* delta dy))
  (restore))

(animate "pendulum" 79
    ()
  (line anchor-x anchor-y 0 pos-x pos-y 0)
  (sphere pos-x pos-y 0 30)
  (dotimes (i 450)
    (next-step)))


