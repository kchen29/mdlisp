(defparameter delta .001)

(defparameter elasticity .1)
(defparameter l 220)

;; (defparameter pos-x 220)
;; (defparameter pos-y 0)
(defparameter pos-x 19.174)
(defparameter pos-y 219.163)
(defparameter dx 0)
(defparameter dy 0)

(defun scale-v (vector scalar)
  (mapcar (lambda (a) (* a scalar)) vector))

(defun next-step ()
  "Calculates the next position and velocity of the pendulum."
  ;;treat rope as a restoring force once distance passes the length of the rope
  (let* ((ddy -9.81)
         (ddx 0)
         (position (list pos-x pos-y))
         (distance (magnitude position))
         (diff (- distance l)))
    (when (plusp diff)
      (let* ((mag (/ diff elasticity))
            (force (scale-v position (/ (- mag) distance))))
        (incf ddx (first force))
        (incf ddy (second force))))
    (incf dy (* delta ddy))
    (incf dx (* delta ddx))
    (incf pos-x (* delta dx))
    (incf pos-y (* delta dy))))

(animate "rope" 79
    ()
  (move 250 250 0)
  (line 0 0 0 pos-x pos-y 0)
  (sphere pos-x pos-y 0 30)
  (dotimes (i 450)
    (next-step)))
