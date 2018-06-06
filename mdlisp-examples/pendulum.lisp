(defparameter delta .001)
(defparameter g 9.81)

(defparameter l 220)
(defparameter theta (/ pi 2))
(defparameter omega 0)

(defun next-step ()
  "Calculate the next theta."
  ;;angular acceleration is g/l sin theta
  (incf omega (* delta (/ g l) (sin theta)))
  (incf theta (* delta omega)))

(animate "pendulum" 79
    ()
  (move 250 250 0)
  (let ((pos-x (* l (sin theta)))
        (pos-y (* l (cos theta))))
    (line 0 0 0 pos-x pos-y 0)
    (sphere pos-x pos-y 0 30))
  (dotimes (i 450)
    (next-step)))
