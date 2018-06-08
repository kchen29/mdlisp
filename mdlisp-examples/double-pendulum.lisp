(defparameter delta .007)
(defparameter g 9.81)

(defparameter m1 10)
(defparameter m2 5)
(defparameter l1 150)
(defparameter l2 80)

(defparameter theta1 (/ pi 2))
(defparameter theta2 pi)
(defparameter omega1 0)
(defparameter omega2 0)

(defun square (x)
  (* x x))

;;http://www.physicsandbox.com/projects/double-pendulum.html
(defun next-step ()
  (let* ((s1 (sin theta1))
         (s2 (sin theta2))
         (del (- theta1 theta2))
         (cd (cos del))
         (sd (sin del))
         (mu (+ 1 (/ m1 m2)))
         (sq1 (* l1 (square omega1)))
         (sq2 (* l2 (square omega2)))
         (denom (- mu (square cd)))
         (alpha1t1 (* g (- (* s2 cd) (* mu s1))))
         (alpha1t2 (* sd (+ sq2 (* sq1 cd))))
         (alpha1 (/ (- alpha1t1 alpha1t2) l1 denom))
         (alpha2t1 (* g mu (- (* s1 cd) s2)))
         (alpha2t2 (* sd (+ (* mu sq1) (* sq2 cd))))
         (alpha2 (/ (+ alpha2t1 alpha2t2) l2 denom)))
    (incf omega1 (* delta alpha1))
    (incf omega2 (* delta alpha2))
    (incf theta1 (* delta omega1))
    (incf theta2 (* delta omega2))))

(animate "double-pendulum" 800
    ()
  (move 250 250 0)
  (let* ((x1 (* l1 (sin theta1)))
         (y1 (* l1 (- (cos theta1))))
         (x2 (+ x1 (* l2 (sin theta2))))
         (y2 (+ y1 (* l2 (- (cos theta2))))))
    (line 0 0 0 x1 y1 0)
    (sphere x1 y1 0 30)
    (line x1 y1 0 x2 y2 0)
    (sphere x2 y2 0 20))
  (dotimes (i 100)
    (next-step)))