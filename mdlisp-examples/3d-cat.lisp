(load "mdlisp-examples/anaglyph.lisp")

(defmacro temp (&body body)
  `(progn (push-stack)
          ,@body
          (pop-stack)))
(defmacro part (name &body body)
  (declare (ignore name))
  `(temp ,@body))
(defun ellipsoid (x y z r sx sy sz)
  (temp
   (scale sx sy sz)
   (sphere x y z r)))

(animate "cat" 60
    ((body (vary (0 14 0 1)
                 (15 44 1 -1)
                 (45 59 -1 0)))
     (fe (vary (0 14 0 1)
               (15 29 1 1)
               (30 59 1 0)))
     (flr (vary (0 14 0 -1)
                (15 29 -1 1)
                (30 44 1 1)
                (45 59 1 0)))
     (frr (vary (0 9 0 -1)
                (10 24 -1 1)
                (25 39 1 1)
                (40 59 1 0)))
     (rc (vary (0 29 0 1)
               (30 44 1 0)
               (45 59 0 0)))
     (rlr (vary (0 29 0 1)
                (29 44 1 0)
                (45 59 0 0)))
     (rrr (vary (0 34 0 1)
                (35 49 1 0)
                (50 59 0 0))))
  (with-anaglyph 2 2.6
    (part body1
      (move 220 270 0)
      (rotate y 5)
      (rotate x 5)
      (rotate z 17 body)
      (ellipsoid 0 0 0 62 2 1 1)
      (part body2
        (move -95 15 0)
        (temp
          (rotate z -10)
          (ellipsoid 0 0 0 62 1.16 .85 1))
        (part left-arm1
          (move -20 30 55)
          (rotate z 30 flr)
          (rotate z -10)
          (ellipsoid 0 -27 0 27 1 2.4 1)
          (part left-arm2
            (move 0 -120 0)
            (rotate z 108 fe)
            (rotate z -18)
            (ellipsoid -14 0 0 14 3.5 1 1)))
        (part right-arm1
          (move -20 30 -55)
          (rotate z 30 frr)
          (rotate z -10)
          (ellipsoid 0 -27 0 27 1 2.4 1)
          (part right-arm2
            (move 0 -120 0)
            (rotate z 108 fe)
            (rotate z -18)
            (ellipsoid -14 0 0 14 3.5 1 1)))
        (part head
          (move -65 15 0)
          (rotate z -17 body)
          (ellipsoid 0 0 0 50 1 1 1.1)
          (part muzzle
            (move -38 -25 0)
            (rotate z -25)
            (ellipsoid 0 0 0 10 2.5 1 3.5))
          (part left-ear
            (move 5 34 45)
            (rotate x 20)
            (rotate z -10)
            (ellipsoid 0 0 0 11 1 2.1 1))
          (part right-ear
            (move 0 34 -45)
            (rotate x -20)
            (rotate z -10)
            (ellipsoid 0 0 0 11 1 2.1 1))))
      (part tail1
        (move 118 40 0)
        (temp
          (rotate z -5)
          (ellipsoid 0 0 0 14 4.6 1 1))
        (part tail2
          (move 65 -3 0)
          (temp
            (rotate z 4)
            (ellipsoid 0 0 0 11 3.37 1 1))
          (part tail3
            (move 46 0 0)
            (rotate z -10)
            (ellipsoid 0 0 0 8 3.4 1 1))))
      (part left-leg1
        (move 100 30 50)
        (rotate z -75 rlr)
        (rotate z 35)
        (ellipsoid 0 -40 0 40 1 1.4 1)
        (part left-leg2
          (move -10 -95 0)
          (rotate z 50 rc)
          (rotate z -20)
          (ellipsoid 30 0 0 30 2 1 1)
          (part left-foot
            (move 120 0 0)
            (rotate z -110 rc)
            (rotate z 70)
            (ellipsoid 0 -14 0 14 1 4 1))))
      (part right-leg1
        (move 100 30 -50)
        (rotate z -75 rrr)
        (rotate z 35)
        (ellipsoid 0 -40 0 40 1 1.4 1)
        (part right-leg2
          (move -10 -95 0)
          (rotate z 50 rc)
          (rotate z -20)
          (ellipsoid 30 0 0 30 2 1 1)
          (part right-foot
            (move 120 0 0)
            (rotate z -110 rc)
            (rotate z 70)
            (ellipsoid 0 -14 0 14 1 4 1)))))))

