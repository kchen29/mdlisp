(defmacro part (name &body body)
  (declare (ignore name))
  `(progn (push)
          ,@body
          (pop)))
(mdlisp-script
 (part body
   (move 250 250 0)
   (box -50 63 25 100 125 50)
   (part head
     (move 0 175 0)
     (rotate y 90)
     (sphere 0 0 0 50))
   (part left-arm
     (move -100 125 0)
     (rotate z -135)
     (rotate y 20)
     (box -40 0 40 40 100 80)
     (part left-lower-arm
       (move -20 -100 0)
       (box -10 0 10 20 125 20)))
   (part right-arm
     (move 100 125 0)
     (rotate z 135)
     (rotate y 20)
     (box 0 0 40 40 100 80)
     (part right-lower-arm
       (move 20 -100 0)
       (box -10 0 10 20 125 20)))
   (part left-leg
     (move -100 -125 0)
     (rotate z -45)
     (rotate y 20)
     (box 0 0 40 50 120 80))
   (part right-leg
     (move 100 -125 0)
     (rotate z 45)
     (rotate y 20)
     (box -50 0 40 50 120 80)))
 (display)
 (save robot.png))
