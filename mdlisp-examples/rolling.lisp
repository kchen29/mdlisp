(animate "rolling" 65
    ((quad (/ (* frame frame) (* 65 65))))
  (line 0 375 0 500 0 0)
  (move -75 526.25 0)
  (move 650 -487.5 0 quad)
  (rotate z -360 quad)
  (sphere 0 0 0 75))
