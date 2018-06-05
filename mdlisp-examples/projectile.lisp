(animate "projectile" 50
    ((x (vary (0 49 0 500)))
     (y (+ 500 (/ (* -500 (- frame 24.5) (- frame 24.5)) 600.25))))
  (sphere x y 0 30))
