(animate "simple" 100 
    ((spinny (vary (0 99 0 1)))
     (bigenator (vary (0 49 0 1)
                      (50 99 1 0))))
  (move 250 250 0)
  (scale 2 2 2 bigenator)
  (rotate y 360 spinny)
  (sphere 0 0 0 100))
