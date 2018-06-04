;;animation vary definitions specs are a work in progress
(animation simple 100 
  ((spinny 0 99 0 1)
   (bigenator 0 49 0 1)
   (bigenator 50 99 1 0))
  (move 250 250 0)
  (scale 2 2 2 bigenator)
  (rotate y 360 spinny)
  (sphere 0 0 0 100))
