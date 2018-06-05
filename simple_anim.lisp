;;animate vary definitions specs are a work in progress
;; (animate simple 100 
;;   ((spinny 0 99 0 1)
;;    (bigenator 0 49 0 1)
;;    (bigenator 50 99 1 0))
;;   (move 250 250 0)
;;   (scale 2 2 2 bigenator)
;;   (rotate y 360 spinny)
;;   (sphere 0 0 0 100))

(animate "simple" 100 
    ((spinny (/ frame 99))
     (bigenator (if (< frame 50)
                    (/ frame 49)
                    (- 1 (/ (- frame 50) 49)))))
  (move 250 250 0)
  (scale 2 2 2 bigenator)
  (rotate y 360 spinny)
  (sphere 0 0 0 100))
