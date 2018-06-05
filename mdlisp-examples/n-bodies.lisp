(defparameter bodies)

(with-args ((body body) x y z r)
  (animate "n-bodies" 100
      ()
    (dolist (body bodies)
      (sphere x y z r))))
