;;;; 3d anaglyph code
(defparameter screen-buffer (make-array '(500 500) :initial-element '(0 0 0)))

(defun combine ()
  "Combines SCREEN-BUFFER with *SCREEN* to form an anaglyph."
  ;;;left is only red, right is only cyan
  (dotimes (x +screen-side+)
    (dotimes (y +screen-side+)
      (setf (aref *screen* x y)
            (list (first (aref screen-buffer x y))
                  (second (aref *screen* x y))
                  (third (aref *screen* x y)))))))

(defmacro with-anaglyph (eye-distance angle &body body)
  "Given the distance from the eye to the center of the eyes,
   the angle each eye makes towards the center,
   and the body of the code, generate an anaglyph."
  ;;;produce left and right images
  `(progn
     ;;have screen-buffer hold the left image
     (let ((*screen* screen-buffer))
       (clear-stack)
       (clear-screen)
       (move ,eye-distance 0 0)
       (rotate y ,angle)
       ,@body)
     (clear-stack)
     (clear-screen)
     ;;*screen* holds the right image
     (move (- ,eye-distance) 0 0)
     (rotate y (- ,angle))
     ,@body
     ;;now combine the images to *screen*
     (combine)))


