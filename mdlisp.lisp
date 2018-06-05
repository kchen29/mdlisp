;;;;Definitions for MDLISP.
(defparameter step-size 10)

(defparameter stack (list (make-transform-matrix)))
(defparameter edges (make-matrix))
(defparameter polygons (make-matrix))

;;helper functions
(defun post-add-edges ()
  (matrix-multiply (car stack) edges)
  (draw-lines edges '(255 0 255))
  (clear-matrix edges))

(defun post-add-polygons ()
  (matrix-multiply (car stack) polygons)
  (draw-polygons polygons)
  (clear-matrix polygons))

(defun update-stack (transform)
  (push (matrix-multiply (pop stack) transform) stack))

(defun clear-stack ()
  (setf stack (list (make-transform-matrix))))

;;commands
(defun push-stack ()
  (push (copy-matrix (car stack)) stack))

(defun pop-stack ()
  (pop stack))

(defun move (x y z &optional knob)
  (if knob
      (update-stack (make-translate (* x knob)
                                    (* y knob)
                                    (* z knob)))
      (update-stack (make-translate x y z))))

(defun scale (x y z &optional knob)
  (if knob
      (update-stack (make-scale (* x knob)
                                (* y knob)
                                (* z knob)))
      (update-stack (make-scale x y z))))

;;for convenience
(defmacro rotate (axis &body body)
  `(rotate-fun ',axis ,@body))

(defun rotate-fun (axis degrees &optional knob)
  (if knob
      (update-stack (make-rotate axis (* degrees knob)))
      (update-stack (make-rotate axis degrees))))

(defun sphere (x y z r)
  (add-sphere polygons step-size x y z r)
  (post-add-polygons))

(defun torus (x y z r1 r2)
  (add-torus polygons step-size x y z r1 r2)
  (post-add-polygons))

(defun box (x y z width height depth)
  (add-box polygons x y z width height depth)
  (post-add-polygons))

(defun line (x0 y0 z0 x1 y1 z1)
  (add-edge edges x0 y0 z0 x1 y1 z1)
  (post-add-edges))

;;;animation
(defmacro animate (basename frames varys &body body)
  "Animate macro. Given BASENAME, FRAMES, VARYS, and BODY, create an animation.
   Each vary in VARYS is a list of a symbol name followed by its value for each frame.
   The current frame is available to use in VARYS and BODY."
  (let ((frames-digs (gensym)))
    `(let ((,frames-digs (integer-digits (1- ,frames))))
       (dotimes (frame ,frames)
         (let ,varys
           (format t "Frame ~a~%" frame)
           (clear-stack)
           (clear-screen)
           ,@body
           (save-frame ,basename frame ,frames-digs)))
       (make-animation ,basename))))
