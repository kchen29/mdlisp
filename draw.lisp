;;;; Draw to screen.
(defparameter shading 'flat)

(defun draw-line (x0 y0 z0 x1 y1 z1 color)
  "Draws a line from (x0, y0) to (x1, y1) on *SCREEN* using COLOR."
  (declare (optimize (speed 3) (safety 0)))
  (roundify x0 y0 x1 y1)
  (setf z0 (float z0)
        z1 (float z1))
  (sortify 0 (x0 y0 z0) (x1 y1 z1))
  (let* ((xdif (- x1 x0))
         (ydif (- y1 y0))
         (zdif (- z1 z0))
         (aydif (abs ydif)))
    (declare (type fixnum x0 y0 x1 y1 xdif ydif aydif))
    (cond
      ((and (zerop xdif) (zerop ydif)) (plot x0 y0 (float (max z0 z1)) color))
      ((< aydif xdif)
       (do ((x x0 (1+ x))
            (y-s (signum ydif))
            (as (* 2 aydif))
            (ad (* 2 xdif))
            (a 0 (+ a as))
            (y y0)
            (z-s (/ zdif (float xdif)))
            (z z0 (+ z z-s)))
           ((> x x1))
         (declare (type fixnum x y a as))
         (when (>= a xdif)
           (incf y y-s)
           (decf a ad))
         (plot x y z color)))
      (t (do ((as (* 2 xdif))
              (ad (* 2 aydif))
              (a 0 (+ a as))
              (x x0)
              (y-s (signum ydif))
              (y y0 (+ y y-s))
              (z-s (/ zdif (float aydif)))
              (z z0 (+ z z-s)))
             ((= y y1) (plot x y z color))
           (declare (type fixnum x y a as))
           (when (>= a aydif)
             (incf x)
             (decf a ad))
           (plot x y z color))))))

(defun draw-line-index (edges index color)
  "Draws the line starting from INDEX in EDGES."
  (macrolet-helper
    `(draw-line
      ,@(generate ((in 2) (co 3))
          `(mref edges ,co (+ ,in index)))
      color)))

(defun draw-lines (edges color)
  "Draws the lines from EDGES to *SCREEN* with COLOR."
  (do-step (index (m-last-col edges) 2)
    (draw-line-index edges index color)))

;;;3d shapes
(defun draw-polygon (x0 y0 z0 x1 y1 z1 x2 y2 z2 color)
  "Draws the polygon to *SCREEN*."
  (draw-line x0 y0 z0 x1 y1 z1 color)
  (draw-line x0 y0 z0 x2 y2 z2 color)
  (draw-line x1 y1 z1 x2 y2 z2 color)
  (scanline x0 y0 z0 x1 y1 z1 x2 y2 z2 color))

(defun draw-polygon-index (polygons index color)
  "Draws the polygon starting from INDEX in POLYGONS"
  (macrolet-helper
    `(draw-polygon
      ,@(generate ((in 3) (co 3))
          `(mref polygons ,co (+ ,in index)))
      color)))

(defun draw-polygon-phong (x0 y0 z0 x1 y1 z1 x2 y2 z2 normals)
  "Draws the polygon at INDEX with NORMALS as the vector normals."
  (scanline-phong x0 y0 z0 (gethash (list x0 y0 z0) normals)
                  x1 y1 z1 (gethash (list x1 y1 z1) normals)
                  x2 y2 z2 (gethash (list x2 y2 z2) normals)))

(defun draw-polygon-index-phong (polygons index normals)
  "Draws the polygon starting from INDEX in POLYGONS with phong shading"
  (macrolet-helper
    `(draw-polygon-phong
      ,@(generate ((in 3) (co 3))
          `(mref polygons ,co (+ ,in index)))
      normals)))

(defun draw-polygons (polygons)
  "Draws the polygons from POLYGONS to *SCREEN*."
  (case shading
    (flat
     (do-step (index (m-last-col polygons) 3)
       (let ((normal (normal polygons index)))
         (when (plusp (nth 2 normal))
           (draw-polygon-index polygons index (calculate-color normal))))))
    (phong
     (let ((normals (get-vector-normals polygons)))
       (do-step (index (m-last-col polygons) 3)
         (let ((normal (normal polygons index)))
           (when (plusp (nth 2 normal))
             (draw-polygon-index-phong polygons index normals))))))
    (otherwise (error "Unknown shading: ~a" shading))))

(defun get-vector-normals (polygons)
  "Create the vector normal hashtable from POLYGONS."
  (let ((normals (make-hash-table :test 'equal)))
    (do-step (tr (m-last-col polygons) 3)
      (let ((normal (normalize (normal polygons tr))))
        (dotimes (i 3)
          (let* ((vector (list (mref polygons 0 (+ tr i))
                               (mref polygons 1 (+ tr i))
                               (mref polygons 2 (+ tr i))))
                 (value (gethash vector normals)))
            (setf (gethash vector normals) (if value (+v value normal) normal))))))
    (maphash (lambda (key value) (normalize value)) normals)
    normals))

(defun scanline (x0 y0 z0 x1 y1 z1 x2 y2 z2 color)
  "Does scanline conversion."
  (roundify y0 y1 y2)
  ;;have y0 be the bottom, y1 the middle, and y2 the top
  (sortify 1 (x0 y0 z0) (x1 y1 z1) (x2 y2 z2))
  (let ((ydif1 (- y1 y0))
        (ydif2 (- y2 y0))
        (ydif3 (- y2 y1)))
    (do ((y y0 (1+ y))
         (a-s (safe-diff-quot x2 x0 ydif2))
         (a x0 (+ a a-s))
         (b-s1 (safe-diff-quot x1 x0 ydif1))
         (b-s2 (safe-diff-quot x2 x1 ydif3))
         (b x0)
         (c-s (safe-diff-quot z2 z0 ydif2))
         (c z0 (+ c c-s))
         (d-s1 (safe-diff-quot z1 z0 ydif1))
         (d-s2 (safe-diff-quot z2 z1 ydif3))
         (d z0))
        ((>= y y2))
      (cond
        ((< y0 y y1) (incf b b-s1) (incf d d-s1))
        ((< y1 y y2) (incf b b-s2) (incf d d-s2))
        ((= y y1) (setf b x1 d z1)))
      (draw-line a y c b y d color))))

(defun scanline-phong (x0 y0 z0 n0 x1 y1 z1 n1 x2 y2 z2 n2)
  "Does scanline conversion with phong shading."
  (roundify y0 y1 y2)
  ;;have y0 be the bottom, y1 the middle, and y2 the top
  (sortify 1 (x0 y0 z0 n0) (x1 y1 z1 n1) (x2 y2 z2 n2))
  (let ((ydif1 (- y1 y0))
        (ydif2 (- y2 y0))
        (ydif3 (- y2 y1)))
    (do ((y y0 (1+ y))
         (a-s (safe-diff-quot x2 x0 ydif2))
         (a x0 (+ a a-s))
         (an-s (safe-diff-quot-v n2 n0 ydif2))
         (an n0 (+v an an-s))
         (b-s1 (safe-diff-quot x1 x0 ydif1))
         (b-s2 (safe-diff-quot x2 x1 ydif3))
         (b x0)
         (bn-s1 (safe-diff-quot-v n1 n0 ydif1))
         (bn-s2 (safe-diff-quot-v n2 n1 ydif3))
         (bn n0)
         (c-s (safe-diff-quot z2 z0 ydif2))
         (c z0 (+ c c-s))
         (d-s1 (safe-diff-quot z1 z0 ydif1))
         (d-s2 (safe-diff-quot z2 z1 ydif3))
         (d z0))
        ((>= y y2))
      (cond
        ((< y0 y y1) (incf b b-s1) (incf d d-s1) (setf bn (+v bn bn-s1)))
        ((< y1 y y2) (incf b b-s2) (incf d d-s2) (setf bn (+v bn bn-s2)))
        ((= y y1) (setf b x1 d z1 bn n1)))
      (draw-scanline-phong y a c an b d bn))))

;;Destructively modify later
(defun +v (v1 v2)
  "Returns a fresh list of the addition for V2 with V1."
  (mapcar (lambda (x y)
            (+ x y))
          v1 v2))

(defun safe-diff-quot-v (n1 n0 denom)
  "SAFE-DIFF-QUOT but for lists. Returns a fresh list."
  (mapcar (lambda (x y)
            (safe-diff-quot x y denom))
          n1 n0))

(defun draw-scanline-phong (y x0 z0 n0 x1 z1 n1)
  "Draw the scanline for phong shading given the points and their vector normals."
  (sortify 0 (x0 z0 n0) (x1 z1 n1))
  (let ((xdif (- x1 x0)))
    (do ((x (round x0) (1+ x))
         (z-s (safe-diff-quot z1 z0 xdif))
         (z z0 (+ z z-s))
         (n-s (safe-diff-quot-v n1 n0 xdif))
         (n n0 (+v n0 n-s)))
        ((> x x1))
      (plot x y z (calculate-color n)))))
