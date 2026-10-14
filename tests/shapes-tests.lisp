(in-package #:borba-cad/tests)

(def-suite shapes
  :description "Thick strokes, discs, rings, rounded rectangles, shadows, gradients and dashes."
  :in all)

(in-suite shapes)

(defun shape-list ()
  "Return a fresh draw list whose white texel is a recognizable point."
  (let ((list (scene:make-draw-list)))
    (setf (scene:draw-list-white-u list) 0.25f0
          (scene:draw-list-white-v list) 0.25f0)
    list))

(defun shape-vertex (list
                     index
                     offset)
  "Return component OFFSET (0 x, 1 y, 4 r, 5 g, 6 b, 7 a) of vertex INDEX of LIST."
  (aref (scene:draw-list-vertices list) (+ offset (* index scene:+floats-per-vertex+))))

(defun filled-area (list)
  "Return the total area of the triangles of LIST, as a double."
  (let ((area 0d0))
    (loop for first from 0 below (scene:draw-list-vertex-count list) by 3
          do (let ((x1 (shape-vertex list first 0))
                   (y1 (shape-vertex list first 1))
                   (x2 (shape-vertex list (+ first 1) 0))
                   (y2 (shape-vertex list (+ first 1) 1))
                   (x3 (shape-vertex list (+ first 2) 0))
                   (y3 (shape-vertex list (+ first 2) 1)))
               (incf area (* 0.5d0 (abs (- (* (- x2 x1) (- y3 y1)) (* (- x3 x1) (- y2 y1))))))))
    area))

(defun relative-error (expected
                       actual)
  "Return |ACTUAL - EXPECTED| as a fraction of EXPECTED."
  (/ (abs (- actual expected)) expected))

(defun within-chordal-error-p (expected
                               actual
                               curved-length)
  "Return true when ACTUAL differs from EXPECTED by no more than a strip as wide as the shapes'
chordal error along CURVED-LENGTH, which is the most that approximating curves by chords can
change an area, plus a thousandth of the area for rounding."
  (<= (abs (- actual expected))
      (+ (* curved-length 0.15d0) (* 0.001d0 expected))))

(defparameter *shape-color* (scene:make-rgba 1f0 0.5f0 0.25f0 1f0)
  "A color with distinct channels.")

(test a-thick-line-is-a-quad-of-the-right-area-and-width
  (let ((list (shape-list)))
    (scene:add-thick-line list 0f0 0f0 10f0 0f0 2f0 *shape-color*)
    (is (= 6 (scene:draw-list-vertex-count list)))
    (is (< (relative-error 20d0 (filled-area list)) 1d-6))
    (let ((ys (loop for index below 6 collect (shape-vertex list index 1))))
      (is (= -1f0 (reduce #'min ys)))
      (is (= 1f0 (reduce #'max ys)))))
  (let ((list (shape-list)))
    (scene:add-thick-line list 0f0 0f0 3f0 4f0 3f0 *shape-color*)
    (is (< (relative-error 15d0 (filled-area list)) 1d-5) "a 3-4-5 stroke of width 3 covers 5 x 3"))
  (let ((list (shape-list)))
    (scene:add-thick-line list 5f0 5f0 5f0 5f0 4f0 *shape-color*)
    (is (zerop (scene:draw-list-vertex-count list)) "a zero-length stroke draws nothing")))

(test a-disc-covers-pi-r-squared
  (dolist (radius '(3f0 10f0 50f0 400f0))
    (let ((list (shape-list)))
      (scene:add-disc list 100f0 100f0 radius *shape-color*)
      (is (within-chordal-error-p (* pi radius radius) (filled-area list) (* 2 pi radius))
          "radius ~a" radius)
      (is (= (* 3 (scene:shape-segments radius)) (scene:draw-list-vertex-count list))))))

(test a-ring-covers-the-area-between-its-two-circles
  (let ((list (shape-list)))
    (scene:add-ring list 0f0 0f0 40f0 4f0 *shape-color*)
    (is (< (relative-error (* pi (- (* 42 42) (* 38 38))) (filled-area list)) 0.01d0))))

(test an-arc-stroke-covers-its-share-of-the-ring
  (let ((list (shape-list)))
    (scene:add-arc-stroke list 0f0 0f0 40f0 0f0 (float pi 1f0) 4f0 *shape-color*)
    (is (< (relative-error (* 0.5 pi (- (* 42 42) (* 38 38))) (filled-area list)) 0.02d0))))

(test a-rounded-rectangle-is-the-rectangle-minus-its-four-corners
  (let ((list (shape-list)))
    (scene:add-rounded-rectangle list 10f0 20f0 100f0 60f0 10f0 *shape-color*)
    (is (within-chordal-error-p (- 6000 (* (- 4 pi) 100)) (filled-area list) (* 2 pi 10))))
  (let ((list (shape-list)))
    ;; A radius larger than half the smaller side is limited to it: a stadium.
    (scene:add-rounded-rectangle list 0f0 0f0 30f0 20f0 99f0 *shape-color*)
    (is (within-chordal-error-p (- 600 (* (- 4 pi) 100)) (filled-area list) (* 2 pi 10))))
  (let ((list (shape-list)))
    (scene:add-rounded-rectangle list 0f0 0f0 40f0 20f0 0f0 *shape-color*)
    (is (< (relative-error 800 (filled-area list)) 1d-6) "radius zero is a plain rectangle")))

(test the-outline-points-stay-inside-the-rectangle
  (let ((points (scene:rounded-rectangle-points 10f0 20f0 100f0 60f0 12f0)))
    (is (every (lambda (point) (<= 10f0 (car point) 110f0)) points))
    (is (every (lambda (point) (<= 20f0 (cdr point) 80f0)) points))
    (is (zerop (mod (length points) 4)))))

(test a-rounded-outline-covers-the-ring-between-two-rounded-rectangles
  (let ((list (shape-list)))
    (scene:add-rounded-rectangle-outline list 10f0 20f0 100f0 60f0 10f0 2f0 *shape-color*)
    (is (< (relative-error (- (- 6000 (* (- 4 pi) 100)) (- 5376 (* (- 4 pi) 64))) (filled-area list))
           0.01d0))))

(defun rounded-area (width
                     height
                     radius)
  "Return the area of a rectangle with rounded corners, as a double."
  (let ((clamped (max 0 (min radius (/ width 2) (/ height 2)))))
    (- (* width height) (* (- 4 pi) clamped clamped))))

(test rounded-outlines-pair-their-points-for-every-radius-and-thickness
  ;; The outer and inner outlines must have the same number of points, or the ring between them
  ;; is stitched with the wrong neighbours. Small radii, where the inner corner is almost square,
  ;; broke that once.
  (dolist (radius '(0.5 1 2 3 5 8 12 24))
    (dolist (thickness '(1 1.5 1.8 2.5 4))
      (let ((list (shape-list))
            (expected (- (rounded-area 120 80 radius)
                         (rounded-area (- 120 (* 2 thickness))
                                       (- 80 (* 2 thickness))
                                       (max 0 (- radius thickness))))))
        (scene:add-rounded-rectangle-outline list
                                             10f0
                                             20f0
                                             120f0
                                             80f0
                                             (float radius 1f0)
                                             (float thickness 1f0)
                                             *shape-color*)
        (is (< (relative-error expected (filled-area list)) 0.02d0)
            "radius ~a with thickness ~a" radius thickness)))))

(test a-gradient-rectangle-shades-from-top-to-bottom
  (let ((list (shape-list))
        (top (scene:make-rgba 1f0 0f0 0f0 1f0))
        (bottom (scene:make-rgba 0f0 0f0 1f0 0.5f0)))
    (scene:add-gradient-rectangle list 0f0 0f0 20f0 10f0 top bottom)
    (is (< (relative-error 200 (filled-area list)) 1d-6))
    (dotimes (index 6)
      (if (= 0f0 (shape-vertex list index 1))
          (is (= 1f0 (shape-vertex list index 4)) "top vertices are red")
          (is (= 1f0 (shape-vertex list index 6)) "bottom vertices are blue")))))

(test a-shadow-grows-and-fades-outward
  (let ((list (shape-list))
        (color (scene:make-rgba 0f0 0f0 0f0 0.5f0)))
    (scene:add-shadow list 100f0 100f0 200f0 100f0 8f0 20f0 color)
    (let* ((count (scene:draw-list-vertex-count list))
           (alphas (loop for index below count collect (shape-vertex list index 7)))
           (xs (loop for index below count collect (shape-vertex list index 0))))
      (is (plusp count))
      (is (<= (reduce #'max alphas) 0.5f0) "never more opaque than asked")
      (is (< (reduce #'min alphas) (reduce #'max alphas)) "it fades")
      (is (< (reduce #'min xs) 100f0) "it reaches beyond the rectangle on the left")
      (is (> (reduce #'max xs) 300f0) "and on the right"))))

(test a-dashed-line-has-the-right-number-of-dashes-and-stays-on-its-line
  (let ((list (shape-list)))
    (scene:add-dashed-line list 0f0 0f0 100f0 0f0 6f0 4f0 *shape-color*)
    (is (= 20 (scene:draw-list-vertex-count list)) "ten dashes of two vertices")
    (is (every (lambda (index) (= 0f0 (shape-vertex list index 1))) (loop for i below 20 collect i)))
    (is (= 96f0 (shape-vertex list 19 0)) "the last dash ends at 96: 9 periods of 10 plus a dash"))
  (let ((list (shape-list)))
    (scene:add-dashed-line list 0f0 0f0 0f0 0f0 6f0 4f0 *shape-color*)
    (is (zerop (scene:draw-list-vertex-count list)))))

(test a-polygon-fan-covers-its-area
  (let ((list (shape-list)))
    (scene:add-polygon list
                       (list (cons 0f0 0f0) (cons 10f0 0f0) (cons 10f0 10f0) (cons 0f0 10f0))
                       *shape-color*)
    (is (< (relative-error 100 (filled-area list)) 1d-6))))

(test round-shapes-use-more-segments-when-bigger-but-within-the-limits
  (let ((counts (mapcar #'scene:shape-segments '(0.1f0 1f0 5f0 20f0 100f0 1000f0 100000f0))))
    (is (apply #'<= counts))
    (is (= scene:+minimum-shape-segments+ (first counts)))
    (is (= scene:+maximum-shape-segments+ (car (last counts))))))

(test with-alpha-scales-the-opacity-only
  (let ((color (scene:with-alpha (scene:make-rgba 0.1f0 0.2f0 0.3f0 0.8f0) 0.5f0)))
    (is (= 0.1f0 (scene:rgba-r color)))
    (is (= 0.2f0 (scene:rgba-g color)))
    (is (= 0.3f0 (scene:rgba-b color)))
    (is (< (abs (- 0.4f0 (scene:rgba-a color))) 1f-6))))
