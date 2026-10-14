(in-package #:borba-cad/tests)

(def-suite viewport
  :description "World/screen transforms, zoom, pan, fitting and the adaptive grid."
  :in all)

(in-suite viewport)

(defconstant +round-trip-tolerance+ 1d-8
  "Absolute world-length tolerance (mm) for world->screen->world round trips. The worst-case
rounding error is about |world - center| * 2.2e-16, at most 4.4e-9 mm for coordinates up to
20 km from the center; the tolerance is about twice that.")

(defun a-viewport (&key (center-x 0d0) (center-y 0d0) (scale 0.5d0) (width 1000d0) (height 600d0))
  "Build a viewport with convenient defaults."
  (view:make-viewport :center-x center-x :center-y center-y :scale scale :width width :height height))

(test the-canvas-center-shows-the-viewport-center
  (let ((viewport (a-viewport :center-x 1000d0 :center-y -500d0 :scale 2d0)))
    (multiple-value-bind (x y) (view:world->screen viewport 1000d0 -500d0)
      (is (= 500d0 x))
      (is (= 300d0 y)))))

(test world-y-is-up-and-screen-y-is-down
  (let ((viewport (a-viewport :scale 1d0)))
    (multiple-value-bind (x y) (view:world->screen viewport 100d0 50d0)
      (is (= 600d0 x))                       ; 500 + 100
      (is (= 250d0 y)))                      ; 300 - 50: a higher world y is a smaller screen y
    (multiple-value-bind (wx wy) (view:screen->world viewport 0d0 0d0)
      (is (= -500d0 wx))
      (is (= 300d0 wy)))))                   ; the top-left pixel is the top of the world view

(test lengths-convert-at-the-current-scale
  (let ((viewport (a-viewport :scale 0.25d0)))
    (is (= 25d0 (view:world-length->pixels viewport 100d0)))
    (is (= 400d0 (view:pixels->world-length viewport 100d0)))))

(test typed-points-convert-consistently-with-the-raw-functions
  (let* ((viewport (a-viewport :scale 0.7d0))
         (screen (view:world-point->screen viewport (view:make-world-point 1234d0 -567d0)))
         (back (view:screen-point->world viewport screen)))
    (is (typep screen 'view:screen-point))
    (is (typep back 'view:world-point))
    (is (approx= 1234d0 (view:world-point-x back)))
    (is (approx= -567d0 (view:world-point-y back)))))

(test world-screen-round-trip-stays-within-the-documented-tolerance
  (let ((prng (rng:make-prng 4)))
    (dotimes (i 3000)
      (let* ((scale (exp (rng:prng-next-double prng (log view:+minimum-scale+) (log view:+maximum-scale+))))
             (center-x (rng:prng-next-double prng -1d6 1d6))
             (center-y (rng:prng-next-double prng -1d6 1d6))
             (viewport (a-viewport :center-x center-x :center-y center-y :scale scale
                                   :width (rng:prng-next-double prng 200d0 3000d0)
                                   :height (rng:prng-next-double prng 200d0 2000d0)))
             (world-x (+ center-x (rng:prng-next-double prng -1d7 1d7)))
             (world-y (+ center-y (rng:prng-next-double prng -1d7 1d7))))
        (multiple-value-bind (sx sy) (view:world->screen viewport world-x world-y)
          (multiple-value-bind (x y) (view:screen->world viewport sx sy)
            (is (< (abs (- x world-x)) +round-trip-tolerance+))
            (is (< (abs (- y world-y)) +round-trip-tolerance+))))))))

(test viewport-construction-clamps-the-scale-and-validates-the-canvas
  (is (= view:+minimum-scale+ (view:viewport-scale (a-viewport :scale 1d-9))))
  (is (= view:+maximum-scale+ (view:viewport-scale (a-viewport :scale 1d9))))
  (signals type-error (a-viewport :width 0d0))
  (signals type-error (a-viewport :height -5d0))
  (signals type-error (a-viewport :scale 1))
  (let ((default (view:default-viewport 1000d0 600d0)))
    (is (= 0.05d0 (view:viewport-scale default)))               ; 1000 px for 20 m
    (is (= 0d0 (view:viewport-center-x default)))))

(test resizing-keeps-center-and-scale
  (let ((resized (view:resize-viewport (a-viewport :center-x 7d0 :scale 3d0) 800d0 400d0)))
    (is (= 7d0 (view:viewport-center-x resized)))
    (is (= 3d0 (view:viewport-scale resized)))
    (is (= 800d0 (view:viewport-width resized)))))

(test panning-moves-the-content-by-exactly-the-dragged-pixels
  (let* ((viewport (a-viewport :center-x 100d0 :center-y 200d0 :scale 0.37d0))
         (panned (view:pan-viewport viewport 55d0 -20d0)))
    (multiple-value-bind (x0 y0) (view:world->screen viewport 321d0 654d0)
      (multiple-value-bind (x1 y1) (view:world->screen panned 321d0 654d0)
        (is (approx= (+ x0 55d0) x1))
        (is (approx= (- y0 20d0) y1))))))

(test zoom-keeps-the-world-point-under-the-cursor-fixed
  (let ((prng (rng:make-prng 6)))
    (dotimes (i 2000)
      (let* ((viewport (a-viewport :center-x (rng:prng-next-double prng -5000d0 5000d0)
                                   :center-y (rng:prng-next-double prng -5000d0 5000d0)
                                   :scale (exp (rng:prng-next-double prng (log 1d-3) (log 100d0)))))
             (factor (exp (rng:prng-next-double prng (log 0.2d0) (log 5d0))))
             (anchor-x (rng:prng-next-double prng 0d0 1000d0))
             (anchor-y (rng:prng-next-double prng 0d0 600d0))
             (zoomed (view:zoom-viewport-at viewport factor anchor-x anchor-y)))
        (multiple-value-bind (before-x before-y) (view:screen->world viewport anchor-x anchor-y)
          (multiple-value-bind (after-x after-y) (view:screen->world zoomed anchor-x anchor-y)
            (is (< (abs (- before-x after-x)) +round-trip-tolerance+))
            (is (< (abs (- before-y after-y)) +round-trip-tolerance+))))))))

(test zoom-scales-by-the-factor-and-respects-both-limits
  (let ((viewport (a-viewport :scale 1d0)))
    (is (approx= 2d0 (view:viewport-scale (view:zoom-viewport-at viewport 2d0 10d0 10d0))))
    (is (approx= 0.5d0 (view:viewport-scale (view:zoom-viewport-at viewport 0.5d0 10d0 10d0)))))
  (let* ((at-maximum (a-viewport :scale view:+maximum-scale+))
         (zoomed (view:zoom-viewport-at at-maximum 10d0 300d0 200d0)))
    (is (= view:+maximum-scale+ (view:viewport-scale zoomed)))
    (multiple-value-bind (before-x before-y) (view:screen->world at-maximum 300d0 200d0)
      (multiple-value-bind (after-x after-y) (view:screen->world zoomed 300d0 200d0)
        (is (approx= before-x after-x 1d-12))                  ; the anchor holds even when clamped
        (is (approx= before-y after-y 1d-12)))))
  (let ((at-minimum (a-viewport :scale view:+minimum-scale+)))
    (is (= view:+minimum-scale+ (view:viewport-scale (view:zoom-viewport-at at-minimum 0.1d0 0d0 0d0))))))

(test fitting-shows-the-bounds-centered-with-the-requested-margin
  (let* ((viewport (a-viewport :width 1000d0 :height 600d0))
         (bounds (geo:make-bounds 0d0 0d0 10000d0 2000d0))
         (fitted (view:fit-viewport-to-bounds viewport bounds :margin-pixels 50d0)))
    ;; Width-limited: (1000 - 100) px for 10000 mm.
    (is (approx= 0.09d0 (view:viewport-scale fitted)))
    (is (= 5000d0 (view:viewport-center-x fitted)))
    (is (= 1000d0 (view:viewport-center-y fitted)))
    (multiple-value-bind (left top) (view:world->screen fitted 0d0 2000d0)
      (is (approx= 50d0 left))
      (is (> top 50d0))))                                      ; height-slack: more room above
  (let ((tall (view:fit-viewport-to-bounds (a-viewport :width 1000d0 :height 600d0)
                                           (geo:make-bounds 0d0 0d0 100d0 5000d0)
                                           :margin-pixels 0d0)))
    (is (approx= (/ 600d0 5000d0) (view:viewport-scale tall)))))

(test fitting-handles-degenerate-bounds-without-dividing-by-zero
  (let ((fitted (view:fit-viewport-to-bounds (a-viewport) (geo:make-bounds 5d0 5d0 5d0 5d0))))
    (is (= view:+maximum-scale+ (view:viewport-scale fitted)))
    (is (= 5d0 (view:viewport-center-x fitted)))))

(test visible-bounds-match-the-canvas-corners
  (let* ((viewport (a-viewport :center-x 100d0 :center-y 50d0 :scale 2d0))
         (bounds (view:viewport-visible-bounds viewport))
         (padded (view:viewport-visible-bounds viewport 10d0)))
    (is (= -150d0 (geo:bounds-min-x bounds)))
    (is (= 350d0 (geo:bounds-max-x bounds)))
    (is (= -100d0 (geo:bounds-min-y bounds)))
    (is (= 200d0 (geo:bounds-max-y bounds)))
    (is (= -155d0 (geo:bounds-min-x padded)))
    (is (= 205d0 (geo:bounds-max-y padded)))))

;;; Adaptive grid

(test grid-level-keeps-the-base-spacing-when-it-is-wide-enough
  (let ((level (view:compute-grid-level (a-viewport :scale 0.5d0) 100d0)))     ; 50 px
    (is (= 100d0 (view:grid-level-spacing level)))
    (is (= view:+grid-major-interval+ (view:grid-level-major-interval level)))))

(test grid-level-follows-the-1-2-5-ladder-when-zoomed-out
  (flet ((spacing (scale) (view:grid-level-spacing (view:compute-grid-level (a-viewport :scale scale) 100d0))))
    (is (= 100d0 (spacing 0.08d0)))        ; exactly 8 px: still wide enough
    (is (= 200d0 (spacing 0.079d0)))       ; 7.9 px: next rung
    (is (= 500d0 (spacing 0.03d0)))        ; 100 x 2 = 200 -> 6 px, so x5 -> 15 px
    (is (= 1000d0 (spacing 0.01d0)))
    (is (= 100000d0 (spacing 0.0001d0)))))

(test grid-level-property-spacing-is-wide-enough-and-minimal
  (let ((prng (rng:make-prng 8))
        (ladder '(1 2 5 10 20 50 100 200 500 1000 2000 5000 10000 20000 50000 100000)))
    (dotimes (i 500)
      (let* ((scale (exp (rng:prng-next-double prng (log view:+minimum-scale+) (log view:+maximum-scale+))))
             (base (rng:prng-pick prng '(1d0 10d0 50d0 100d0 250d0 1000d0)))
             (spacing (view:grid-level-spacing (view:compute-grid-level (a-viewport :scale scale) base)))
             (multiplier (round spacing base))
             (rung (position multiplier ladder)))
        (is (>= (* spacing scale) view:+minimum-grid-spacing-pixels+))
        (is (integerp rung))
        ;; Minimal: the previous rung of the ladder would have been too narrow.
        (when (plusp rung)
          (is (< (* base (nth (1- rung) ladder) scale) view:+minimum-grid-spacing-pixels+)))))))

(test visible-grid-range-covers-exactly-the-lines-inside-the-canvas
  (let* ((viewport (a-viewport :center-x 0d0 :center-y 0d0 :scale 1d0 :width 1000d0 :height 600d0)))
    ;; The canvas spans x -500..500 and y -300..300, so with spacing 100 the lines are -5..5, -3..3.
    (multiple-value-bind (first-x last-x first-y last-y) (view:visible-grid-range viewport 100d0)
      (is (= -5 first-x)) (is (= 5 last-x))
      (is (= -3 first-y)) (is (= 3 last-y))))
  (let ((viewport (a-viewport :center-x 50d0 :center-y 50d0 :scale 1d0 :width 100d0 :height 100d0)))
    (multiple-value-bind (first-x last-x) (view:visible-grid-range viewport 100d0)
      (is (= 0 first-x)) (is (= 1 last-x)))))                  ; x from 0 to 100 inclusive: lines 0 and 1

;;; Canvas origin inside the window

(test the-canvas-origin-offsets-window-coordinates
  (let ((viewport (view:make-viewport :center-x 0d0 :center-y 0d0 :scale 1d0
                                      :width 1000d0 :height 600d0 :origin-x 0d0 :origin-y 40d0)))
    (multiple-value-bind (x y) (view:world->screen viewport 0d0 0d0)
      (is (= 500d0 x))
      (is (= 340d0 y)))                                   ; 40 + 600 / 2
    (multiple-value-bind (wx wy) (view:screen->world viewport 500d0 340d0)
      (is (= 0d0 wx))
      (is (= 0d0 wy)))
    (let ((bounds (view:viewport-visible-bounds viewport)))
      (is (= -500d0 (geo:bounds-min-x bounds)))
      (is (= 300d0 (geo:bounds-max-y bounds)))
      (is (= -300d0 (geo:bounds-min-y bounds))))))

(test zoom-pan-and-resize-keep-the-canvas-origin
  (let* ((viewport (view:make-viewport :center-x 10d0 :center-y 20d0 :scale 0.5d0
                                       :width 800d0 :height 500d0 :origin-x 12d0 :origin-y 40d0))
         (zoomed (view:zoom-viewport-at viewport 2d0 300d0 200d0))
         (panned (view:pan-viewport zoomed 10d0 5d0))
         (resized (view:resize-viewport panned 900d0 450d0)))
    (is (= 12d0 (view:viewport-origin-x resized)))
    (is (= 40d0 (view:viewport-origin-y resized)))
    ;; The world point under the anchor stays under it when the canvas is offset.
    (multiple-value-bind (before-x before-y) (view:screen->world viewport 300d0 200d0)
      (multiple-value-bind (after-x after-y) (view:screen->world zoomed 300d0 200d0)
        (is (approx= before-x after-x 1d-12))
        (is (approx= before-y after-y 1d-12))))
    (let ((moved (view:resize-viewport viewport 800d0 500d0 :origin-x 0d0 :origin-y 0d0)))
      (is (= 0d0 (view:viewport-origin-y moved))))))

(test the-world-screen-round-trip-holds-with-an-origin
  (let ((prng (rng:make-prng 14)))
    (dotimes (i 500)
      (let* ((viewport (view:make-viewport :center-x (rng:prng-next-double prng -1d5 1d5)
                                           :center-y (rng:prng-next-double prng -1d5 1d5)
                                           :scale (exp (rng:prng-next-double prng (log 1d-3) (log 100d0)))
                                           :width 900d0 :height 600d0
                                           :origin-x (rng:prng-next-double prng 0d0 300d0)
                                           :origin-y (rng:prng-next-double prng 0d0 100d0)))
             (wx (rng:prng-next-double prng -1d6 1d6))
             (wy (rng:prng-next-double prng -1d6 1d6)))
        (multiple-value-bind (sx sy) (view:world->screen viewport wx wy)
          (multiple-value-bind (x y) (view:screen->world viewport sx sy)
            (is (< (abs (- x wx)) +round-trip-tolerance+))
            (is (< (abs (- y wy)) +round-trip-tolerance+))))))))
