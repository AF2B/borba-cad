(in-package #:borba-cad/tests)

(def-suite scene
  :description "The draw list and the scene builder."
  :in all)

(in-suite scene)

;;; Helpers

(defconstant +pixel-tolerance+ 1d-3
  "Tolerance in pixels for comparing single-float vertex coordinates with exact doubles.")

(defun scene-viewport (&key (center-x 0d0) (center-y 0d0) (scale 1d0) (width 1000d0) (height 600d0))
  "Build a viewport for scene tests."
  (view:make-viewport :center-x center-x :center-y center-y :scale scale :width width :height height))

(defun hidden-grid-drawing ()
  "Return a drawing whose grid is hidden, so only entities produce geometry."
  (let ((drawing (dom:make-drawing)))
    (dom:set-grid-visible drawing nil)
    drawing))

(defun build (drawing viewport &key state)
  "Build a scene and return the draw list and the scene result."
  (let ((list (scene:make-draw-list)))
    (let ((result (scene:build-scene list drawing viewport
                                     (or state (scene:make-scene-state))
                                     (scene:default-theme))))
      (values list result))))

(defun vertex-at (list index)
  "Return vertex INDEX of LIST as (x y r g b a)."
  (let ((array (scene:draw-list-vertices list))
        (base (* index scene:+floats-per-vertex+)))
    (list (aref array base) (aref array (+ base 1))
          (aref array (+ base 4)) (aref array (+ base 5)) (aref array (+ base 6)) (aref array (+ base 7)))))

(defun draw-lines (list)
  "Return every line of LIST as (x1 y1 x2 y2 r g b a)."
  (let ((lines '()))
    (loop for command across (scene:draw-list-commands list)
          when (eq :lines (scene:draw-command-kind command))
            do (loop for start from (scene:draw-command-start command)
                       below (+ (scene:draw-command-start command) (scene:draw-command-count command))
                     by 2
                     do (let ((a (vertex-at list start)) (b (vertex-at list (1+ start))))
                          (push (list (first a) (second a) (first b) (second b)
                                      (third a) (fourth a) (fifth a) (sixth a))
                                lines))))
    (nreverse lines)))

(defun draw-triangles (list)
  "Return every triangle of LIST as a list of three (x y) pairs."
  (let ((triangles '()))
    (loop for command across (scene:draw-list-commands list)
          when (eq :triangles (scene:draw-command-kind command))
            do (loop for start from (scene:draw-command-start command)
                       below (+ (scene:draw-command-start command) (scene:draw-command-count command))
                     by 3
                     do (push (loop for k below 3
                                    collect (subseq (vertex-at list (+ start k)) 0 2))
                              triangles)))
    (nreverse triangles)))

(defun text-runs-of (list)
  "Return the text runs of LIST."
  (coerce (scene:draw-list-text-runs list) 'list))

(defun line-near-p (line x1 y1 x2 y2)
  "True when LINE (x1 y1 x2 y2 ...) runs from the given start to the given end."
  (and (< (abs (- (first line) x1)) +pixel-tolerance+)
       (< (abs (- (second line) y1)) +pixel-tolerance+)
       (< (abs (- (third line) x2)) +pixel-tolerance+)
       (< (abs (- (fourth line) y2)) +pixel-tolerance+)))

(defun red-layer-drawing ()
  "Return a grid-less drawing with layer 1 colored pure red."
  (let ((drawing (hidden-grid-drawing)))
    (dom:add-layer drawing "red" (dom:make-rgb 255 0 0))
    drawing))

;;; Draw list

(test the-draw-list-counts-and-merges-primitives
  (let ((list (scene:make-draw-list))
        (color (scene:make-rgba 1f0 0f0 0f0 1f0)))
    (scene:add-line list 0f0 0f0 10f0 10f0 color)
    (scene:add-line list 1f0 1f0 5f0 5f0 color)
    (is (= 4 (scene:draw-list-vertex-count list)))
    (is (= 1 (length (scene:draw-list-commands list))))               ; merged into one run
    (scene:add-triangle list 0f0 0f0 1f0 0f0 0f0 1f0 color)
    (scene:add-line list 2f0 2f0 3f0 3f0 color)
    (is (= 3 (length (scene:draw-list-commands list))))               ; lines, triangles, lines
    (is (= 3 (scene:draw-list-line-count list)))
    (is (= 1 (scene:draw-list-triangle-count list)))
    (scene:draw-list-reset list)
    (is (= 0 (scene:draw-list-vertex-count list)))
    (is (= 0 (length (scene:draw-list-commands list))))))

(test the-draw-list-grows-without-losing-vertices
  (let ((list (scene:make-draw-list))
        (color (scene:make-rgba 0f0 1f0 0f0 1f0)))
    (dotimes (i 5000)
      (scene:add-line list (float i 1f0) 0f0 (float i 1f0) 1f0 color))
    (is (= 10000 (scene:draw-list-vertex-count list)))
    (is (equal '(4999f0 0f0) (subseq (vertex-at list 9998) 0 2)))
    (is (equal '(0f0 0f0) (subseq (vertex-at list 0) 0 2)))
    (is (= 5000 (scene:draw-list-line-count list)))))

(test rectangles-are-two-triangles-or-four-lines
  (let ((list (scene:make-draw-list))
        (color (scene:make-rgba 1f0 1f0 1f0 1f0)))
    (scene:add-filled-rectangle list 10f0 20f0 30f0 40f0 color)
    (is (= 2 (scene:draw-list-triangle-count list)))
    (scene:add-rectangle-outline list 10f0 20f0 30f0 40f0 color)
    (is (= 4 (scene:draw-list-line-count list)))))

(test text-runs-are-recorded-in-order-and-merged-into-one-command
  (let ((list (scene:make-draw-list))
        (color (scene:make-rgba 1f0 1f0 1f0 1f0)))
    (scene:add-text list "one" 1f0 2f0 12f0 color)
    (scene:add-text list "two" 3f0 4f0 14f0 color :angle 0.5f0)
    (is (= 1 (length (scene:draw-list-commands list))))
    (is (= 2 (scene:draw-command-count (aref (scene:draw-list-commands list) 0))))
    (let ((runs (text-runs-of list)))
      (is (equal '("one" "two") (mapcar #'scene:text-run-string runs)))
      (is (= 14f0 (scene:text-run-size (second runs))))
      (is (= 0.5f0 (scene:text-run-angle (second runs)))))))

(test layer-colors-convert-to-unit-floats
  (let ((color (scene:rgb->rgba (dom:make-rgb 255 0 51) 0.5f0)))
    (is (= 1f0 (scene:rgba-r color)))
    (is (= 0f0 (scene:rgba-g color)))
    (is (< (abs (- 0.2f0 (scene:rgba-b color))) 1f-6))
    (is (= 0.5f0 (scene:rgba-a color)))))

;;; Circle tessellation

(test circle-segment-count-follows-the-chordal-error-rule
  (is (= scene:+circle-minimum-segments+ (scene:circle-segment-count 0.1d0)))
  (is (= scene:+circle-minimum-segments+ (scene:circle-segment-count 2d0)))
  (is (= 45 (scene:circle-segment-count 100d0)))
  (is (= scene:+circle-maximum-segments+ (scene:circle-segment-count 1d9)))
  (let ((previous 0))
    (dolist (radius '(1d0 3d0 10d0 30d0 100d0 300d0 1000d0 10000d0))
      (let ((count (scene:circle-segment-count radius)))
        (is (>= count previous))
        (setf previous count)
        ;; The sagitta of a chord spanning 2 pi / count stays within the allowed error.
        (is (<= (* radius (- 1d0 (cos (/ +test-pi+ count)))) (+ scene:+circle-chordal-error-pixels+ 1d-9)))))))

;;; Scene content

(test a-line-is-transformed-to-screen-pixels-with-y-flipped
  (let ((drawing (red-layer-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (a-line :id 1 :layer-id 1 :x1 0d0 :y1 0d0 :x2 100d0 :y2 0d0)
                                      (a-line :id 2 :layer-id 1 :x1 0d0 :y1 0d0 :x2 0d0 :y2 100d0))))
    (multiple-value-bind (list result) (build drawing (scene-viewport))
      (let ((lines (draw-lines list)))
        (is (= 2 (length lines)))
        (is (line-near-p (first lines) 500d0 300d0 600d0 300d0))
        (is (line-near-p (second lines) 500d0 300d0 500d0 200d0))     ; world +y is screen up
        (is (equal '(1f0 0f0 0f0 1f0) (subseq (first lines) 4))))      ; the layer color
      (is (= 2 (scene:scene-result-drawn-entities result)))
      (is (= 0 (scene:scene-result-culled-entities result))))))

(test far-endpoints-are-clipped-to-the-canvas-margin-in-double-precision
  (let ((drawing (red-layer-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (a-line :id 1 :layer-id 1 :x1 -9d6 :y1 0d0 :x2 9d6 :y2 0d0))))
    (let ((lines (draw-lines (build drawing (scene-viewport)))))
      (is (= 1 (length lines)))
      (is (line-near-p (first lines) -4d0 300d0 1004d0 300d0)))
    ;; At extreme zoom the screen coordinates of the endpoints are ~1e10 px; clipping must
    ;; still place the visible part exactly.
    (let ((lines (draw-lines (build drawing (scene-viewport :scale 1d3)))))
      (is (= 1 (length lines)))
      (is (line-near-p (first lines) -4d0 300d0 1004d0 300d0)))))

(test off-screen-and-hidden-entities-are-culled-and-locked-ones-are-drawn
  (let ((drawing (red-layer-drawing)))
    (dom:add-layer drawing "hidden" (dom:make-rgb 0 255 0))
    (dom:add-layer drawing "locked" (dom:make-rgb 0 0 255))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list
                                 (a-line :id 1 :layer-id 1 :x1 0d0 :y1 0d0 :x2 100d0 :y2 0d0)
                                 ;; Off screen:
                                 (a-line :id 2 :layer-id 1 :x1 5000d0 :y1 5000d0 :x2 5100d0 :y2 5000d0)
                                 ;; On the hidden layer:
                                 (a-line :id 3 :layer-id 2 :x1 0d0 :y1 50d0 :x2 100d0 :y2 50d0)
                                 ;; On the locked layer:
                                 (a-line :id 4 :layer-id 3 :x1 0d0 :y1 -50d0 :x2 100d0 :y2 -50d0))))
    (dom:set-layer-visible drawing 2 nil)
    (dom:set-layer-locked drawing 3 t)
    (multiple-value-bind (list result) (build drawing (scene-viewport))
      (is (= 2 (length (draw-lines list))))
      (is (= 2 (scene:scene-result-drawn-entities result)))
      (is (= 2 (scene:scene-result-culled-entities result))))))

(test selected-entities-are-drawn-once-in-the-selection-color-and-follow-the-move-preview
  (let* ((drawing (red-layer-drawing))
         (selected (make-hash-table))
         (theme (scene:default-theme)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (a-line :id 1 :layer-id 1 :x1 0d0 :y1 0d0 :x2 100d0 :y2 0d0)
                                      (a-line :id 2 :layer-id 1 :x1 0d0 :y1 100d0 :x2 100d0 :y2 100d0))))
    (setf (gethash 2 selected) t)
    (let* ((lines (draw-lines (build drawing (scene-viewport)
                                     :state (scene:make-scene-state :selected-ids selected))))
           (selection (scene:theme-selection theme)))
      (is (= 2 (length lines)))
      ;; Unselected: the layer color, drawn first.
      (is (equal '(1f0 0f0 0f0 1f0) (subseq (first lines) 4)))
      ;; Selected: the selection color, drawn on top.
      (is (equal (list (scene:rgba-r selection)
                       (scene:rgba-g selection)
                       (scene:rgba-b selection)
                       (scene:rgba-a selection))
                 (subseq (second lines) 4))))
    ;; A move preview of (+50 mm, +20 mm) shifts only the selected line: right 50 px, up 20 px.
    (let ((lines (draw-lines (build drawing (scene-viewport)
                                    :state (scene:make-scene-state :selected-ids selected
                                                                   :translation (cons 50d0 20d0))))))
      (is (line-near-p (first lines) 500d0 300d0 600d0 300d0))
      (is (line-near-p (second lines) 550d0 180d0 650d0 180d0)))))

(test shapes-emit-the-expected-segments
  (let ((drawing (red-layer-drawing)))
    (dom:apply-changes
     drawing
     (dom:create-entities-changes
      (list (a-rectangle :id 1 :layer-id 1 :min-x 0d0 :min-y 0d0 :width 100d0 :height 50d0)
            (a-polyline :id 2 :layer-id 1 :coordinates (doubles 0d0 100d0 50d0 100d0 50d0 150d0))
            (a-polyline :id 3 :layer-id 1 :coordinates (doubles 0d0 200d0 50d0 200d0 50d0 250d0) :closed t))))
    (let ((lines (draw-lines (build drawing (scene-viewport)))))
      (is (= (+ 4 2 3) (length lines)))
      ;; The rectangle: bottom, right, top, left edges.
      (is (line-near-p (first lines) 500d0 300d0 600d0 300d0))
      (is (line-near-p (second lines) 600d0 300d0 600d0 250d0))
      (is (line-near-p (third lines) 600d0 250d0 500d0 250d0))
      (is (line-near-p (fourth lines) 500d0 250d0 500d0 300d0))
      ;; The closed polyline ends with the closing segment back to its first vertex.
      (is (line-near-p (car (last lines)) 550d0 50d0 500d0 100d0)))))

(test circles-are-closed-polygons-around-the-center
  (let ((drawing (red-layer-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (a-circle :id 1 :layer-id 1 :center-x 0d0 :center-y 0d0 :radius 100d0))))
    (let ((lines (draw-lines (build drawing (scene-viewport)))))
      (is (= 45 (length lines)))
      (is (line-near-p (first lines) 600d0 300d0 (third (first lines)) (fourth (first lines))))   ; starts at angle 0
      (flet ((gap (x1 y1 x2 y2)
               (geo:distance (float x1 1d0) (float y1 1d0) (float x2 1d0) (float y2 1d0))))
        (dolist (line lines)
          (is (< (abs (- 100d0 (gap 500 300 (first line) (second line)))) 0.01d0))
          (is (< (abs (- 100d0 (gap 500 300 (third line) (fourth line)))) 0.01d0)))
        ;; Consecutive segments share endpoints and the last returns to the first.
        (loop for (a b) on lines while b
              do (is (< (gap (third a) (fourth a) (first b) (second b)) 0.01d0)))
        (is (< (gap (third (car (last lines))) (fourth (car (last lines)))
                    (first (first lines)) (second (first lines)))
               0.01d0))))))

(test entities-smaller-than-a-pixel-become-a-one-pixel-dash
  (let ((drawing (red-layer-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (a-rectangle :id 1
                                                   :layer-id 1
                                                   :min-x 10d0
                                                   :min-y 10d0
                                                   :width 0.2d0
                                                   :height 0.2d0))))
    (let ((lines (draw-lines (build drawing (scene-viewport)))))
      (is (= 1 (length lines)))
      (is (< (abs (- 1d0 (- (third (first lines)) (first (first lines))))) +pixel-tolerance+)))))

(test text-becomes-a-text-run-or-a-baseline-stroke-when-too-small
  (let ((drawing (red-layer-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (a-text :id 1 :layer-id 1 :x 100d0 :y 50d0 :string "Hall" :height 100d0))))
    (let ((list (build drawing (scene-viewport :scale 0.5d0))))
      (is (= 0 (length (draw-lines list))))
      (let ((run (first (text-runs-of list))))
        (is (string= "Hall" (scene:text-run-string run)))
        (is (= 50f0 (scene:text-run-size run)))                         ; 100 mm x 0.5 px/mm
        (is (= 550f0 (scene:text-run-x run)))                           ; 500 + 100 * 0.5
        (is (= 275f0 (scene:text-run-y run)))                           ; 300 - 50 * 0.5
        (is (= 0f0 (scene:text-run-angle run)))))
    ;; At 0.01 px/mm the text is 1 px tall: drawn as a baseline stroke of its nominal width.
    (let ((list (build drawing (scene-viewport :scale 0.01d0))))
      (is (null (text-runs-of list)))
      (is (= 1 (length (draw-lines list)))))))

(test dimensions-emit-lines-arrowheads-and-rotated-text
  (let ((drawing (red-layer-drawing)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (a-dimension :id 1 :layer-id 1 :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0
                                                   :offset 500d0 :text-height 100d0))))
    (let ((list (build drawing (scene-viewport :scale 0.5d0))))
      (is (= 3 (length (draw-lines list))))                             ; two extension lines and the dimension line
      (is (= 2 (scene:draw-list-triangle-count list)))
      (let ((run (first (text-runs-of list))))
        (is (string= "1000" (scene:text-run-string run)))
        (is (= 50f0 (scene:text-run-size run)))
        (is (< (abs (- 690f0 (scene:text-run-x run))) 1f-3))            ; 500 + 380 * 0.5
        (is (< (abs (- 32.5f0 (scene:text-run-y run))) 1f-3))))         ; 300 - 535 * 0.5
    ;; A vertical dimension reads bottom to top: its text run is rotated by a quarter turn.
    (let ((vertical (red-layer-drawing)))
      (dom:apply-changes vertical (dom:create-entities-changes
                                   (list (a-dimension :id 1 :layer-id 1 :x1 0d0 :y1 0d0 :x2 0d0 :y2 1000d0
                                                      :offset 300d0 :text-height 100d0))))
      (let ((run (first (text-runs-of (build vertical (scene-viewport :scale 0.5d0))))))
        (is (< (abs (- (/ +test-pi+ 2) (scene:text-run-angle run))) 1f-5))))
    ;; Zoomed far out the arrowheads and the text are skipped, but the lines remain.
    (let ((list (build drawing (scene-viewport :scale 0.01d0))))
      (is (= 0 (scene:draw-list-triangle-count list)))
      (is (null (text-runs-of list))))))

(test the-grid-matches-the-spacing-and-marks-axes-and-major-lines
  (let ((drawing (dom:make-drawing))
        (theme (scene:default-theme)))
    ;; Scale 1 px/mm, spacing 100 mm: lines every 100 px; the 1000x600 canvas centered on the
    ;; origin shows x lines at -500..500 (11) and y lines at -300..300 (7).
    (let* ((list (build drawing (scene-viewport)))
           (lines (draw-lines list))
           (vertical (remove-if-not (lambda (line) (and (< (abs (- (first line) (third line))) 1d-6)
                                                        (> (abs (- (second line) (fourth line))) 100)))
                                    lines))
           (horizontal (remove-if-not (lambda (line) (< (abs (- (second line) (fourth line))) 1d-6)) lines)))
      (is (= 11 (length vertical)))
      (is (= 7 (length horizontal)))
      (is (equal '(0f0 100f0 200f0 300f0 400f0 500f0 600f0 700f0 800f0 900f0 1000f0)
                 (mapcar #'first vertical)))
      (flet ((color-of (line) (subseq line 4))
             (color-list (rgba) (list (scene:rgba-r rgba)
                                      (scene:rgba-g rgba)
                                      (scene:rgba-b rgba)
                                      (scene:rgba-a rgba))))
        ;; The vertical axis line is at x = 0: the y axis.
        (is (equal (color-list (scene:theme-axis-y theme))
                   (color-of (find 500f0 vertical :key #'first))))
        ;; A major line is at x = -500.
        (is (equal (color-list (scene:theme-grid-major theme))
                   (color-of (find 0f0 vertical :key #'first))))
        (is (equal (color-list (scene:theme-grid-minor theme))
                   (color-of (find 100f0 vertical :key #'first))))
        (is (equal (color-list (scene:theme-axis-x theme)) (color-of (find 300f0 horizontal :key #'second))))))
    (dom:set-grid-visible drawing nil)
    (is (null (draw-lines (build drawing (scene-viewport)))))))

(test the-grid-density-on-screen-stays-bounded-at-every-zoom
  ;; The displayed spacing adapts so lines are never closer than the minimum, which bounds the
  ;; number of lines by the canvas size over that minimum, at any zoom level.
  (let* ((drawing (dom:make-drawing))
         (bound (+ (/ 1000d0 view:+minimum-grid-spacing-pixels+)
                   (/ 600d0 view:+minimum-grid-spacing-pixels+)
                   4)))
    (dolist (scale '(100d0 1d0 0.01d0 1d-4))
      (let ((count (length (draw-lines (build drawing (scene-viewport :scale scale))))))
        (is (plusp count))
        (is (<= count bound))))))

(test snap-markers-and-the-selection-window-have-the-expected-shapes
  (let ((drawing (hidden-grid-drawing)))
    (flet ((lines-for (snap) (length (draw-lines (build drawing (scene-viewport)
                                                        :state (scene:make-scene-state :snap snap))))))
      (is (= 4 (lines-for '(0d0 0d0 :endpoint))))
      (is (= 16 (lines-for '(0d0 0d0 :center))))
      (is (= 2 (lines-for '(0d0 0d0 :grid))))
      (is (= 0 (lines-for nil))))
    (let ((list (build drawing (scene-viewport)
                       :state (scene:make-scene-state :window (geo:make-bounds -100d0 -50d0 100d0 50d0)))))
      (is (= 2 (scene:draw-list-triangle-count list)))
      (is (= 4 (length (draw-lines list))))
      (let ((first-triangle (first (draw-triangles list))))
        (is (equal '((400f0 250f0) (600f0 250f0) (600f0 350f0)) first-triangle))))))

(test preview-entities-use-the-preview-color
  (let* ((drawing (red-layer-drawing))
         (theme (scene:default-theme))
         (lines (draw-lines (build drawing (scene-viewport)
                                   :state (scene:make-scene-state
                                           :preview-entities (list (a-line :id 1 :layer-id 1)))))))
    (is (= 1 (length lines)))
    (is (= (scene:rgba-g (scene:theme-preview theme)) (sixth (first lines))))))

;;; Properties

(test every-emitted-line-stays-inside-the-clip-margin
  (let ((prng (rng:make-prng 41))
        (drawing (rng:generate-drawing :seed 6 :entity-count 300)))
    (dotimes (i 40)
      (let* ((viewport (scene-viewport :center-x (rng:prng-next-double prng 0d0 60000d0)
                                       :center-y (rng:prng-next-double prng 0d0 60000d0)
                                       :scale (exp (rng:prng-next-double prng (log 1d-3) (log 200d0)))
                                       :width (rng:prng-next-double prng 300d0 1500d0)
                                       :height (rng:prng-next-double prng 300d0 900d0)))
             (list (build drawing viewport))
             (margin (+ scene:+culling-margin-pixels+ +pixel-tolerance+)))
        (dolist (line (draw-lines list))
          (is (<= (- margin) (first line) (+ (view:viewport-width viewport) margin)))
          (is (<= (- margin) (third line) (+ (view:viewport-width viewport) margin)))
          (is (<= (- margin) (second line) (+ (view:viewport-height viewport) margin)))
          (is (<= (- margin) (fourth line) (+ (view:viewport-height viewport) margin))))))))

(test building-twice-into-a-reset-list-is-deterministic
  (let* ((drawing (rng:generate-drawing :seed 9 :entity-count 200))
         (viewport (scene-viewport :center-x 20000d0 :center-y 20000d0 :scale 0.05d0))
         (list (scene:make-draw-list)))
    (flet ((snapshot ()
             (scene:draw-list-reset list)
             (scene:build-scene list drawing viewport (scene:make-scene-state) (scene:default-theme))
             (list (scene:draw-list-vertex-count list)
                   (subseq (scene:draw-list-vertices list) 0
                           (* scene:+floats-per-vertex+ (scene:draw-list-vertex-count list)))
                   (length (scene:draw-list-text-runs list)))))
      (is (equalp (snapshot) (snapshot))))))

(test the-scene-honors-the-canvas-origin
  (let ((drawing (red-layer-drawing))
        (viewport (view:make-viewport :center-x 0d0 :center-y 0d0 :scale 1d0
                                      :width 1000d0 :height 560d0 :origin-x 0d0 :origin-y 40d0)))
    (dom:apply-changes drawing (dom:create-entities-changes
                                (list (a-line :id 1 :layer-id 1 :x1 -5000d0 :y1 0d0 :x2 5000d0 :y2 0d0))))
    (let ((lines (draw-lines (build drawing viewport))))
      (is (= 1 (length lines)))
      (is (line-near-p (first lines) -4d0 320d0 1004d0 320d0)))        ; y = 40 + 560 / 2; clipped to x margin
    ;; The grid spans exactly the canvas rectangle, not the window.
    (let ((grid (draw-lines (build (dom:make-drawing) viewport))))
      (is (every (lambda (line) (and (>= (min (second line) (fourth line)) 40f0)
                                     (<= (max (second line) (fourth line)) 600f0)))
                 grid)))))
