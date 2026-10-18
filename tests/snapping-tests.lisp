(in-package #:borba-cad/tests)

(def-suite snapping
  :description "The object snaps beyond endpoint and center: midpoint, intersection, quadrant, perpendicular, tangent."
  :in all)

(in-suite snapping)

;;; Helpers

(defun snap-drawing (&rest entities)
  "Return a drawing, with the object snap on and the grid snap off, holding ENTITIES."
  (let ((drawing (dom:make-drawing)))
    (dom:set-object-snap drawing t)
    (dom:set-snap-to-grid drawing nil)
    (dom:apply-changes drawing (dom:create-entities-changes entities))
    drawing))

(defun snapped (drawing
                x
                y
                &key (radius 30d0)
                     anchor)
  "Return what the snap gives for a pointer at (X, Y) as a list of x, y and kind."
  (multiple-value-bind (snap-x snap-y kind) (dom:snap-point drawing
                                                            (float x 1d0)
                                                            (float y 1d0)
                                                            (float radius 1d0)
                                                            anchor)
    (list snap-x snap-y kind)))

(defun snap-is (expected-x
                expected-y
                expected-kind
                result)
  "Return true when RESULT, a list of x, y and kind, is the point (EXPECTED-X, EXPECTED-Y) of EXPECTED-KIND."
  (and (eq expected-kind (third result))
       (< (abs (- expected-x (first result))) 1d-6)
       (< (abs (- expected-y (second result))) 1d-6)))

(defun snap-line (id x1 y1 x2 y2)
  "Return a line on the default layer."
  (dom:make-line :id id :layer-id 0 :x1 (float x1 1d0) :y1 (float y1 1d0) :x2 (float x2 1d0) :y2 (float y2 1d0)))

(defun snap-circle (id x y radius)
  "Return a circle on the default layer."
  (dom:make-circle :id id :layer-id 0 :center-x (float x 1d0) :center-y (float y 1d0) :radius (float radius 1d0)))

(defun snap-arc (id x y radius start end)
  "Return an arc on the default layer, with angles in degrees."
  (dom:make-arc :id id :layer-id 0 :center-x (float x 1d0) :center-y (float y 1d0) :radius (float radius 1d0)
                :start-angle (base:degrees->radians (float start 1d0))
                :end-angle (base:degrees->radians (float end 1d0))))

;;; The geometry

(test two-segments-cross-where-they-meet-and-nowhere-else
  (multiple-value-bind (x y) (geo:segment-segment-intersection 0d0 0d0 10d0 10d0 0d0 10d0 10d0 0d0)
    (is (and (< (abs (- 5d0 x)) 1d-9) (< (abs (- 5d0 y)) 1d-9))))
  (is (null (geo:segment-segment-intersection 0d0 0d0 10d0 0d0 0d0 1d0 10d0 1d0)) "parallel")
  (is (null (geo:segment-segment-intersection 0d0 0d0 4d0 4d0 0d0 10d0 10d0 0d0)) "the first stops before the second")
  (is (null (geo:segment-segment-intersection 0d0 0d0 10d0 0d0 20d0 -5d0 20d0 5d0)) "their lines cross beyond them")
  (multiple-value-bind (x y) (geo:segment-segment-intersection 0d0 0d0 10d0 0d0 10d0 0d0 10d0 5d0)
    (is (and x (< (abs (- 10d0 x)) 1d-9) (< (abs y) 1d-9)) "a segment that ends on another")))

(test a-segment-crosses-a-circle-at-two-points-one-or-none
  (is (equal 2 (length (geo:segment-circle-intersections -10d0 0d0 10d0 0d0 0d0 0d0 5d0))))
  (let ((points (geo:segment-circle-intersections -10d0 0d0 10d0 0d0 0d0 0d0 5d0)))
    (is (equal '(-5 5) (sort (mapcar (lambda (point) (round (car point))) points) #'<)) "at -5 and 5")
    (is (every (lambda (point) (< (abs (cdr point)) 1d-9)) points)))
  (is (= 1 (length (geo:segment-circle-intersections 0d0 0d0 10d0 0d0 0d0 0d0 5d0))) "from the center out: one")
  (is (= 1 (length (geo:segment-circle-intersections -10d0 5d0 10d0 5d0 0d0 0d0 5d0))) "touching")
  (is (null (geo:segment-circle-intersections -10d0 6d0 10d0 6d0 0d0 0d0 5d0)))
  (is (null (geo:segment-circle-intersections 6d0 0d0 20d0 0d0 0d0 0d0 5d0)) "a segment that stops short"))

(test two-circles-cross-at-two-points-touch-at-one-or-miss
  (let ((points (geo:circle-circle-intersections 0d0 0d0 5d0 6d0 0d0 5d0)))
    (is (= 2 (length points)))
    (is (every (lambda (point) (< (abs (- 3d0 (car point))) 1d-9)) points))
    (is (equal '(-4 4) (sort (mapcar (lambda (point) (round (cdr point))) points) #'<))))
  (is (= 1 (length (geo:circle-circle-intersections 0d0 0d0 5d0 10d0 0d0 5d0))) "touching from outside")
  (is (null (geo:circle-circle-intersections 0d0 0d0 5d0 20d0 0d0 5d0)) "apart")
  (is (null (geo:circle-circle-intersections 0d0 0d0 5d0 1d0 0d0 2d0)) "one inside the other")
  (is (null (geo:circle-circle-intersections 0d0 0d0 5d0 0d0 0d0 5d0)) "the same circle has no points to give"))

(test the-foot-of-a-perpendicular-is-on-the-segment-or-there-is-none
  (multiple-value-bind (x y) (geo:point-segment-foot 3d0 7d0 0d0 0d0 10d0 0d0)
    (is (and (< (abs (- 3d0 x)) 1d-9) (< (abs y) 1d-9))))
  (is (null (geo:point-segment-foot 13d0 7d0 0d0 0d0 10d0 0d0)) "beyond the end")
  (multiple-value-bind (x y) (geo:point-segment-foot 0d0 10d0 0d0 0d0 10d0 10d0)
    (is (and (< (abs (- 5d0 x)) 1d-9) (< (abs (- 5d0 y)) 1d-9)) "on a diagonal")))

(test the-perpendicular-and-tangent-points-of-a-circle-are-where-they-should-be
  (let ((feet (geo:circle-perpendicular-feet 20d0 0d0 0d0 0d0 5d0)))
    (is (equal '(-5 5) (sort (mapcar (lambda (point) (round (car point))) feet) #'<)))
    (is (null (geo:circle-perpendicular-feet 0d0 0d0 0d0 0d0 5d0)) "from the center there is no line"))
  (let ((points (geo:circle-tangent-points 20d0 0d0 0d0 0d0 5d0)))
    (is (= 2 (length points)))
    (dolist (point points)
      (is (< (abs (- 5d0 (geo:distance 0d0 0d0 (car point) (cdr point)))) 1d-9) "on the circle")
      (is (< (abs (+ (* (- (car point) 20d0) (car point)) (* (cdr point) (cdr point)))) 1d-9)
          "the line from the point is at a right angle to the radius")))
  (is (null (geo:circle-tangent-points 5d0 0d0 0d0 0d0 5d0)) "from the circle itself")
  (is (null (geo:circle-tangent-points 1d0 1d0 0d0 0d0 5d0)) "from inside"))

;;; Midpoints and quadrants

(test the-middle-of-a-line-a-rectangle-a-polyline-and-a-polygon-side-is-a-snap
  (let ((drawing (snap-drawing (snap-line 1 0 0 1000 0)
                               (dom:make-rectangle :id 2 :layer-id 0 :min-x 0d0 :min-y 500d0 :width 400d0 :height 200d0)
                               (dom:make-polyline :id 3 :layer-id 0 :closed nil
                                                  :coordinates (doubles 2000d0 0d0 2000d0 600d0 2400d0 600d0)))))
    (is (snap-is 500 0 :midpoint (snapped drawing 505 6)) "the line")
    (is (snap-is 200 500 :midpoint (snapped drawing 195 494)) "the bottom of the rectangle")
    (is (snap-is 400 600 :midpoint (snapped drawing 410 604)) "the right side of the rectangle")
    (is (snap-is 2000 300 :midpoint (snapped drawing 2005 297)) "the first segment of the polyline")
    (is (snap-is 2200 600 :midpoint (snapped drawing 2190 604)) "the second")))

(test a-circle-has-four-quadrants-and-an-arc-those-it-covers-and-its-middle
  (let ((drawing (snap-drawing (snap-circle 1 0 0 500)
                               (snap-arc 2 3000 0 500 0 90))))
    (is (snap-is 500 0 :quadrant (snapped drawing 505 4)))
    (is (snap-is 0 500 :quadrant (snapped drawing -4 495)))
    (is (snap-is -500 0 :quadrant (snapped drawing -495 -3)))
    (is (snap-is 0 -500 :quadrant (snapped drawing 3 -490)))
    (is (snap-is 3500 0 :endpoint (snapped drawing 3498 3)) "the end of the arc is an endpoint, not a quadrant")
    (is (snap-is 3000 500 :endpoint (snapped drawing 3002 498)))
    (let ((middle (* 500 (cos (/ pi 4)))))
      (is (snap-is (+ 3000 middle) middle :midpoint (snapped drawing (+ 3000 middle 5) (- middle 4)))
          "the middle of the arc"))
    (is (null (third (snapped drawing 2500 0))) "the quadrant at 180 degrees is not on the arc")))

;;; Intersections

(test two-lines-that-cross-give-the-crossing
  (let ((drawing (snap-drawing (snap-line 1 0 0 1000 1000)
                               (snap-line 2 0 1000 1000 0))))
    (is (snap-is 500 500 :intersection (snapped drawing 510 495)))))

(test a-line-a-circle-and-an-arc-cross-where-they-meet
  (let ((drawing (snap-drawing (snap-line 1 -1000 0 1000 0)
                               (snap-circle 2 0 0 500)
                               (snap-arc 3 4000 0 500 0 90)
                               (snap-line 4 3000 -100 5000 100))))
    (is (snap-is 500 0 :intersection (snapped drawing 495 5)) "line and circle: the other crossing is at -500 too")
    (is (snap-is -500 0 :intersection (snapped drawing -503 -4)))))

(test two-circles-cross-in-two-places
  (let ((drawing (snap-drawing (snap-circle 1 0 0 500) (snap-circle 2 600 0 500))))
    (is (snap-is 300 (sqrt (- 250000d0 90000d0)) :intersection (snapped drawing 305 395 :radius 40)))
    (is (snap-is 300 (- (sqrt (- 250000d0 90000d0))) :intersection (snapped drawing 295 -395 :radius 40)))))

(test a-crossing-beyond-the-end-of-an-arc-is-not-given
  (let ((drawing (snap-drawing (snap-arc 1 0 0 500 0 90)
                               (snap-line 2 -1000 0 1000 0))))
    (is (not (eq :intersection (third (snapped drawing -500 0)))) "the circle crosses there, the arc does not")
    (is (not (eq :intersection (third (snapped drawing 500 0)))) "and at 500 it is the end of the arc")
    (is (eq :endpoint (third (snapped drawing 500 0))))))

(test a-rectangle-and-a-polyline-cross-each-other
  (let ((drawing (snap-drawing (dom:make-rectangle :id 1 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 1000d0 :height 1000d0)
                               (dom:make-polyline :id 2 :layer-id 0 :closed nil
                                                  :coordinates (doubles -500d0 500d0 1500d0 700d0)))))
    (is (snap-is 0 550 :intersection (snapped drawing 6 554)))
    (is (snap-is 1000 650 :intersection (snapped drawing 995 646)))))

(test an-endpoint-wins-over-an-intersection-at-the-same-point
  (let ((drawing (snap-drawing (snap-line 1 0 0 1000 0) (snap-line 2 1000 0 1000 800))))
    (is (snap-is 1000 0 :endpoint (snapped drawing 1004 3)))))

(test the-snap-prefers-the-nearer-point-even-of-a-lesser-kind
  (let ((drawing (snap-drawing (snap-line 1 0 0 1000 0))))
    (is (snap-is 500 0 :midpoint (snapped drawing 500 3)) "3 from the midpoint, 500 from the ends")
    (is (snap-is 0 0 :endpoint (snapped drawing 4 2)))))

(test hidden-and-locked-layers-give-no-snaps
  (let ((drawing (snap-drawing (snap-line 1 -500 0 1000 0))))
    (let ((layer (dom:add-layer drawing "other" (dom:make-rgb 1 2 3))))
      (dom:apply-changes drawing
                         (dom:create-entities-changes
                          (list (dom:make-line :id 2 :layer-id (dom:layer-id layer)
                                               :x1 0d0 :y1 -500d0 :x2 0d0 :y2 500d0))))
      (is (snap-is 0 0 :intersection (snapped drawing 3 2)) "both visible: they cross there")
      (dom:set-layer-locked drawing (dom:layer-id layer) t)
      (is (not (eq :intersection (third (snapped drawing 3 2)))) "one locked: it gives nothing")
      (dom:set-layer-locked drawing (dom:layer-id layer) nil)
      (dom:set-layer-visible drawing (dom:layer-id layer) nil)
      (is (not (eq :intersection (third (snapped drawing 3 2)))) "or hidden"))))

(test the-object-snap-switch-turns-all-of-them-off
  (let ((drawing (snap-drawing (snap-line 1 0 0 1000 0))))
    (is (eq :midpoint (third (snapped drawing 500 3))))
    (dom:set-object-snap drawing nil)
    (is (null (third (snapped drawing 500 3))))))

(test a-crowded-place-is-answered-and-a-pointer-far-from-everything-gets-nothing
  (let ((drawing (snap-drawing)))
    (let ((entities (loop for index from 1 to 120
                          collect (snap-line index (- (* index 10)) -500 (* index 10) 500))))
      (dom:apply-changes drawing (dom:create-entities-changes entities)))
    (is (member (third (snapped drawing 0 0 :radius 50d0)) '(:intersection :endpoint :midpoint)))
    (is (null (third (snapped drawing 90000 90000))))))

;;; From the last point

(test the-foot-of-a-perpendicular-is-a-snap-only-when-there-is-an-anchor
  (let ((drawing (snap-drawing (snap-line 1 0 0 1000 0))))
    (is (snap-is 300 0 :perpendicular (snapped drawing 305 6 :anchor (cons 300d0 500d0))))
    (is (null (third (snapped drawing 305 6))) "no anchor, no perpendicular")
    (is (null (third (snapped drawing 305 6 :anchor (cons 3000d0 500d0)))) "the foot is beyond the end")))

(test a-circle-gives-its-perpendicular-points-and-an-arc-only-the-ones-it-covers
  (let ((drawing (snap-drawing (snap-circle 1 0 0 500)
                               (snap-arc 2 3000 0 500 0 90))))
    (let ((anchor (cons 2000d0 0d0))
          (slanted (cons 2000d0 2000d0))
          (diagonal (* 500 (cos (/ pi 4)))))
      (is (snap-is 500 0 :quadrant (snapped drawing 504 3 :anchor anchor))
          "the quadrant ranks before the perpendicular")
      (is (snap-is diagonal diagonal :perpendicular (snapped drawing (+ diagonal 4) (- diagonal 3) :anchor slanted))
          "the near side of the circle, on the line to the center")
      (is (snap-is (- diagonal) (- diagonal) :perpendicular (snapped drawing (- 4 diagonal) (- (- diagonal) 3)
                                                                     :anchor slanted))
          "and the far side")
      (let ((arc-anchor (cons (+ 3000 (* 2000 (cos (/ pi 4)))) (* 2000 (sin (/ pi 4))))))
        (is (snap-is (+ 3000 diagonal) diagonal :midpoint (snapped drawing (+ 3000 diagonal 4) (+ diagonal 3)
                                                                   :anchor arc-anchor))
            "at the middle of the arc the middle wins")
        (is (null (third (snapped drawing (- 3000 diagonal) (- diagonal) :anchor arc-anchor)))
            "the far side is not on the arc")))))

(test a-circle-gives-the-points-of-tangency-from-a-point-outside
  (let* ((drawing (snap-drawing (snap-circle 1 0 0 500)))
         (anchor (cons 2000d0 0d0))
         (points (geo:circle-tangent-points 2000d0 0d0 0d0 0d0 500d0)))
    (dolist (point points)
      (let ((result (snapped drawing (+ (car point) 4) (- (cdr point) 3) :anchor anchor)))
        (is (snap-is (car point) (cdr point) :tangent result))))
    (is (null (third (snapped drawing 0 600 :anchor (cons 100d0 100d0)))) "from inside there is none")))

;;; In the application

(test the-line-tool-snaps-to-a-perpendicular-from-its-first-point
  (let ((application (make-test-app)))
    (dom:set-snap-to-grid (app:app-drawing application) nil)
    (dom:set-object-snap (app:app-drawing application) t)
    (app:commit-changes application
                        (dom:create-entities-changes (list (snap-line 1 -2000 0 2000 0)))
                        "entity.create")
    (press application :l)
    (click application 700 1500)
    (send application (pointer application :move nil 705 8))
    (let ((cursor (app:app-cursor application)))
      (is (eq :perpendicular (app:cursor-snap-kind cursor)))
      (is (< (abs (- 700d0 (app:cursor-x cursor))) 1d-6))
      (is (< (abs (app:cursor-y cursor)) 1d-6)))
    (is (search "perpendicular" (nth-value 2 (app:app-status-texts application nil))))
    (click application 705 8)
    (let ((line (last-entity application)))
      (is (< (abs (- 700d0 (dom:line-x2 line))) 1d-6))
      (is (< (abs (dom:line-y2 line)) 1d-6) "the new line ends square on the old one"))))

(test the-chip-says-what-the-object-snap-gives
  (let ((application (make-test-app)))
    (is (search "points of the entities" (ui:widget-hint (find-widget application :object-snap))))))

(test every-kind-of-snap-has-a-marker-and-a-name-in-every-language
  (dolist (kind '(:endpoint :midpoint :center :intersection :quadrant :perpendicular :tangent :grid :ortho :polar))
    (let ((list (scene:make-draw-list)))
      (scene:add-snap-marker list
                             (view:make-viewport :center-x 0d0 :center-y 0d0 :scale 1d0 :width 100d0 :height 100d0)
                             (list 0d0 0d0 kind)
                             (scene:default-theme))
      (is (plusp (scene:draw-list-vertex-count list)) "~a has a marker" kind))
    (is (plusp (length (app::snap-kind-label kind))))
    (dolist (language '(:pt-br :ru-ru))
      (i18n:with-language (language)
        (is (plusp (length (app::snap-kind-label kind))))
        (when (and (eq language :ru-ru) (not (eq kind :grid)))
          (is (string/= (string-downcase (symbol-name kind)) (app::snap-kind-label kind))
              "~a in ~a" kind language))))))
