(in-package #:borba-cad/tests)

(def-suite grips
  :description "Grips: the handles on the selected entity that are dragged to change it."
  :in all)

(in-suite grips)

(defun grip-named (entity id)
  "Return the grip of ENTITY with the id ID."
  (find id (dom:entity-grips entity) :key #'dom:grip-id :test #'equal))

(defun grip-ids (entity)
  "Return the ids of the grips of ENTITY."
  (mapcar #'dom:grip-id (dom:entity-grips entity)))

(defun dragged (entity id x y)
  "Return ENTITY with its grip ID dragged to (X, Y)."
  (dom:entity-with-grip entity (grip-named entity id) x y))

(defun values-close-p (first second)
  "Return true when two lists of property values agree to a nanometre or a nano-radian."
  (and (= (length first) (length second))
       (every (lambda (a b) (if (numberp a) (< (abs (- a b)) 1d-9) (equalp a b))) first second)))

(defun grip-sample-entities ()
  "Return an entity of every kind, to try grips on."
  (list (dom:make-line :id 1 :layer-id 0 :x1 100d0 :y1 200d0 :x2 1100d0 :y2 700d0)
        (dom:make-rectangle :id 2 :layer-id 0 :min-x 0d0 :min-y 0d0 :width 1000d0 :height 500d0)
        (dom:make-circle :id 3 :layer-id 0 :center-x 50d0 :center-y -80d0 :radius 400d0)
        (dom:make-polyline :id 4 :layer-id 0
                           :coordinates (coerce '(0d0 0d0 500d0 0d0 500d0 300d0 100d0 600d0)
                                                '(simple-array double-float (*)))
                           :closed t)
        (dom:make-text :id 5 :layer-id 0 :x 10d0 :y 20d0 :string "Kitchen" :height 250d0 :angle 0.4d0)
        (dom:make-dimension :id 6 :layer-id 0 :x1 0d0 :y1 0d0 :x2 3000d0 :y2 1000d0 :offset 400d0
                            :text-height 200d0 :unit :millimeter :precision 0)
        (dom:make-arc :id 7 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 900d0 :start-angle 0.3d0
                      :end-angle 2.4d0)
        (dom:make-ellipse :id 8 :layer-id 0 :center-x 100d0 :center-y 100d0 :radius-x 800d0 :radius-y 300d0
                          :rotation 0.5d0)
        (dom:make-polygon :id 9 :layer-id 0 :center-x -50d0 :center-y 70d0 :radius 600d0 :sides 7
                          :rotation 0.3d0)))

;;; Every kind of entity

(test every-kind-of-entity-has-grips-and-each-sits-on-the-entity
  (dolist (entity (grip-sample-entities))
    (let ((grips (dom:entity-grips entity)))
      (is (plusp (length grips)) "~s has grips" (dom:entity-kind entity))
      (is (= (length grips) (length (remove-duplicates (mapcar #'dom:grip-id grips) :test #'equal)))
          "with distinct ids")
      (dolist (grip grips)
        (unless (eq :move (dom:grip-kind grip))
          (is (< (dom:entity-pick-distance entity (dom:grip-x grip) (dom:grip-y grip))
                 (if (eq :dimension (dom:entity-kind entity))
                     (dom:dimension-text-height entity) ; a definition point is pointed at, not touched
                     1d-6))
              "the ~s grip of the ~s is on it" (dom:grip-id grip) (dom:entity-kind entity)))))))

(test dragging-a-grip-to-where-it-already-is-changes-nothing
  (dolist (entity (grip-sample-entities))
    (dolist (grip (dom:entity-grips entity))
      (let ((same (dom:entity-with-grip entity grip (dom:grip-x grip) (dom:grip-y grip))))
        (is (values-close-p (entity-signature-values entity) (entity-signature-values same))
            "the ~s grip of the ~s" (dom:grip-id grip) (dom:entity-kind entity))))))

(defun entity-signature-values (entity)
  "Return the property values of ENTITY."
  (mapcar #'dom:property-value (dom:entity-properties entity)))

(test the-move-grip-moves-the-whole-entity-like-translating-it
  (dolist (entity (grip-sample-entities))
    (let ((grip (find :move (dom:entity-grips entity) :key #'dom:grip-kind)))
      (when grip
        (let ((moved (dom:entity-with-grip entity grip (+ (dom:grip-x grip) 300d0) (- (dom:grip-y grip) 120d0)))
              (translated (dom:entity-translated entity 300d0 -120d0)))
          (is (values-close-p (entity-signature-values moved) (entity-signature-values translated))
              "the ~s" (dom:entity-kind entity)))))))

(test a-grip-never-changes-the-id-or-the-layer
  (dolist (entity (grip-sample-entities))
    (dolist (grip (dom:entity-grips entity))
      (let ((result (handler-case (dom:entity-with-grip entity grip (+ (dom:grip-x grip) 77d0)
                                                        (+ (dom:grip-y grip) 33d0))
                      (base:invalid-entity () nil))))
        (when result
          (is (= (dom:entity-id entity) (dom:entity-id result)))
          (is (= (dom:entity-layer-id entity) (dom:entity-layer-id result)))
          (is (eq (dom:entity-kind entity) (dom:entity-kind result))))))))

;;; Each kind

(test the-ends-of-a-line-are-dragged-and-its-middle-moves-it
  (let ((line (first (grip-sample-entities))))
    (is (equal '(:start :end :move) (grip-ids line)))
    (let ((moved (dragged line :start 0d0 0d0)))
      (is (= 0d0 (dom:line-x1 moved)))
      (is (= 1100d0 (dom:line-x2 moved)) "the other end stays"))
    (is (= 900d0 (dom:line-y2 (dragged line :end 5d0 900d0))))
    (is (= 200d0 (dom:line-x1 (dragged line :move 700d0 500d0))) "the middle was at (600, 450)")
    (signals base:invalid-entity (dragged line :end 100d0 200d0) "the end onto the start leaves no line")))

(test a-corner-of-a-rectangle-is-dragged-with-the-opposite-one-fixed
  (let ((rectangle (second (grip-sample-entities))))
    (is (equal '(:bottom-left :bottom-right :top-right :top-left :move) (grip-ids rectangle)))
    (let ((stretched (dragged rectangle :top-right 2000d0 1500d0)))
      (is (= 0d0 (dom:rectangle-min-x stretched)))
      (is (= 2000d0 (dom:rectangle-width stretched)))
      (is (= 1500d0 (dom:rectangle-height stretched))))
    (let ((flipped (dragged rectangle :bottom-left 1500d0 800d0)))
      (is (= 1000d0 (dom:rectangle-min-x flipped)) "past the opposite corner it is turned the right way round")
      (is (= 500d0 (dom:rectangle-width flipped)))
      (is (= 500d0 (dom:rectangle-min-y flipped)))
      (is (= 300d0 (dom:rectangle-height flipped))))
    (signals base:invalid-entity (dragged rectangle :top-left 1000d0 500d0) "a side of no length")))

(test a-circle-changes-its-radius-from-any-quadrant-and-moves-from-its-center
  (let ((circle (third (grip-sample-entities))))
    (is (equal '(:east :north :west :south :move) (grip-ids circle)))
    (is (= 500d0 (dom:circle-radius (dragged circle :east 550d0 -80d0))))
    (is (= 300d0 (dom:circle-radius (dragged circle :north 50d0 220d0))))
    (is (= 50d0 (dom:circle-center-x (dragged circle :south 80d0 -500d0))) "the center stays")))

(test a-polyline-has-a-grip-on-each-vertex
  (let ((polyline (fourth (grip-sample-entities))))
    (is (equal '(0 1 2 3) (grip-ids polyline)))
    (let ((moved (dragged polyline 2 900d0 400d0)))
      (is (= 900d0 (aref (dom:polyline-coordinates moved) 4)))
      (is (= 400d0 (aref (dom:polyline-coordinates moved) 5)))
      (is (= 0d0 (aref (dom:polyline-coordinates moved) 0)) "the others stay")
      (is (= 500d0 (aref (dom:polyline-coordinates polyline) 4)) "and the original is not changed"))
    (signals base:invalid-entity (dragged polyline 1 0d0 0d0) "onto the vertex next to it")))

(test a-polyline-with-very-many-vertices-has-grips-on-only-the-first-ones
  (let* ((count 3000)
         (coordinates (make-array (* 2 count) :element-type 'double-float))
         (polyline (progn (dotimes (index count)
                            (setf (aref coordinates (* 2 index)) (* 10d0 index)
                                  (aref coordinates (1+ (* 2 index))) (if (evenp index) 0d0 5d0)))
                          (dom:make-polyline :id 1 :layer-id 0 :coordinates coordinates :closed nil))))
    (is (= dom:+maximum-polyline-grips+ (length (dom:entity-grips polyline))))))

(test the-end-of-a-text-turns-it
  (let* ((text (fifth (grip-sample-entities)))
         (turned (dragged text :angle 10d0 1020d0)))
    (is (equal '(:move :angle) (grip-ids text)))
    (is (< (abs (- (/ pi 2) (dom:text-angle turned))) 1d-12) "straight up from the insertion point")
    (is (= 10d0 (dom:text-x turned)))
    (is (= 100d0 (dom:text-x (dragged text :move 100d0 20d0))))))

(test a-dimension-is-changed-at-its-points-and-its-line
  (let ((dimension (sixth (grip-sample-entities))))
    (is (equal '(:first :second :offset) (grip-ids dimension)))
    (is (= 3500d0 (dom:dimension-x2 (dragged dimension :second 3500d0 1000d0))))
    (let ((moved (dragged dimension :offset 1500d0 500d0)))
      (is (< (abs (- (* 1000d0 (/ 3000d0 (sqrt 10d6)))
                     (dom:dimension-offset (dragged dimension :offset 0d0 1000d0))))
             1d-6)
          "the offset is the signed distance from the line through the two points")
      (is (not (= 400d0 (dom:dimension-offset moved)))))))

(test an-arc-is-changed-at-its-ends-and-its-middle
  (let ((arc (seventh (grip-sample-entities))))
    (is (equal '(:start :end :radius :move) (grip-ids arc)))
    (is (< (abs (- (/ pi 2) (dom:arc-start-angle (dragged arc :start 0d0 500d0)))) 1d-12))
    (is (< (abs (- pi (dom:arc-end-angle (dragged arc :end -700d0 0d0)))) 1d-12))
    (is (= 1200d0 (dom:arc-radius (dragged arc :radius 1200d0 0d0))))
    (is (= 0.3d0 (dom:arc-start-angle (dragged arc :radius 1200d0 0d0))) "its ends stay where they point")
    (is (= 100d0 (dom:arc-center-x (dragged arc :move 100d0 0d0))))))

(test an-ellipse-is-changed-at-the-ends-of-its-axes
  (let ((ellipse (eighth (grip-sample-entities))))
    (is (equal '(:first-axis-end :first-axis-start :second-axis-end :second-axis-start :move) (grip-ids ellipse)))
    (let ((changed (dragged ellipse :first-axis-end 100d0 1100d0)))
      (is (= 1000d0 (dom:ellipse-radius-x changed)))
      (is (< (abs (- (/ pi 2) (dom:ellipse-rotation changed))) 1d-12) "the axis follows the grip"))
    (let ((other-end (dragged ellipse :first-axis-start 100d0 1100d0)))
      (is (= 1000d0 (dom:ellipse-radius-x other-end)))
      (is (< (abs (- (/ pi 2) (dom:ellipse-rotation other-end))) 1d-12)
          "dragged to the same point, the far end gives the same axis, which has no direction"))
    (signals base:invalid-entity (dragged ellipse :second-axis-end 100d0 100d0)
             "on the center the other axis is nothing")))

(test a-polygon-is-resized-and-turned-from-a-vertex
  (let ((polygon (ninth (grip-sample-entities))))
    (is (equal '(:move 0 1 2 3 4 5 6) (grip-ids polygon)))
    (let ((changed (dragged polygon 0 -50d0 570d0)))
      (is (= 500d0 (dom:polygon-radius changed)))
      (is (< (abs (- (/ pi 2) (dom:polygon-rotation changed))) 1d-12)))
    (let ((third-vertex (dragged polygon 2 -50d0 570d0)))
      (is (= 500d0 (dom:polygon-radius third-vertex)))
      (is (< (abs (- (/ pi 2) (geo:normalize-angle (+ (dom:polygon-rotation third-vertex)
                                                      (* 2 (/ (* 2 pi) 7))))))
             1d-9)
          "vertex 2 is two sides on from the first"))))

;;; The change a grip makes

(test a-grip-change-is-one-edit-of-an-entity-that-can-be-edited
  (let ((drawing (dom:make-drawing))
        (line (first (grip-sample-entities))))
    (dom:apply-changes drawing (dom:create-entity-changes line))
    (let ((changes (dom:grip-changes drawing 1 (grip-named line :end) 2000d0 0d0)))
      (is (= 1 (length changes)))
      (is (= 2000d0 (dom:line-x2 (dom:change-after (first changes))))))
    (is (null (dom:grip-changes drawing 1 (grip-named line :end) 1100d0 700d0)) "nothing changed, no change")
    (dom:set-layer-locked drawing dom:+default-layer-id+ t)
    (signals base:layer-error (dom:grip-changes drawing 1 (grip-named line :end) 2000d0 0d0))
    (signals base:layer-error (dom:grip-changes drawing 99 (grip-named line :end) 2000d0 0d0))))

;;; With the mouse

(defun grips-app (&rest entities)
  "Return an application holding ENTITIES, with the first of them selected."
  (let ((application (make-test-app)))
    (app:commit-changes application (dom:create-entities-changes entities) "entity.create")
    (app:select-ids application (list (dom:entity-id (first entities))))
    application))

(defun a-short-line (&key (id 1) (x1 0d0) (y1 0d0) (x2 1000d0) (y2 0d0))
  "Return a line."
  (dom:make-line :id id :layer-id 0 :x1 x1 :y1 y1 :x2 x2 :y2 y2))

(test the-selected-entity-shows-its-grips-and-two-selected-entities-show-none
  (let ((application (grips-app (a-short-line :id 1) (a-short-line :id 2 :y1 500d0 :y2 500d0))))
    (is (= 3 (length (app:selection-grips application))))
    (app:operation-select-all application)
    (is (null (app:selection-grips application)))
    (app:select-ids application '())
    (is (null (app:selection-grips application)))))

(test grips-are-shown-only-while-the-select-tool-is-the-active-one
  (let ((application (grips-app (a-short-line))))
    (is (app:selection-grips application))
    (press application :r)
    (is (null (app:selection-grips application)))
    (press application :escape)
    (is (app:selection-grips application))))

(test grips-are-drawn-and-the-one-under-the-pointer-is-drawn-filled
  (let ((application (grips-app (a-short-line))))
    (app:app-render application)
    (let ((with-grips (scene:draw-list-vertex-count (app:app-draw-list application))))
      (press application :r)
      (app:app-render application)
      (is (> with-grips (scene:draw-list-vertex-count (app:app-draw-list application))) "the handles are geometry"))
    (press application :escape)
    (send application (pointer application :move nil 1000 0))
    (is-true (app:grip-under-pointer application))
    (is (eq :end (dom:grip-id (app:grip-under-pointer application))))
    (send application (pointer application :move nil 500 400))
    (is (null (app:grip-under-pointer application)))))

(test dragging-a-grip-changes-the-entity-in-one-step-of-undo
  (let ((application (grips-app (a-short-line))))
    (drag application 1000 0 1500 300)
    (let ((line (first (entities-of application))))
      (is (= 1500d0 (dom:line-x2 line)))
      (is (= 300d0 (dom:line-y2 line)))
      (is (= 0d0 (dom:line-x1 line)) "the other end stays"))
    (press application :z :control)
    (is (= 1000d0 (dom:line-x2 (first (entities-of application)))))
    (is (= 0d0 (dom:line-y2 (first (entities-of application)))))
    (press application :y :control)
    (is (= 1500d0 (dom:line-x2 (first (entities-of application)))))
    (is (equal '(1) (app:selection-ids application)) "the line stays selected")))

(test a-grip-is-previewed-while-it-is-dragged-and-escape-abandons-the-drag
  (let ((application (grips-app (a-short-line))))
    (send application (pointer application :move nil 1000 0)
          (pointer application :down :left 1000 0)
          (pointer application :move nil 1200 100)
          (pointer application :move nil 1500 300))
    (let ((preview (app:tool-preview-entities (app:app-tool application) application)))
      (is (= 1 (length preview)))
      (is (= 1500d0 (dom:line-x2 (first preview)))))
    (is (null (app:selection-grips application)) "the handles wait while one is dragged")
    (press application :escape)
    (is (null (app:app-grip-drag application)))
    (send application (pointer application :up :left 1500 300))
    (is (= 1000d0 (dom:line-x2 (first (entities-of application)))) "nothing was changed")
    (is (app:selection-grips application))))

(test clicking-a-grip-without-moving-changes-nothing
  (let* ((application (grips-app (a-short-line)))
         (revision (dom:drawing-revision (app:app-drawing application))))
    (click application 1000 0)
    (is (= 1000d0 (dom:line-x2 (first (entities-of application)))))
    (is (= revision (dom:drawing-revision (app:app-drawing application))) "an untouched grip is not an edit")))

(test a-grip-drag-follows-the-grid-snap
  (let ((application (grips-app (a-short-line))))
    (drag application 1000 0 1530 270)
    (let ((line (first (entities-of application))))
      (is (= 1500d0 (dom:line-x2 line)) "to the nearest grid point, at 100 millimetres")
      (is (= 300d0 (dom:line-y2 line))))))

(test an-invalid-result-of-a-grip-drag-changes-nothing-and-says-why
  (let ((application (grips-app (a-short-line))))
    (drag application 1000 0 0 0)
    (is (= 1000d0 (dom:line-x2 (first (entities-of application)))))
    (is (eq :error (cdr (app:app-message application))))
    (is (search "Invalid line" (message-text application)))))

(test the-grip-of-a-circle-resizes-it-with-the-mouse
  (let ((application (grips-app (dom:make-circle :id 1 :layer-id 0 :center-x 0d0 :center-y 0d0 :radius 500d0))))
    (drag application 500 0 1200 0)
    (is (= 1200d0 (dom:circle-radius (first (entities-of application)))))))

(test pressing-away-from-the-grips-still-picks-and-moves-as-before
  (let ((application (grips-app (a-short-line))))
    (drag application 500 0 500 400)
    (let ((line (first (entities-of application))))
      (is (= 400d0 (dom:line-y1 line)) "the line moved up by 400")
      (is (= 400d0 (dom:line-y2 line))))
    (click application 1500 -1500)
    (is (null (app:selection-ids application)))))
