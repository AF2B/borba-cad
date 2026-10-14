(in-package #:borba-cad/tests)

(def-suite text-angle
  :description "Texts at an angle: the entity, the inspector, the tool and the drawing of them."
  :in all)

(in-suite text-angle)

(defun angled-text (&key (x 100d0) (y 200d0) (string "Kitchen") (height 250d0) (angle 0d0))
  "Return a text entity."
  (dom:make-text :id 1 :layer-id 0 :x x :y y :string string :height height :angle angle))

;;; The entity

(test a-text-has-an-angle-that-defaults-to-zero-and-is-kept-within-one-turn
  (is (= 0d0 (dom:text-angle (dom:make-text :id 1 :layer-id 0 :x 0d0 :y 0d0 :string "a" :height 100d0))))
  (is (= 0.5d0 (dom:text-angle (angled-text :angle 0.5d0))))
  (is (< (abs (- (* 1.5d0 pi) (dom:text-angle (angled-text :angle (- (/ pi 2)))))) 1d-12) "-90 degrees is 270")
  (is (< (abs (- 1d0 (dom:text-angle (angled-text :angle (+ 1d0 (* 4 pi)))))) 1d-9) "two turns more is the same")
  (is (= 0d0 (dom:text-angle (angled-text :angle (* 2 pi)))) "a whole turn is none"))

(test an-angle-that-is-not-a-finite-number-is-refused
  (dolist (angle (list 1d7 -1d7 1 "90" nil))
    (signals base:invalid-entity (angled-text :angle angle))))

(test the-bounds-and-the-picking-of-a-text-follow-its-angle
  (let ((flat (angled-text :angle 0d0))
        (upright (angled-text :angle (/ pi 2))))
    (is (> (geo:bounds-width (dom:entity-bounds flat)) (geo:bounds-height (dom:entity-bounds flat))))
    (is (< (geo:bounds-width (dom:entity-bounds upright)) (geo:bounds-height (dom:entity-bounds upright)))
        "a text turned a quarter stands up")
    (is (= 0d0 (dom:entity-pick-distance upright 95d0 400d0)) "a point on the upright text")
    (is (plusp (dom:entity-pick-distance upright 600d0 210d0)) "where the flat text would have been")
    (is (= 0d0 (dom:entity-pick-distance flat 600d0 210d0)))
    (is (plusp (dom:entity-pick-distance flat 95d0 400d0)))))

(test a-turned-text-is-picked-by-its-box-and-not-by-its-bounds
  (let* ((text (angled-text :angle (/ pi 4)))
         (bounds (dom:entity-bounds text)))
    ;; The corner of the bounds is empty space for a text at 45 degrees.
    (is (plusp (dom:entity-pick-distance text (geo:bounds-max-x bounds) (geo:bounds-min-y bounds))))
    (is (not (dom:entity-touches-bounds-p text (geo:make-bounds (- (geo:bounds-max-x bounds) 5d0)
                                                                (geo:bounds-min-y bounds)
                                                                (geo:bounds-max-x bounds)
                                                                (+ (geo:bounds-min-y bounds) 5d0))))
        "a crossing in that corner does not take it")
    (is-true (dom:entity-touches-bounds-p text (geo:make-bounds 100d0 200d0 110d0 210d0)))))

(test moving-editing-and-copying-a-text-keep-its-angle
  (let ((text (angled-text :angle 1.2d0)))
    (is (= 1.2d0 (dom:text-angle (dom:entity-translated text 50d0 -20d0))))
    (is (= 1.2d0 (dom:text-angle (dom:entity-with-property text :string "Hall"))))
    (is (= 1.2d0 (dom:text-angle (dom:entity-with-layer text 0))))
    (is (= 1.2d0 (dom:text-angle (dom:entity-with-identity text 9 0))))
    (is (= 0.3d0 (dom:text-angle (dom:entity-with-property text :angle 0.3d0))))
    (let ((moved (dom:entity-translated text 50d0 -20d0)))
      (is (= 150d0 (dom:text-x moved)))
      (is (= 180d0 (dom:text-y moved))))))

;;; The inspector

(defun app-with-text (&key (angle 0d0))
  "Return an application whose drawing has one text, selected."
  (let ((application (make-test-app)))
    (app:commit-changes application
                        (dom:create-entity-changes (angled-text :x 0d0 :y 0d0 :angle angle))
                        "entity.create")
    (app:operation-select-all application)
    application))

(test the-inspector-shows-the-angle-in-degrees-and-edits-it-in-degrees
  (let ((application (app-with-text :angle (/ pi 6))))
    (is (string= "Angle" (ui:widget-label (find-widget application '(:label (:property :angle))))))
    (is (string= "30" (ui:widget-label (find-widget application '(:property :angle)))))
    (click-widget application '(:property :angle))
    (type-text application "90")
    (press application :enter)
    (is (< (abs (- (/ pi 2) (dom:text-angle (first (app:selected-entities application))))) 1d-12))
    (is (string= "90" (ui:widget-label (find-widget application '(:property :angle)))))
    (click-widget application '(:property :angle))
    (type-text application "-45,5")
    (press application :enter)
    (is (string= "314.5" (ui:widget-label (find-widget application '(:property :angle)))) "kept within one turn")
    (click-widget application '(:property :angle))
    (type-text application "turn")
    (press application :enter)
    (is (eq :error (cdr (app:app-message application))) "text that is not a number is refused")))

;;; The tool

(defun place-text (application from-x from-y to-x to-y string)
  "Drag from one world point to another with the text tool and type STRING in the prompt."
  (press application :t)
  (drag application from-x from-y to-x to-y)
  (type-text application string)
  (press application :enter))

(test a-click-places-a-horizontal-text-and-a-drag-sets-the-angle
  (let ((application (make-test-app)))
    (press application :t)
    (click application 100 100)
    (type-text application "flat")
    (press application :enter)
    (is (= 0d0 (dom:text-angle (last-entity application))))
    (place-text application 0 500 1000 1000 "slope")
    (let ((angle (dom:text-angle (last-entity application))))
      (is (< (abs (- (atan 500d0 1000d0) angle)) 0.1d0) "about 27 degrees: ~a" angle)
      (is (< 0d0 angle (/ pi 2))))))

(test a-dragged-text-angle-snaps-to-the-quarter-turns-and-the-fifteen-degree-steps
  (let ((application (make-test-app)))
    (place-text application 0 0 20 1000 "upright")
    (is (= (/ pi 2) (dom:text-angle (last-entity application))) "a little off 90 degrees snaps to it")
    (place-text application 0 0 1000 -1000 "down-right")
    (is (< (abs (- (* 7 (/ pi 4)) (dom:text-angle (last-entity application)))) 1d-12) "315 degrees")
    (place-text application 0 0 1000 400 "free")
    (is (< (abs (- (atan 400d0 1000d0) (dom:text-angle (last-entity application)))) 1d-12)
        "21.8 degrees is not within 3 of 15 or of 30")))

(test a-text-being-dragged-is-previewed-at-its-angle-and-escape-abandons-it
  (let ((application (make-test-app)))
    (press application :t)
    (send application (pointer application :move nil 0 0)
          (pointer application :down :left 0 0)
          (pointer application :move nil 500 500)
          (pointer application :move nil 1000 1000))
    (let ((preview (app:tool-preview-entities (app:app-tool application) application)))
      (is (= 1 (length preview)))
      (is (< (abs (- (/ pi 4) (dom:text-angle (first preview)))) 1d-9)))
    (is-true (app:tool-in-progress-p (app:app-tool application)))
    (press application :escape)
    (is (null (app:tool-preview-entities (app:app-tool application) application)))
    (is (null (app:app-prompt application)))
    (send application (pointer application :up :left 1000 1000))
    (is (null (app:app-prompt application)) "the release after the abandoned drag asks for nothing")))

(test the-text-tool-prompt-says-how-to-turn-the-text
  (let ((application (make-test-app)))
    (press application :t)
    (is (search "drag to set its angle" (app:tool-prompt (app:app-tool application) application)))))

;;; The drawing

(test a-text-with-an-angle-is-drawn-rotated
  (let ((application (app-with-text :angle (/ pi 3))))
    (app:app-render application)
    (let ((runs (coerce (remove-if-not (lambda (run) (and (string= "Kitchen" (scene:text-run-string run))
                                                          (eq :mono (scene:text-run-font run))))
                                       (scene:draw-list-text-runs (app:app-draw-list application)))
                        'list)))
      (is (= 1 (length runs)))
      (is (< (abs (- (/ pi 3) (scene:text-run-angle (first runs)))) 1d-5)))))

(test a-text-angle-survives-saving-and-opening
  (let* ((drawing (dom:make-drawing)))
    (dom:apply-changes drawing (dom:create-entity-changes (angled-text :angle 2.25d0)))
    (let ((back (persist:parse-drawing-text (persist:drawing-to-string drawing))))
      (is (= 2.25d0 (dom:text-angle (first (dom:drawing-entities back))))))))
