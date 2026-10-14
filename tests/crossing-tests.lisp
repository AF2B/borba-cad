(in-package #:borba-cad/tests)

(def-suite crossing
  :description "Crossing selection: dragging from right to left takes what the rectangle touches."
  :in all)

(in-suite crossing)

(defun shapes-drawing ()
  "Return a drawing with a line, a rectangle, a circle, a polyline, a text and a dimension, spread
apart so that each can be reached alone."
  (let ((drawing (dom:make-drawing))
        (layer dom:+default-layer-id+))
    (dom:apply-changes drawing
                       (dom:create-entities-changes
                        (list (dom:make-line :id 1 :layer-id layer :x1 0d0 :y1 0d0 :x2 1000d0 :y2 0d0)
                              (dom:make-rectangle :id 2 :layer-id layer :min-x 0d0 :min-y 2000d0
                                                  :width 1000d0 :height 500d0)
                              (dom:make-circle :id 3 :layer-id layer :center-x 5000d0 :center-y 0d0 :radius 400d0)
                              (dom:make-polyline :id 4 :layer-id layer
                                                 :coordinates (coerce '(0d0 -3000d0 1000d0 -3000d0 1000d0 -4000d0)
                                                                      '(simple-array double-float (*)))
                                                 :closed nil)
                              (dom:make-text :id 5 :layer-id layer :x 5000d0 :y 2000d0 :string "Kitchen"
                                             :height 250d0)
                              (dom:make-dimension :id 6 :layer-id layer :x1 5000d0 :y1 -3000d0 :x2 6000d0
                                                  :y2 -3000d0 :offset 500d0 :text-height 250d0
                                                  :unit :millimeter :precision 0))))
    drawing))

(defun crossing-ids (drawing x1 y1 x2 y2)
  "Return the ids of the entities that a crossing window with the given corners takes."
  (mapcar #'dom:entity-id (dom:entities-crossing-window drawing (geo:bounds-from-corners x1 y1 x2 y2))))

(defun window-ids (drawing x1 y1 x2 y2)
  "Return the ids of the entities that a window with the given corners takes."
  (mapcar #'dom:entity-id (dom:entities-in-window drawing (geo:bounds-from-corners x1 y1 x2 y2))))

;;; In the domain

(test a-crossing-takes-what-it-touches-where-a-window-takes-only-what-it-holds
  (let ((drawing (shapes-drawing)))
    (is (equal '(1) (crossing-ids drawing -100d0 -100d0 100d0 100d0)) "the end of the line")
    (is (null (window-ids drawing -100d0 -100d0 100d0 100d0)) "which a window does not take")
    (is (equal '(1) (window-ids drawing -100d0 -100d0 1100d0 100d0)) "unless it holds the whole line")
    (is (equal '(1) (crossing-ids drawing -100d0 -100d0 1100d0 100d0)))))

(test a-crossing-inside-a-closed-shape-takes-nothing-where-its-outline-is-not-reached
  (let ((drawing (shapes-drawing)))
    (is (null (crossing-ids drawing 400d0 2200d0 600d0 2300d0)) "inside the rectangle")
    (is (equal '(2) (crossing-ids drawing 400d0 2400d0 600d0 2600d0)) "over its top edge")
    (is (null (crossing-ids drawing 4950d0 -50d0 5050d0 50d0)) "inside the circle")
    (is (equal '(3) (crossing-ids drawing 5350d0 -50d0 5450d0 50d0)) "over the circle's outline")))

(test a-crossing-takes-a-polyline-a-text-and-a-dimension-where-they-are
  (let ((drawing (shapes-drawing)))
    (is (equal '(4) (crossing-ids drawing 900d0 -3500d0 1100d0 -3400d0)) "the corner of the polyline")
    (is (null (crossing-ids drawing 100d0 -3900d0 900d0 -3100d0)) "the polyline's empty inside")
    (is (equal '(5) (crossing-ids drawing 5100d0 2050d0 5200d0 2100d0)) "the box of the text")
    (is (equal '(6) (crossing-ids drawing 5400d0 -2550d0 5600d0 -2450d0)) "the line of the dimension")
    (is (null (crossing-ids drawing 5400d0 -3400d0 5600d0 -3300d0)) "empty space under the dimension")))

(test a-crossing-answers-in-stacking-order-and-skips-hidden-and-locked-layers
  (let ((drawing (shapes-drawing)))
    (is (equal '(1 2 3 4 5 6) (crossing-ids drawing -10000d0 -10000d0 10000d0 10000d0)))
    (dom:set-layer-locked drawing dom:+default-layer-id+ t)
    (is (null (crossing-ids drawing -10000d0 -10000d0 10000d0 10000d0)) "locked")
    (dom:set-layer-locked drawing dom:+default-layer-id+ nil)
    (dom:set-layer-visible drawing dom:+default-layer-id+ nil)
    (is (null (crossing-ids drawing -10000d0 -10000d0 10000d0 10000d0)) "hidden")))

;;; With the mouse

(test dragging-from-right-to-left-selects-what-the-rectangle-touches
  (let ((application (two-lines-app)))
    (drag application 500 100 -100 -100)                  ; from the right: touches the first line only
    (is (equal '(1) (app:selection-ids application)))
    (drag application 500 400 -100 -100)                  ; touches both
    (is (equal '(1 2) (app:selection-ids application)))
    (drag application -100 -100 500 400)                  ; the other way, a window: holds neither line
    (is (null (app:selection-ids application)))))

(test shift-adds-a-crossing-to-the-selection
  (let ((application (two-lines-app)))
    (drag application 500 100 -100 -100)
    (is (equal '(1) (app:selection-ids application)))
    (drag application 500 400 -100 200 :modifiers '(:shift))
    (is (equal '(1 2) (app:selection-ids application)))))

(test a-crossing-in-progress-is-drawn-dashed-and-a-window-is-not
  (let ((application (two-lines-app)))
    (flet ((lines-while-dragging (from-x to-x)
             (send application (pointer application :move nil from-x 100)
                   (pointer application :down :left from-x 100))
             (send application (pointer application :move nil (/ (+ from-x to-x) 2) -100)
                   (pointer application :move nil to-x -100))
             (app:app-render application)
             (let ((count (scene:draw-list-line-count (app:app-draw-list application)))
                   (crossing (app:app-window-crossing application)))
               (send application (pointer application :up :left to-x -100))
               (values count crossing))))
      (multiple-value-bind (window-lines window-crossing) (lines-while-dragging -300 300)
        (multiple-value-bind (crossing-lines crossing-flag) (lines-while-dragging 300 -300)
          (is (null window-crossing))
          (is-true crossing-flag)
          (is (> crossing-lines (+ window-lines 10)) "the dashes are many lines, the window outline four"))))))

(test escape-abandons-a-crossing-in-progress
  (let ((application (two-lines-app)))
    (send application (pointer application :move nil 500 100)
          (pointer application :down :left 500 100)
          (pointer application :move nil 0 -100)
          (pointer application :move nil -100 -100))
    (is-true (app:app-window-crossing application))
    (press application :escape)
    (is (null (app:app-window application)))
    (is (null (app:app-window-crossing application)))))

(test the-select-prompt-tells-both-directions
  (let ((application (make-test-app)))
    (is (search "left to right" (app:tool-prompt (app:app-tool application) application)))
    (is (search "right to left" (app:tool-prompt (app:app-tool application) application)))))
