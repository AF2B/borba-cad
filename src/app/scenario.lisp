(in-package #:borba-cad/app)

;;;; Scripted interaction. A scenario is a list of steps, each a function of no arguments that
;;;; feeds a few real input events to the application. The window loop runs one step every few
;;;; frames, so a demo produces the same measurements as a person drawing; tests and benchmarks
;;;; run the same steps without a window.

(defconstant +demo-scale+ 0.1d0
  "Zoom of the demo view in pixels per millimetre: a 10 m wide plan fills 1000 pixels.")

(defconstant +demo-wheel-notches+ 3d0
  "Wheel notches the demo zooms in and out by.")

(defun screen-of (app
                  world-x
                  world-y)
  "Return the window position of a world point as two values.

APP: the application.
WORLD-X: world x in millimetres.
WORLD-Y: world y in millimetres."
  (view:world->screen (app-viewport app)
                      (float world-x 1d0)
                      (float world-y 1d0)))

(defun scenario-click (app
                       world-x
                       world-y
                       &key (button :left)
                            modifiers)
  "Move to a world position, press and release a button, as a person would.

APP: the application.
WORLD-X: world x in millimetres.
WORLD-Y: world y in millimetres.
BUTTON: :left, :middle or :right.
MODIFIERS: modifier keys held."
  (multiple-value-bind (x y) (screen-of app
                                        world-x
                                        world-y)
    (app-handle-event app (make-pointer-event :move
                                              nil
                                              x
                                              y
                                              modifiers))
    (app-handle-event app (make-pointer-event :down
                                              button
                                              x
                                              y
                                              modifiers))
    (app-handle-event app (make-pointer-event :up
                                              button
                                              x
                                              y
                                              modifiers))))

(defun scenario-drag (app
                      from-x
                      from-y
                      to-x
                      to-y
                      &key (button :left)
                           (steps 8))
  "Press at one world position, move in steps and release at another.

APP: the application.
FROM-X: start world x.
FROM-Y: start world y.
TO-X: end world x.
TO-Y: end world y.
BUTTON: the button held.
STEPS: number of intermediate pointer moves."
  (multiple-value-bind (x y) (screen-of app
                                        from-x
                                        from-y)
    (app-handle-event app (make-pointer-event :move
                                              nil
                                              x
                                              y))
    (app-handle-event app (make-pointer-event :down
                                              button
                                              x
                                              y)))
  (loop for step from 1 to steps
        do (let ((fraction (/ step (float steps 1d0))))
             (multiple-value-bind (x y)
                 (screen-of app
                            (+ from-x (* fraction (- to-x from-x)))
                            (+ from-y (* fraction (- to-y from-y))))
               (app-handle-event app (make-pointer-event :move
                                                         nil
                                                         x
                                                         y)))))
  (multiple-value-bind (x y) (screen-of app
                                        to-x
                                        to-y)
    (app-handle-event app (make-pointer-event :up
                                              button
                                              x
                                              y))))

(defun scenario-key (app
                     key
                     &optional modifiers)
  "Press and release a key.

APP: the application.
KEY: key keyword.
MODIFIERS: list of the modifier keys held."
  (app-handle-event app (make-key-event key
                                        modifiers
                                        :down))
  (app-handle-event app (make-key-event key
                                        modifiers
                                        :up)))

(defun scenario-type (app
                      text)
  "Type text.

APP: the application.
TEXT: the characters."
  (app-handle-event app (make-text-event text)))

(defun scenario-wheel (app
                       world-x
                       world-y
                       notches)
  "Turn the mouse wheel with the pointer over a world position.

APP: the application.
WORLD-X: world x under the pointer.
WORLD-Y: world y under the pointer.
NOTCHES: positive to zoom in."
  (multiple-value-bind (x y) (screen-of app
                                        world-x
                                        world-y)
    (app-handle-event app (make-wheel-event (float notches 1d0)
                                            x
                                            y))))

(defun demo-view (app)
  "Show the area of the demo plan.

APP: the application."
  (let ((viewport (app-viewport app)))
    (setf (app-viewport app)
          (view:make-viewport :center-x 5000d0
                              :center-y 3200d0
                              :scale +demo-scale+
                              :width (view:viewport-width viewport)
                              :height (view:viewport-height viewport)
                              :origin-x (view:viewport-origin-x viewport)
                              :origin-y (view:viewport-origin-y viewport)))
    (app-request-redraw app)))

;;; Each row is one step of the demonstration. The first element names what happens and the
;;; others are its arguments, in world millimetres where they are coordinates.
(defparameter *demo-script*
  '((:view)
    ;; Layers, then the walls.
    (:add-layer "walls")
    (:key :r)
    (:click 0 0)
    (:click 10000 7000)
    (:key :l)
    (:click 6000 0)
    (:click 6000 7000)
    (:click 0 3500)
    (:click 6000 3500)
    ;; Furniture on its own layer.
    (:add-layer "furniture")
    (:key :c)
    (:click 1000 1000)
    (:click 1300 1000)
    (:click 9000 6000)
    (:click 9300 6000)
    (:key :p)
    (:click 7000 1000)
    (:click 9000 1000)
    (:click 9000 2500)
    (:click 7000 2500)
    (:click 7000 1000)
    (:key :p)
    (:click 1500 4500)
    (:click 3500 4500)
    (:click 3500 6000)
    (:key :enter)
    ;; Annotations: text and a dimension.
    (:add-layer "annotations")
    (:key :t)
    (:click 500 3000)
    (:type "Living room")
    (:key :enter)
    (:click 6500 3000)
    (:type "Kitchen")
    (:key :enter)
    (:key :d)
    (:click 0 0)
    (:click 10000 0)
    (:click 5000 -700)
    (:key :escape)
    ;; Selection, move and undo: grab a column by its outline, move it, undo it, then select by
    ;; a window.
    (:click 1000 1300)
    (:drag 1000 1300 1500 1800)
    (:key :z :control)
    (:drag -500 -500 4000 4000)
    (:key :escape)
    ;; Delete the same column, then bring it back with undo.
    (:click 1000 1300)
    (:key :delete)
    (:key :z :control)
    ;; View: zoom in and out around the plan three times, pan, fit.
    (:zoom-in 5000 3500)
    (:zoom-out 5000 3500)
    (:zoom-in 5000 3500)
    (:zoom-out 5000 3500)
    (:zoom-in 5000 3500)
    (:zoom-out 5000 3500)
    (:drag 5000 3500 4500 3000 :button :middle)
    ;; Files: save the plan under another name, then open that file again.
    (:save-as)
    (:open)
    (:key :home)
    ;; The overlay, the grid twice (off and on again) and the display unit through its cycle.
    (:key :f3)
    (:key :g)
    (:key :g)
    (:key :u)
    (:key :u)
    (:key :u))
  "The demonstration scenario: layers, a floor plan drawn with every tool, a selection and a
move, delete and undo, zoom, pan, a save and an open, the overlay and the display unit.")

(defun scenario-save-as (app)
  "Save the drawing through the Save As prompt, typing the demonstration path. The folder of
that path is created first, because saving never creates folders.

APP: the application."
  (ensure-directories-exist (demo-save-path app))
  (scenario-key app
                :s
                '(:control :shift))
  (scenario-type app (demo-save-path app))
  (scenario-key app :enter))

(defun scenario-open (app)
  "Open the demonstration file through the Open prompt.

APP: the application."
  (scenario-key app
                :o
                '(:control))
  (scenario-type app (demo-save-path app))
  (scenario-key app :enter))

(defun run-scenario-step (app
                          step)
  "Run one row of a scenario script.

APP: the application.
STEP: a list of the step kind and its arguments, as in *DEMO-SCRIPT*."
  (destructuring-bind (kind &rest arguments) step
    (ecase kind
      (:view (demo-view app))
      (:add-layer (operation-add-layer app (first arguments)))
      (:save-as (scenario-save-as app))
      (:open (scenario-open app))
      (:key (scenario-key app
                          (first arguments)
                          (rest arguments)))
      (:type (scenario-type app (first arguments)))
      (:click (apply #'scenario-click app arguments))
      (:drag (apply #'scenario-drag app arguments))
      (:zoom-in (scenario-wheel app
                                (first arguments)
                                (second arguments)
                                +demo-wheel-notches+))
      (:zoom-out (scenario-wheel app
                                 (first arguments)
                                 (second arguments)
                                 (- +demo-wheel-notches+))))))

(defun demo-steps (app)
  "Return the steps of the demonstration scenario as functions of no arguments: layers, a floor
plan drawn with every tool, a selection and a move, undo, zoom, pan, the overlay, and a save.

APP: the application."
  (mapcar (lambda (step)
            (lambda ()
              (run-scenario-step app step)))
          *demo-script*))

(defun demo-save-path (app)
  "Return the path where the demonstration saves its drawing: var/demo.bcad in the project.

APP: the application."
  (namestring (merge-pathnames "var/demo.bcad" (base:config-project-root (app-config app)))))

;;; Benchmark input

(defconstant +benchmark-zoom-notches+ 0.25d0
  "Wheel notches per benchmark frame; the sign alternates so that the zoom oscillates.")

(defconstant +benchmark-zoom-period-frames+ 20
  "Benchmark frames between two reversals of the zoom direction.")

(defconstant +benchmark-pointer-wobble-x+ 7
  "Pixels, modulo, that the benchmark pointer moves horizontally from frame to frame.")

(defconstant +benchmark-pointer-wobble-y+ 5
  "Pixels, modulo, that the benchmark pointer moves vertically from frame to frame.")

(defun apply-benchmark-input (app
                              frame)
  "Feed the application the input of one benchmark frame: a wheel step at the canvas center
and a small pointer movement, then ask for a redraw. Every benchmark, headless or windowed,
uses this same interaction so that their frame costs are comparable.

APP: the application.
FRAME: frame number, which selects the zoom direction."
  (let* ((viewport (app-viewport app))
         (x (+ (view:viewport-origin-x viewport) (/ (view:viewport-width viewport) 2)))
         (y (+ (view:viewport-origin-y viewport) (/ (view:viewport-height viewport) 2)))
         (direction (if (evenp (floor frame +benchmark-zoom-period-frames+)) 1 -1)))
    (app-handle-event app (make-wheel-event (* direction +benchmark-zoom-notches+)
                                            x
                                            y))
    (app-handle-event app (make-pointer-event :move
                                              nil
                                              (+ x (mod frame +benchmark-pointer-wobble-x+))
                                              (+ y (mod frame +benchmark-pointer-wobble-y+))))
    (app-request-redraw app)))

;;; The shipped example

(defun write-demo-drawing (config
                           path-text)
  "Run the demonstration scenario without a window and save the drawing it produces. The result
is the example plan shipped in examples/, so the example is always what the program itself
draws.

CONFIG: the configuration, which supplies the project root.
PATH-TEXT: where to save the drawing."
  (let ((application (make-app config)))
    (ensure-directories-exist path-text)
    (dolist (step (demo-steps application))
      (funcall step))
    (unless (save-app-drawing application path-text)
      (error 'base:drawing-file-io-error
             :path path-text
             :reason (format nil "the example could not be saved: ~a" (app-last-error application))))
    (dom:drawing-entity-count (app-drawing application))))
