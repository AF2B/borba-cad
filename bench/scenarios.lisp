(in-package #:borba-cad/bench)

;;;; Benchmark scenarios. Each takes a drawing and returns a list of measurements. They record
;;;; what happened; they never assert performance targets.

(defconstant +canvas-width+ 1440d0
  "Canvas width, in logical pixels, of every benchmark viewport.")

(defconstant +canvas-height+ 900d0
  "Canvas height, in logical pixels, of every benchmark viewport.")

(defconstant +empty-site-side+ 20000d0
  "Side, in millimetres, of the site assumed for an empty drawing.")

(defconstant +working-view-fraction+ 0.1d0
  "Fraction of the site width visible at the typical working zoom.")

(defconstant +selection-window-fraction+ 0.1d0
  "Side of a benchmark selection window, as a fraction of the site side.")

(defconstant +pan-steps+ 60
  "Viewports visited by one pan sweep.")

(defconstant +zoom-steps+ 40
  "Viewports visited by one zoom sweep.")

(defconstant +scene-repetitions+ 20
  "Base repetitions of the all-in-view scene build.")

(defconstant +pan-repetitions+ 300
  "Base repetitions of the pan and zoom sweeps (five sweeps).")

(defconstant +pick-repetitions+ 2000
  "Base repetitions of the picking measurement.")

(defconstant +window-select-repetitions+ 200
  "Base repetitions of the window-selection measurement.")

(defconstant +snap-repetitions+ 1000
  "Base repetitions of the snapping measurement.")

(defconstant +edit-repetitions+ 100
  "Base repetitions of each edit operation.")

(defconstant +persistence-repetitions+ 5
  "Base repetitions of saving and loading.")

(defconstant +soak-cycles+ 400
  "Create-and-delete cycles of the memory soak.")

(defconstant +soak-entities-per-cycle+ 50
  "Entities created (then deleted) by each soak cycle.")

(defconstant +soak-checkpoint-interval+ 100
  "Soak cycles between two heap measurements.")

(defconstant +soak-seed-offset+ 7
  "Added to the benchmark seed for the entities created by the soak.")

;;; Viewports and inputs

(defun site-bounds (drawing)
  "Return the bounds of every entity in DRAWING, or a default square site when it is empty.

DRAWING: the drawing."
  (let ((union nil))
    (dom:map-entities (lambda (entity)
                        (let ((bounds (dom:entity-bounds entity)))
                          (setf union (if union
                                          (geo:bounds-union union bounds)
                                          bounds))))
                      (dom:drawing-store drawing))
    (or union
        (geo:make-bounds 0d0
                         0d0
                         +empty-site-side+
                         +empty-site-side+))))

(defun benchmark-viewport (center-x
                           center-y
                           scale)
  "Return a viewport of the benchmark canvas size.

CENTER-X: world x at the canvas center.
CENTER-Y: world y at the canvas center.
SCALE: pixels per millimetre."
  (view:make-viewport :center-x center-x
                      :center-y center-y
                      :scale scale
                      :width +canvas-width+
                      :height +canvas-height+))

(defun working-scale (bounds)
  "Return the scale at which +WORKING-VIEW-FRACTION+ of the site width fills the canvas.

BOUNDS: the site bounds."
  (/ +canvas-width+
     (* +working-view-fraction+
        (max (geo:bounds-width bounds) base:+drawing-resolution+))))

(defun pan-path (bounds)
  "Return a vector of viewports that sweep the working zoom from one corner of the site to
the opposite corner.

BOUNDS: the site bounds."
  (let* ((scale (working-scale bounds))
         (half-width (/ +canvas-width+ scale 2))
         (half-height (/ +canvas-height+ scale 2))
         (start-x (+ (geo:bounds-min-x bounds) half-width))
         (start-y (+ (geo:bounds-min-y bounds) half-height))
         (end-x (max start-x (- (geo:bounds-max-x bounds) half-width)))
         (end-y (max start-y (- (geo:bounds-max-y bounds) half-height)))
         (path (make-array +pan-steps+)))
    (dotimes (step +pan-steps+ path)
      (let ((fraction (/ step (1- +pan-steps+))))
        (setf (svref path step)
              (benchmark-viewport (+ start-x (* fraction (- end-x start-x)))
                                  (+ start-y (* fraction (- end-y start-y)))
                                  scale))))))

(defun zoom-path (bounds)
  "Return a vector of viewports that zoom in, notch by notch, from the whole site to its
center.

BOUNDS: the site bounds."
  (let ((viewport (view:fit-viewport-to-bounds (benchmark-viewport (geo:bounds-center-x bounds)
                                                                   (geo:bounds-center-y bounds)
                                                                   1d0)
                                               bounds))
        (path (make-array +zoom-steps+)))
    (dotimes (step +zoom-steps+ path)
      (setf (svref path step) viewport
            viewport (view:zoom-viewport-at viewport
                                            app:+zoom-step-per-notch+
                                            (/ +canvas-width+ 2)
                                            (/ +canvas-height+ 2))))))

(defun random-points (prng
                      bounds
                      count)
  "Return a vector of COUNT random (x . y) world points inside BOUNDS.

PRNG: the generator.
BOUNDS: where the points lie.
COUNT: how many points."
  (let ((points (make-array count)))
    (dotimes (index count points)
      (setf (svref points index)
            (cons (rng:prng-next-double prng
                                        (geo:bounds-min-x bounds)
                                        (geo:bounds-max-x bounds))
                  (rng:prng-next-double prng
                                        (geo:bounds-min-y bounds)
                                        (geo:bounds-max-y bounds)))))))

(defun make-scene-renderer (drawing)
  "Return a function of a viewport that builds the scene of DRAWING into one reused draw
list and returns the scene result.

DRAWING: the drawing."
  (let ((list (scene:make-draw-list))
        (state (scene:make-scene-state))
        (theme (scene:default-theme)))
    (lambda (viewport)
      (scene:draw-list-reset list)
      (scene:build-scene list
                         drawing
                         viewport
                         state
                         theme))))

(defun scene-note (result)
  "Return the note describing a scene result.

RESULT: a scene result."
  (format nil "last scene: ~d entities drawn, ~d culled"
          (scene:scene-result-drawn-entities result)
          (scene:scene-result-culled-entities result)))

;;; Scene

(defun measure-scene-all-in-view (drawing
                                  entity-count)
  "Measure building the scene with the whole drawing in view, the worst case for drawing.

DRAWING: the drawing.
ENTITY-COUNT: number of entities in it."
  (let* ((bounds (site-bounds drawing))
         (viewport (view:fit-viewport-to-bounds (benchmark-viewport (geo:bounds-center-x bounds)
                                                                    (geo:bounds-center-y bounds)
                                                                    1d0)
                                                bounds))
         (render (make-scene-renderer drawing))
         (result nil)
         (measurement (measure-operation "scene.build, all entities in view"
                                         (scaled-repetitions +scene-repetitions+ entity-count)
                                         (lambda (index)
                                           (declare (ignore index))
                                           (setf result (funcall render viewport))))))
    (push (scene-note result) (measurement-notes measurement))
    (list measurement)))

(defun measure-sweep (name
                      drawing
                      entity-count
                      path)
  "Measure building the scene at each viewport of PATH, repeatedly, while recording GC events.

NAME: label of the measurement.
DRAWING: the drawing.
ENTITY-COUNT: number of entities in it.
PATH: vector of viewports to visit."
  (let ((render (make-scene-renderer drawing))
        (steps (length path))
        (result nil))
    (multiple-value-bind (measurement collections total maximum)
        (call-collecting-gc
         (lambda ()
           (measure-operation name
                              (scaled-repetitions +pan-repetitions+ entity-count)
                              (lambda (index)
                                (setf result (funcall render (svref path (mod index steps))))))))
      (push (scene-note result) (measurement-notes measurement))
      (push (gc-note collections
                     total
                     maximum) (measurement-notes measurement))
      (list measurement))))

(defun measure-scene-pan (drawing
                          entity-count)
  "Measure scene builds while panning at the working zoom across the whole site.

DRAWING: the drawing.
ENTITY-COUNT: number of entities in it."
  (measure-sweep "scene.build, pan sweep at working zoom"
                 drawing
                 entity-count
                 (pan-path (site-bounds drawing))))

(defun measure-scene-zoom (drawing
                           entity-count)
  "Measure scene builds while zooming in from the whole site.

DRAWING: the drawing.
ENTITY-COUNT: number of entities in it."
  (measure-sweep "scene.build, zoom sweep from whole site"
                 drawing
                 entity-count
                 (zoom-path (site-bounds drawing))))

;;; Interaction queries

(defun measure-picking (drawing
                        entity-count)
  "Measure picking the entity under a random point, at the working zoom.

DRAWING: the drawing.
ENTITY-COUNT: number of entities in it."
  (let* ((bounds (site-bounds drawing))
         (repetitions (scaled-repetitions +pick-repetitions+ entity-count))
         (points (random-points (rng:make-prng (+ +benchmark-seed+ 1))
                                bounds
                                repetitions))
         (radius (/ app:+pick-radius-pixels+ (working-scale bounds)))
         (hits 0)
         (measurement (measure-operation "pick.entity, random point"
                                         repetitions
                                         (lambda (index)
                                           (let ((point (svref points (mod index repetitions))))
                                             (when (and (dom:pick-entity drawing
                                                                         (car point)
                                                                         (cdr point)
                                                                         radius)
                                                        (>= index 0))
                                               (incf hits)))))))
    (push (format nil "~d of ~d random points hit an entity" hits repetitions)
          (measurement-notes measurement))
    (list measurement)))

(defun measure-window-selection (drawing
                                 entity-count)
  "Measure selecting the entities fully inside a random window covering a tenth of the site
side.

DRAWING: the drawing.
ENTITY-COUNT: number of entities in it."
  (let* ((bounds (site-bounds drawing))
         (repetitions (scaled-repetitions +window-select-repetitions+ entity-count))
         (points (random-points (rng:make-prng (+ +benchmark-seed+ 2))
                                bounds
                                repetitions))
         (side (* +selection-window-fraction+
                  (max (geo:bounds-width bounds) (geo:bounds-height bounds))))
         (selected 0)
         (measurement (measure-operation "select.window, random window"
                                         repetitions
                                         (lambda (index)
                                           (let* ((point (svref points (mod index repetitions)))
                                                  (found (dom:entities-in-window
                                                          drawing
                                                          (geo:make-bounds (car point)
                                                                           (cdr point)
                                                                           (+ (car point) side)
                                                                           (+ (cdr point) side)))))
                                             (when (>= index 0)
                                               (incf selected (length found))))))))
    (push (format nil "on average ~a entities selected per window"
                  (base:format-decimal (/ selected repetitions) 1))
          (measurement-notes measurement))
    (list measurement)))

(defun measure-snapping (drawing
                         entity-count)
  "Measure object-snap lookup for a random pointer position, at the working zoom.

DRAWING: the drawing.
ENTITY-COUNT: number of entities in it."
  (let* ((bounds (site-bounds drawing))
         (repetitions (scaled-repetitions +snap-repetitions+ entity-count))
         (points (random-points (rng:make-prng (+ +benchmark-seed+ 3))
                                bounds
                                repetitions))
         (radius (/ app:+snap-radius-pixels+ (working-scale bounds)))
         (previous-setting (dom:drawing-object-snap drawing)))
    (dom:set-object-snap drawing t)
    (unwind-protect
         (list (measure-operation "snap.object, random point"
                                  repetitions
                                  (lambda (index)
                                    (let ((point (svref points (mod index repetitions))))
                                      (dom:snap-point drawing
                                                      (car point)
                                                      (cdr point)
                                                      radius)))))
      (dom:set-object-snap drawing previous-setting))))

;;; Editing

(defun benchmark-line (drawing)
  "Return a new valid line with the next free id of DRAWING, in the middle of the site.

DRAWING: the drawing."
  (dom:make-line :id (dom:drawing-next-entity-id drawing)
                 :layer-id (dom:drawing-current-layer-id drawing)
                 :x1 1000d0
                 :y1 1000d0
                 :x2 2000d0
                 :y2 3000d0))

(defun measure-editing (drawing
                        entity-count)
  "Measure committing one entity creation and one deletion through the history, and undoing
and redoing them. Afterwards the drawing holds the same entities as before; only its revision
and id counter have advanced.

DRAWING: the drawing, modified temporarily.
ENTITY-COUNT: number of entities in it."
  (let ((history (dom:make-history))
        (repetitions (scaled-repetitions +edit-repetitions+ entity-count))
        (created-ids '()))
    (flet ((measure-edit (name function)
             (measure-operation name
                                repetitions
                                (lambda (index)
                                  (declare (ignore index))
                                  (funcall function))
                                :warmup 0)))
      (list (measure-edit "edit.create, one entity"
                          (lambda ()
                            (let ((entity (benchmark-line drawing)))
                              (dom:history-commit history
                                                  drawing
                                                  (dom:create-entity-changes entity))
                              (push (dom:entity-id entity) created-ids))))
            (measure-edit "edit.undo, one creation"
                          (lambda ()
                            (dom:history-undo history drawing)))
            (measure-edit "edit.redo, one creation"
                          (lambda ()
                            (dom:history-redo history drawing)))
            (measure-edit "edit.delete, one entity"
                          (lambda ()
                            (dom:history-commit history
                                                drawing
                                                (dom:delete-entities-changes drawing
                                                                             (list (pop created-ids))))))))))

;;; Persistence

(defparameter *benchmark-file-name* "benchmark.bcad"
  "Name of the scratch drawing file written by the persistence measurements.")

(defun measure-persistence (drawing
                            entity-count
                            directory)
  "Measure saving DRAWING to a file and loading it back, and verify that the loaded drawing is
identical to the saved one.

DRAWING: the drawing.
ENTITY-COUNT: number of entities in it.
DIRECTORY: scratch directory that receives the file; the file is deleted afterwards."
  (let* ((path (merge-pathnames *benchmark-file-name* directory))
         (repetitions (scaled-repetitions +persistence-repetitions+ entity-count))
         (expected (rng:drawing-checksum drawing))
         (bytes 0)
         (loaded nil)
         (save (measure-operation "file.save (serialize, flush, rename)"
                                  repetitions
                                  (lambda (index)
                                    (declare (ignore index))
                                    (setf bytes (getf (nth-value 1 (persist:save-drawing drawing path))
                                                      :bytes)))
                                  :warmup 1
                                  :unit :milliseconds))
         (load (measure-operation "file.load (read, parse, validate)"
                                  repetitions
                                  (lambda (index)
                                    (declare (ignore index))
                                    (setf loaded (persist:load-drawing path)))
                                  :warmup 1
                                  :unit :milliseconds))
         (actual (rng:drawing-checksum loaded)))
    (uiop:delete-file-if-exists path)
    (push (format nil "~:d bytes on disk" bytes) (measurement-notes save))
    (push (if (string= expected actual)
              (format nil "round trip verified: checksum ~a before and after" expected)
              (format nil "ROUND TRIP MISMATCH: checksum ~a before, ~a after" expected actual))
          (measurement-notes load))
    (list save load)))

;;; Memory

(defun run-memory-soak (drawing)
  "Create and delete batches of entities through the history many times, measuring the heap in
use after a full collection at regular checkpoints and once more after clearing the history.
A heap that keeps growing after the history fills up would reveal a leak. Returns a list of
(label . bytes) conses; the drawing keeps the entities it had.

DRAWING: the drawing, modified temporarily."
  (let* ((history (dom:make-history))
         (prng (rng:make-prng (+ +benchmark-seed+ +soak-seed-offset+)))
         (bounds (site-bounds drawing))
         (side (max (geo:bounds-max-x bounds) (geo:bounds-max-y bounds)))
         (series (list (cons "start" (heap-after-full-gc-bytes)))))
    (loop for cycle from 1 to +soak-cycles+
          do (let* ((first-id (dom:drawing-next-entity-id drawing))
                    (entities (loop for offset below +soak-entities-per-cycle+
                                    collect (rng:random-entity prng
                                                               (rng:random-kind prng)
                                                               (+ first-id offset)
                                                               (dom:drawing-current-layer-id drawing)
                                                               side))))
               (dom:history-commit history
                                   drawing
                                   (dom:create-entities-changes entities))
               (dom:history-commit history
                                   drawing
                                   (dom:delete-entities-changes drawing (mapcar #'dom:entity-id entities)))
               (when (zerop (mod cycle +soak-checkpoint-interval+))
                 (push (cons (format nil "after ~d cycles" cycle) (heap-after-full-gc-bytes))
                       series))))
    (dom:history-clear history)
    (push (cons "history cleared" (heap-after-full-gc-bytes)) series)
    (nreverse series)))

;;; Instrumentation overhead

(defconstant +overhead-frames-per-round+ 150
  "Frames each instrumentation mode renders in one round.")

(defconstant +overhead-rounds+ 10
  "Interleaved rounds of the overhead measurement; interleaving cancels slow drift. A multiple
of the number of modes, so that every mode takes every position in the rotation equally often.")

(defconstant +overhead-entities+ 1000
  "Entities in the drawing used by the overhead measurement.")

(defparameter *instrumentation-modes* '(:off :off-control :error :info :debug :overlay)
  "Instrumentation compared by the overhead measurement: no telemetry at all, the very same
setup once more as a control (the difference between the two is the noise floor), telemetry
writing at each log level, and telemetry at the info level with the live overlay drawn on every
frame.")

(defun uninstrumented-mode-p (mode)
  "Return true when MODE runs without any telemetry.

MODE: one of *INSTRUMENTATION-MODES*."
  (member mode '(:off :off-control)))

(defun make-overhead-context (mode
                              directory)
  "Create a headless application whose instrumentation is MODE, with its own log directory.
Returns a list of the mode, the application, its telemetry (or NIL) and an empty sample set.

MODE: one of *INSTRUMENTATION-MODES*.
DIRECTORY: scratch directory below which the log directory of this mode is created."
  (let ((config (base:copy-config (base:default-config :project-root directory))))
    (setf (base:config-log-directory config) (merge-pathnames (format nil "~(~a~)/" mode) directory)
          (base:config-log-level config) (cond ((uninstrumented-mode-p mode) :error)
                                               ((eq mode :overlay) :info)
                                               (t mode)))
    (let* ((telemetry (unless (uninstrumented-mode-p mode)
                        (obs:make-telemetry config)))
           (app (app:make-app config
                              :telemetry telemetry
                              :drawing (rng:generate-drawing :seed +benchmark-seed+
                                                             :entity-count +overhead-entities+)
                              :width +canvas-width+
                              :height +canvas-height+)))
      (app:operation-zoom-extents app)
      (setf (app:app-overlay-visible app) (eq mode :overlay))
      (list mode app telemetry (obs:make-sample-set)))))

(defun drive-frame (app
                    telemetry
                    index)
  "Feed one frame of the shared benchmark input, render, and record the frame like the main
loop does.

APP: the application.
TELEMETRY: its telemetry, or NIL.
INDEX: frame number."
  (let ((started (sbcl:monotonic-nanoseconds)))
    (app:apply-benchmark-input app index)
    (app:app-render app)
    (when telemetry
      (let ((finished (sbcl:monotonic-nanoseconds)))
        (obs:telemetry-record-frame telemetry
                                    (- finished started)
                                    0
                                    finished)
        (obs:telemetry-tick telemetry finished)))))

(defun rotate-list (list
                    places)
  "Return LIST rotated left by PLACES positions (modulo its length).

LIST: any list.
PLACES: how many elements move from the front to the back."
  (let ((shift (mod places (max 1 (length list)))))
    (append (nthcdr shift list)
            (subseq list 0 shift))))

(defun measure-instrumentation-overhead (directory)
  "Render the same scripted frames with each instrumentation mode, interleaved in rounds, and
return one measurement per mode. The measurements cover event handling, scene and panel
construction, but no GPU work.

DIRECTORY: scratch directory for the logs written by the instrumented modes."
  (let ((contexts (mapcar (lambda (mode) (make-overhead-context mode directory))
                          *instrumentation-modes*))
        (frame 0))
    (dotimes (round +overhead-rounds+)
      ;; The order rotates every round so that no mode is always the one right after a GC.
      (dolist (context (rotate-list contexts round))
        (destructuring-bind (mode app telemetry samples) context
          (declare (ignore mode))
          (dotimes (step +overhead-frames-per-round+)
            (let ((started (sbcl:monotonic-nanoseconds)))
              (drive-frame app
                           telemetry
                           (+ frame step))
              (obs:sample-set-add samples
                                  (/ (- (sbcl:monotonic-nanoseconds) started)
                                     sbcl:+nanoseconds-per-microsecond+))))))
      (incf frame +overhead-frames-per-round+))
    (mapcar #'finish-overhead-context contexts)))

(defun finish-overhead-context (context)
  "Close the telemetry of an overhead context and return its measurement, noting how much the
instrumentation wrote, which proves that the mode did real work.

CONTEXT: a list of the mode, the application, the telemetry (or NIL) and the sample set."
  (destructuring-bind (mode app telemetry samples) context
    (declare (ignore app))
    (let* ((log-path (and telemetry (obs:logger-path (obs:telemetry-logger telemetry))))
           (measurement (progn
                          (when telemetry
                            (obs:close-telemetry telemetry))
                          (make-measurement (format nil "frame, instrumentation ~(~a~)" mode)
                                            :microseconds
                                            (copy-seq (obs:sample-set-samples samples))
                                            (obs:summarize-samples samples)))))
      (push (if (and log-path (probe-file log-path))
                (format nil "~:d log lines, ~:d bytes written"
                        (length (uiop:read-file-lines log-path))
                        (sbcl:file-size-bytes log-path))
                "no telemetry: nothing written")
            (measurement-notes measurement))
      measurement)))
