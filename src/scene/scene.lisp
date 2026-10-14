(in-package #:borba-cad/scene)

;;;; The scene builder. It walks the drawing in painter's order and emits screen-space lines,
;;;; triangles and text runs. Everything is culled against the canvas, segments are clipped in
;;;; double precision before conversion to single-float, and circles are tessellated by their
;;;; on-screen size.

(defconstant +circle-chordal-error-pixels+ 0.25d0
  "Largest distance, in pixels, between a tessellated circle and the true circle.")

(defconstant +circle-minimum-segments+ 12
  "Fewest segments used for a circle, however small on screen.")

(defconstant +circle-maximum-segments+ 4096
  "Most segments used for a circle; keeps the error under 0.3 px up to radius ~1e6 px.")

(defconstant +minimum-text-pixels+ 3d0
  "Text smaller than this many pixels is drawn as a baseline stroke instead of glyphs.")

(defconstant +culling-margin-pixels+ 4d0
  "Border around the canvas inside which geometry is still emitted, so lines do not end
visibly at the edge.")

(defconstant +snap-marker-half-size-pixels+ 5f0
  "Half the size of the snap marker, in pixels.")

(defconstant +snap-circle-segments+ 16
  "Segments of the circular snap marker.")

(defconstant +arrow-minimum-pixels+ 2d0
  "Arrowheads shorter than this many pixels are not drawn.")

(defstruct (scene-state (:constructor make-scene-state (&key selected-ids
                                                             translation
                                                             preview-entities
                                                             window
                                                             window-crossing
                                                             snap)))
  "The interaction state that decorates the drawing.

SELECTED-IDS: hash table of selected entity ids, or NIL.
TRANSLATION: (dx . dy) in millimetres applied to selected entities while dragging, or NIL.
PREVIEW-ENTITIES: entities being created, drawn in the preview color.
WINDOW: world bounds of a window-selection rectangle, or NIL.
WINDOW-CROSSING: true when that rectangle is a crossing selection, drawn dashed and in another color.
SNAP: (x y kind) in world coordinates of the snap marker, or NIL."
  selected-ids translation preview-entities window window-crossing snap)

(defstruct (scene-result (:constructor make-scene-result (drawn-entities
                                                          culled-entities)))
  "What building a scene did.

DRAWN-ENTITIES: entities that produced geometry.
CULLED-ENTITIES: entities skipped because they are off screen or on a hidden layer."
  (drawn-entities 0 :type fixnum)
  (culled-entities 0 :type fixnum))

(defstruct (frame (:constructor make-frame (list
                                            viewport
                                            clip
                                            theme)))
  "Per-build context shared by the emit functions.

LIST: the draw list being filled.
VIEWPORT: the viewport.
CLIP: screen-space bounds inside which geometry is kept.
THEME: the theme.
OFFSET-X: world x added to every point (move preview).
OFFSET-Y: world y added to every point."
  list
  viewport
  (clip nil :type geo:bounds)
  theme
  (offset-x 0d0 :type double-float)
  (offset-y 0d0 :type double-float))

(declaim (inline to-single))
(defun to-single (value)
  "Convert a pixel coordinate to single-float.

VALUE: a double-float within the canvas margin."
  (declare (type double-float value))
  (coerce value 'single-float))

(defun circle-segment-count (radius-pixels)
  "Return how many segments tessellate a circle of RADIUS-PIXELS so the chordal error stays
under +CIRCLE-CHORDAL-ERROR-PIXELS+, between the minimum and the maximum.

RADIUS-PIXELS: on-screen radius in pixels."
  (declare (type double-float radius-pixels))
  (if (<= radius-pixels +circle-chordal-error-pixels+)
      +circle-minimum-segments+
      (let ((count (ceiling (float pi 1d0)
                            (acos (- 1d0 (/ +circle-chordal-error-pixels+ radius-pixels))))))
        (max +circle-minimum-segments+ (min +circle-maximum-segments+ count)))))

(defun emit-world-segment (frame
                           wx1
                           wy1
                           wx2
                           wy2
                           color)
  "Transform a world segment to the screen, clip it and append it.

FRAME: the build context.
WX1: start x in world millimetres.
WY1: start y.
WX2: end x.
WY2: end y.
COLOR: an RGBA."
  (declare (type double-float wx1 wy1 wx2 wy2))
  (let ((viewport (frame-viewport frame))
        (ox (frame-offset-x frame))
        (oy (frame-offset-y frame)))
    (multiple-value-bind (sx1 sy1)
        (view:world->screen viewport
                            (+ wx1 ox)
                            (+ wy1 oy))
      (multiple-value-bind (sx2 sy2)
          (view:world->screen viewport
                              (+ wx2 ox)
                              (+ wy2 oy))
        (multiple-value-bind (cx1 cy1 cx2 cy2)
            (geo:clip-segment-to-bounds sx1
                                        sy1
                                        sx2
                                        sy2
                                        (frame-clip frame))
          (when cx1
            (add-line (frame-list frame)
                      (to-single cx1)
                      (to-single cy1)
                      (to-single cx2)
                      (to-single cy2)
                      color)))))))

(defun emit-dash (frame
                  bounds
                  color)
  "Draw a one-pixel dash at the center of BOUNDS: the stand-in for an entity too small to see.

FRAME: the build context.
BOUNDS: world bounds of the entity.
COLOR: an RGBA."
  (multiple-value-bind (sx sy)
      (view:world->screen (frame-viewport frame)
                          (+ (geo:bounds-center-x bounds) (frame-offset-x frame))
                          (+ (geo:bounds-center-y bounds) (frame-offset-y frame)))
    (let ((x (to-single sx))
          (y (to-single sy)))
      (add-line (frame-list frame)
                x
                y
                (+ x 1f0)
                y
                color))))

(defun emit-polyline-path (frame
                           coordinates
                           closed
                           color)
  "Emit the segments of a flat x/y coordinate array.

FRAME: the build context.
COORDINATES: simple double-float array x0 y0 x1 y1 ...
CLOSED: when true, also emit the closing segment.
COLOR: an RGBA."
  (declare (type (simple-array double-float (*)) coordinates))
  (let ((count (geo:polyline-vertex-count coordinates)))
    (loop for index from 0 below (1- count)
          do (emit-world-segment frame
                                 (aref coordinates (* 2 index))
                                 (aref coordinates (+ (* 2 index) 1))
                                 (aref coordinates (+ (* 2 index) 2))
                                 (aref coordinates (+ (* 2 index) 3))
                                 color))
    (when (and closed (> count 2))
      (emit-world-segment frame
                          (aref coordinates (- (length coordinates) 2))
                          (aref coordinates (- (length coordinates) 1))
                          (aref coordinates 0)
                          (aref coordinates 1)
                          color))))

(defun emit-circle (frame
                    center-x
                    center-y
                    radius
                    color)
  "Emit a circle as a closed polygon of segments chosen from its on-screen radius.

FRAME: the build context.
CENTER-X: world x of the center.
CENTER-Y: world y of the center.
RADIUS: world radius in millimetres.
COLOR: an RGBA."
  (declare (type double-float center-x center-y radius))
  (let* ((viewport (frame-viewport frame))
         (radius-pixels (view:world-length->pixels viewport radius))
         (segments (circle-segment-count radius-pixels))
         (step (/ (* 2d0 (float pi 1d0)) segments))
         (cosine (cos step))
         (sine (sin step)))
    (declare (type double-float radius-pixels step cosine sine) (type fixnum segments))
    (multiple-value-bind (center-sx center-sy)
        (view:world->screen viewport
                            (+ center-x (frame-offset-x frame))
                            (+ center-y (frame-offset-y frame)))
      ;; Rotate a vector around the center by one step per segment: four multiplications
      ;; instead of two trigonometric calls per vertex.
      (let ((dx radius-pixels)
            (dy 0d0)
            (previous-x (+ center-sx radius-pixels))
            (previous-y center-sy))
        (declare (type double-float dx dy previous-x previous-y))
        (dotimes (segment segments)
          (let* ((next-dx (- (* dx cosine) (* dy sine)))
                 (next-dy (+ (* dx sine) (* dy cosine)))
                 (next-x (+ center-sx next-dx))
                 (next-y (- center-sy next-dy)))
            (declare (type double-float next-dx next-dy next-x next-y))
            (multiple-value-bind (cx1 cy1 cx2 cy2)
                (geo:clip-segment-to-bounds previous-x
                                            previous-y
                                            next-x
                                            next-y
                                            (frame-clip frame))
              (when cx1
                (add-line (frame-list frame)
                          (to-single cx1)
                          (to-single cy1)
                          (to-single cx2)
                          (to-single cy2)
                          color)))
            (setf dx next-dx
                  dy next-dy
                  previous-x next-x
                  previous-y next-y)))))))

(defun emit-arc (frame
                 center-x
                 center-y
                 radius
                 start-angle
                 sweep
                 color)
  "Emit an arc as a run of segments chosen from its on-screen radius and its sweep.

FRAME: the build context.
CENTER-X: world x of the center.
CENTER-Y: world y of the center.
RADIUS: world radius in millimetres.
START-ANGLE: direction of the start in radians.
SWEEP: how far it turns counter-clockwise, in radians.
COLOR: an RGBA."
  (declare (type double-float center-x center-y radius start-angle sweep))
  (let* ((radius-pixels (view:world-length->pixels (frame-viewport frame) radius))
         (full-circle (circle-segment-count radius-pixels))
         (segments (max 1 (ceiling (* full-circle sweep) (* 2d0 (float pi 1d0)))))
         (step (/ sweep segments))
         (previous-x (+ center-x (* radius (cos start-angle))))
         (previous-y (+ center-y (* radius (sin start-angle)))))
    (declare (type double-float radius-pixels step previous-x previous-y) (type fixnum full-circle segments))
    (loop for index from 1 to segments
          do (let* ((angle (+ start-angle (* step index)))
                    (x (+ center-x (* radius (cos angle))))
                    (y (+ center-y (* radius (sin angle)))))
               (declare (type double-float angle x y))
               (emit-world-segment frame
                                   previous-x
                                   previous-y
                                   x
                                   y
                                   color)
               (setf previous-x x
                     previous-y y)))))

(defun emit-ellipse (frame
                     ellipse
                     color)
  "Emit an ellipse as a closed run of segments chosen from the on-screen size of its longer axis.

FRAME: the build context.
ELLIPSE: an ellipse entity.
COLOR: an RGBA."
  (let* ((radius-x (dom:ellipse-radius-x ellipse))
         (radius-y (dom:ellipse-radius-y ellipse))
         (segments (circle-segment-count (view:world-length->pixels (frame-viewport frame)
                                                                    (max radius-x radius-y))))
         (points (geo:ellipse-points (dom:ellipse-center-x ellipse)
                                     (dom:ellipse-center-y ellipse)
                                     radius-x
                                     radius-y
                                     (dom:ellipse-rotation ellipse)
                                     (max 3 segments))))
    (emit-polyline-path frame
                        points
                        t
                        color)))

(defun emit-text-run (frame
                      string
                      x
                      y
                      height
                      angle
                      color)
  "Emit text anchored at a world position; text too small to read becomes a baseline stroke.

FRAME: the build context.
STRING: the characters.
X: world x of the baseline start.
Y: world y of the baseline start.
HEIGHT: text height in millimetres.
ANGLE: counter-clockwise direction in radians.
COLOR: an RGBA."
  (declare (type double-float x y height angle))
  (let* ((viewport (frame-viewport frame))
         (size (view:world-length->pixels viewport height)))
    (if (< size +minimum-text-pixels+)
        (let ((length (geo:text-width (length string) height)))
          (emit-world-segment frame
                              x
                              y
                              (+ x (* length (cos angle)))
                              (+ y (* length (sin angle)))
                              color))
        (multiple-value-bind (sx sy)
            (view:world->screen viewport
                                (+ x (frame-offset-x frame))
                                (+ y (frame-offset-y frame)))
          (add-text (frame-list frame)
                    string
                    (to-single sx)
                    (to-single sy)
                    (to-single size)
                    color
                    :angle (to-single angle))))))

(defun emit-dimension (frame
                       dimension
                       color)
  "Emit the extension lines, dimension line, arrowheads and text of a dimension.

FRAME: the build context.
DIMENSION: a dimension entity.
COLOR: an RGBA."
  (let* ((layout (dom:dimension-layout dimension))
         (extensions (geo:dimension-layout-extension-lines layout))
         (line (geo:dimension-layout-dimension-line layout))
         (arrows (geo:dimension-layout-arrowheads layout))
         (viewport (frame-viewport frame))
         (arrow-pixels (view:world-length->pixels viewport
                                                  (* geo:+dimension-arrow-length-ratio+
                                                     (dom:dimension-text-height dimension)))))
    (emit-world-segment frame
                        (aref extensions 0)
                        (aref extensions 1)
                        (aref extensions 2)
                        (aref extensions 3)
                        color)
    (emit-world-segment frame
                        (aref extensions 4)
                        (aref extensions 5)
                        (aref extensions 6)
                        (aref extensions 7)
                        color)
    (emit-world-segment frame
                        (aref line 0)
                        (aref line 1)
                        (aref line 2)
                        (aref line 3)
                        color)
    (when (>= arrow-pixels +arrow-minimum-pixels+)
      (loop for base in '(0 6)
            do (flet ((corner (index)
                        (multiple-value-bind (sx sy)
                            (view:world->screen viewport
                                                (+ (aref arrows (+ base (* 2 index))) (frame-offset-x frame))
                                                (+ (aref arrows (+ base (* 2 index) 1)) (frame-offset-y frame)))
                          (cons (to-single sx) (to-single sy)))))
                 (let ((a (corner 0)) (b (corner 1)) (c (corner 2)))
                   (add-triangle (frame-list frame)
                                 (car a)
                                 (cdr a)
                                 (car b)
                                 (cdr b)
                                 (car c)
                                 (cdr c)
                                 color)))))
    (emit-text-run frame
                   (dom:dimension-text dimension)
                   (geo:dimension-layout-text-x layout)
                   (geo:dimension-layout-text-y layout)
                   (dom:dimension-text-height dimension)
                   (geo:dimension-layout-text-angle layout)
                   color)))

(defun emit-entity (frame
                    entity
                    color)
  "Emit the geometry of one entity.

FRAME: the build context.
ENTITY: any entity.
COLOR: an RGBA."
  (etypecase entity
    (dom:line-entity
     (emit-world-segment frame
                         (dom:line-x1 entity)
                         (dom:line-y1 entity)
                         (dom:line-x2 entity)
                         (dom:line-y2 entity)
                         color))
    (dom:rectangle-entity
     (let ((bounds (dom:entity-bounds entity)))
       (let ((min-x (geo:bounds-min-x bounds)) (min-y (geo:bounds-min-y bounds))
             (max-x (geo:bounds-max-x bounds)) (max-y (geo:bounds-max-y bounds)))
         (emit-world-segment frame
                             min-x
                             min-y
                             max-x
                             min-y
                             color)
         (emit-world-segment frame
                             max-x
                             min-y
                             max-x
                             max-y
                             color)
         (emit-world-segment frame
                             max-x
                             max-y
                             min-x
                             max-y
                             color)
         (emit-world-segment frame
                             min-x
                             max-y
                             min-x
                             min-y
                             color))))
    (dom:circle-entity
     (emit-circle frame
                  (dom:circle-center-x entity)
                  (dom:circle-center-y entity)
                  (dom:circle-radius entity)
                  color))
    (dom:polyline-entity
     (emit-polyline-path frame
                         (dom:polyline-coordinates entity)
                         (dom:polyline-closed entity)
                         color))
    (dom:text-entity
     (emit-text-run frame
                    (dom:text-string entity)
                    (dom:text-x entity)
                    (dom:text-y entity)
                    (dom:text-height entity)
                    (dom:text-angle entity)
                    color))
    (dom:dimension-entity
     (emit-dimension frame
                     entity
                     color))
    (dom:arc-entity
     (emit-arc frame
               (dom:arc-center-x entity)
               (dom:arc-center-y entity)
               (dom:arc-radius entity)
               (dom:arc-start-angle entity)
               (dom:arc-sweep entity)
               color))
    (dom:ellipse-entity
     (emit-ellipse frame
                   entity
                   color))
    (dom:polygon-entity
     (emit-polyline-path frame
                         (dom:polygon-vertices entity)
                         t
                         color))))

(defun tiny-on-screen-p (viewport
                         bounds)
  "Return true when BOUNDS covers less than one pixel in both directions.

VIEWPORT: the viewport.
BOUNDS: world bounds."
  (and (< (view:world-length->pixels viewport (geo:bounds-width bounds)) 1d0)
       (< (view:world-length->pixels viewport (geo:bounds-height bounds)) 1d0)))

(defun emit-or-dash (frame
                     entity
                     color)
  "Emit ENTITY, or a one-pixel dash when it is too small to see.

FRAME: the build context.
ENTITY: any entity.
COLOR: an RGBA."
  (if (tiny-on-screen-p (frame-viewport frame) (dom:entity-bounds entity))
      (emit-dash frame
                 (dom:entity-bounds entity)
                 color)
      (emit-entity frame
                   entity
                   color)))

(defun layer-styles (drawing)
  "Return a hash table from layer id to (visible . RGBA), computed once per frame.

DRAWING: the drawing."
  (let ((table (make-hash-table)))
    (dolist (layer (dom:layers-in-order drawing) table)
      (setf (gethash (dom:layer-id layer) table)
            (cons (dom:layer-visible layer) (rgb->rgba (dom:layer-color layer)))))))

(defun add-drawing-entities (frame
                             drawing
                             state)
  "Emit every visible entity: unselected ones in layer colors, then selected ones in the
selection color (shifted by the move preview), then the preview entities.

FRAME: the build context.
DRAWING: the drawing.
STATE: the scene state."
  (let* ((viewport (frame-viewport frame))
         (visible (view:viewport-visible-bounds viewport +culling-margin-pixels+))
         (styles (layer-styles drawing))
         (selected (scene-state-selected-ids state))
         (theme (frame-theme frame))
         (drawn 0)
         (culled 0))
    (declare (type fixnum drawn culled))
    (setf (frame-offset-x frame) 0d0
          (frame-offset-y frame) 0d0)
    (dom:map-entities
     (lambda (entity)
       (let ((style (gethash (dom:entity-layer-id entity) styles)))
         (cond ((or (null style)
                    (not (car style))
                    (not (geo:bounds-intersect-p (dom:entity-bounds entity) visible)))
                (incf culled))
               ((and selected (gethash (dom:entity-id entity) selected)))
               (t (emit-or-dash frame
                                entity
                                (cdr style))
                  (incf drawn)))))
     (dom:drawing-store drawing))
    (when (and selected (plusp (hash-table-count selected)))
      (let* ((translation (scene-state-translation state))
             (dx (if translation (float (car translation) 1d0) 0d0))
             (dy (if translation (float (cdr translation) 1d0) 0d0))
             (shifted (geo:make-bounds (- (geo:bounds-min-x visible) dx)
                                       (- (geo:bounds-min-y visible) dy)
                                       (- (geo:bounds-max-x visible) dx)
                                       (- (geo:bounds-max-y visible) dy))))
        (setf (frame-offset-x frame) dx
              (frame-offset-y frame) dy)
        (loop for id being the hash-keys of selected
              do (let* ((entity (dom:store-find (dom:drawing-store drawing) id))
                        (style (and entity (gethash (dom:entity-layer-id entity) styles))))
                   (when (and style (car style)
                              (geo:bounds-intersect-p (dom:entity-bounds entity) shifted))
                     (emit-or-dash frame
                                   entity
                                   (theme-selection theme))
                     (incf drawn))))))
    (setf (frame-offset-x frame) 0d0
          (frame-offset-y frame) 0d0)
    (dolist (entity (scene-state-preview-entities state))
      (emit-entity frame
                   entity
                   (theme-preview theme)))
    (make-scene-result drawn culled)))

(defun add-grid (list
                 drawing
                 viewport
                 theme)
  "Emit the adaptive grid and the axes.

LIST: the draw list.
DRAWING: the drawing, which supplies the base spacing and visibility.
VIEWPORT: the viewport.
THEME: the theme."
  (when (dom:drawing-grid-visible drawing)
    (let* ((level (view:compute-grid-level viewport (dom:drawing-grid-spacing drawing)))
           (spacing (view:grid-level-spacing level))
           (major (view:grid-level-major-interval level))
           (left (to-single (view:viewport-origin-x viewport)))
           (top (to-single (view:viewport-origin-y viewport)))
           (right (+ left (to-single (view:viewport-width viewport))))
           (bottom (+ top (to-single (view:viewport-height viewport)))))
      (multiple-value-bind (first-x last-x first-y last-y)
          (view:visible-grid-range viewport spacing)
        (flet ((color-for (index
                           axis-color)
                 (cond ((zerop index) axis-color)
                       ((zerop (mod index major)) (theme-grid-major theme))
                       (t (theme-grid-minor theme)))))
          ;; The vertical line through x = 0 is the y axis, the horizontal one the x axis.
          (loop for index from first-x to last-x
                do (let ((x (to-single (view:world->screen viewport
                                                           (* index spacing)
                                                           0d0))))
                     (add-line list
                               x
                               top
                               x
                               bottom
                               (color-for index (theme-axis-y theme)))))
          (loop for index from first-y to last-y
                do (let ((y (nth-value 1 (view:world->screen viewport
                                                             0d0
                                                             (* index spacing)))))
                     (add-line list
                               left
                               (to-single y)
                               right
                               (to-single y)
                               (color-for index (theme-axis-x theme))))))))))

(defun add-snap-marker (list
                        viewport
                        snap
                        theme)
  "Emit the snap indicator: a square for endpoints, a circle for centers, a cross for the grid.

LIST: the draw list.
VIEWPORT: the viewport.
SNAP: (x y kind) in world coordinates, or NIL.
THEME: the theme."
  (when snap
    (destructuring-bind (world-x world-y kind) snap
      (multiple-value-bind (sx sy) (view:world->screen viewport
                                                       world-x
                                                       world-y)
        (let ((half (float +snap-marker-half-size-pixels+ 1d0))
              (color (theme-snap-marker theme)))
          (flet ((line (x1 y1 x2 y2)
                   (add-line list
                             (to-single x1)
                             (to-single y1)
                             (to-single x2)
                             (to-single y2)
                             color)))
            (ecase kind
              (:endpoint
               (line (- sx half) (- sy half) (+ sx half) (- sy half))
               (line (+ sx half) (- sy half) (+ sx half) (+ sy half))
               (line (+ sx half) (+ sy half) (- sx half) (+ sy half))
               (line (- sx half) (+ sy half) (- sx half) (- sy half)))
              (:center
               (let ((previous-x (+ sx half))
                     (previous-y sy))
                 (loop for step from 1 to +snap-circle-segments+
                       do (let* ((angle (* step (/ (* 2 (float pi 1d0)) +snap-circle-segments+)))
                                 (next-x (+ sx (* half (cos angle))))
                                 (next-y (+ sy (* half (sin angle)))))
                            (line previous-x previous-y next-x next-y)
                            (setf previous-x next-x
                                  previous-y next-y)))))
              (:grid
               (line (- sx half) sy (+ sx half) sy)
               (line sx (- sy half) sx (+ sy half))))))))))

(defconstant +crossing-dash-pixels+ 6f0
  "Length of a dash of the outline of a crossing selection, in pixels.")

(defconstant +crossing-gap-pixels+ 4f0
  "Length of a gap of the outline of a crossing selection, in pixels.")

(defconstant +crossing-outline-opacity+ 0.95f0
  "Opacity of the outline of a crossing selection.")

(defun add-selection-window (list
                             viewport
                             window
                             crossing
                             theme)
  "Emit the selection rectangle: a window, which takes what lies inside it, is a translucent
fill with a solid outline; a crossing, which takes what it touches, a translucent fill of another
color with a dashed outline.

LIST: the draw list.
VIEWPORT: the viewport.
WINDOW: world bounds, or NIL.
CROSSING: true for a crossing selection.
THEME: the theme."
  (when window
    (multiple-value-bind (left top)
        (view:world->screen viewport
                            (geo:bounds-min-x window)
                            (geo:bounds-max-y window))
      (multiple-value-bind (right bottom)
          (view:world->screen viewport
                              (geo:bounds-max-x window)
                              (geo:bounds-min-y window))
        (let ((x (to-single left)) (y (to-single top))
              (width (to-single (- right left))) (height (to-single (- bottom top))))
          (add-filled-rectangle list
                                x
                                y
                                width
                                height
                                (if crossing
                                    (theme-crossing-fill theme)
                                    (theme-selection-fill theme)))
          (if crossing
              (let* ((fill (theme-crossing-fill theme))
                     (color (make-rgba (rgba-r fill)
                                       (rgba-g fill)
                                       (rgba-b fill)
                                       +crossing-outline-opacity+)))
                (flet ((side (x1 y1 x2 y2)
                         (add-dashed-line list
                                          x1
                                          y1
                                          x2
                                          y2
                                          +crossing-dash-pixels+
                                          +crossing-gap-pixels+
                                          color)))
                  (side x y (+ x width) y)
                  (side (+ x width) y (+ x width) (+ y height))
                  (side (+ x width) (+ y height) x (+ y height))
                  (side x (+ y height) x y)))
              (add-rectangle-outline list
                                     x
                                     y
                                     width
                                     height
                                     (theme-selection theme))))))))

(defun build-scene (list
                    drawing
                    viewport
                    state
                    theme)
  "Fill LIST (already reset) with the canvas content: grid, entities, selection window and
snap marker. Returns a SCENE-RESULT.

LIST: the draw list.
DRAWING: the drawing.
VIEWPORT: the viewport.
STATE: a scene state.
THEME: the theme."
  (let* ((clip (geo:make-bounds (- (view:viewport-origin-x viewport) +culling-margin-pixels+)
                                (- (view:viewport-origin-y viewport) +culling-margin-pixels+)
                                (+ (view:viewport-origin-x viewport) (view:viewport-width viewport)
                                   +culling-margin-pixels+)
                                (+ (view:viewport-origin-y viewport) (view:viewport-height viewport)
                                   +culling-margin-pixels+)))
         (frame (make-frame list
                            viewport
                            clip
                            theme)))
    (add-grid list
              drawing
              viewport
              theme)
    (let ((result (add-drawing-entities frame
                                        drawing
                                        state)))
      (add-selection-window list
                            viewport
                            (scene-state-window state)
                            (scene-state-window-crossing state)
                            theme)
      (add-snap-marker list
                       viewport
                       (scene-state-snap state)
                       theme)
      result)))
