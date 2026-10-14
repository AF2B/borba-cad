(in-package #:borba-cad/scene)

;;;; The draw list. Vertices are single-floats in logical screen pixels, eight per vertex:
;;;; x y u v r g b a. Commands replay the list in painter's order. Lines and triangles share one
;;;; vertex array; text runs are laid out by the renderer, which owns the font. The list is
;;;; reset and refilled every frame without allocating in the steady state.

(defconstant +floats-per-vertex+ 8
  "Single-floats per vertex: x y u v r g b a.")

(defconstant +initial-vertex-capacity+ 4096
  "Vertices the draw list can hold before its first growth.")

(defstruct (rgba (:constructor make-rgba (r
                                          g
                                          b
                                          a)))
  "A color with channels in [0, 1].

R: red.
G: green.
B: blue.
A: opacity."
  (r 1f0 :type single-float :read-only t)
  (g 1f0 :type single-float :read-only t)
  (b 1f0 :type single-float :read-only t)
  (a 1f0 :type single-float :read-only t))

(defconstant +byte-maximum+ 255f0
  "Largest value of an 8-bit color channel, as a single-float.")

(defun rgb->rgba (rgb
                  &optional (alpha 1f0))
  "Convert an 8-bit RGB color of the domain to a float color.

RGB: a domain rgb value.
ALPHA: opacity in [0, 1]."
  (make-rgba (/ (dom:rgb-red rgb) +byte-maximum+)
             (/ (dom:rgb-green rgb) +byte-maximum+)
             (/ (dom:rgb-blue rgb) +byte-maximum+)
             alpha))

(defstruct (draw-command (:constructor make-draw-command (kind
                                                          start
                                                          count)))
  "A run of primitives of one kind.

KIND: :lines (vertex pairs), :triangles (vertex triples) or :text (text runs).
START: first vertex, or first text run for :text.
COUNT: number of vertices, or of text runs for :text."
  (kind :lines :type keyword :read-only t)
  (start 0 :type fixnum :read-only t)
  (count 0 :type fixnum))

(defstruct (text-run (:constructor make-text-run (string
                                                  x
                                                  y
                                                  size
                                                  angle
                                                  color
                                                  font)))
  "A line of text to draw.

STRING: the characters.
X: x of the baseline start, in pixels.
Y: y of the baseline start, in pixels (down).
SIZE: em size in pixels.
ANGLE: counter-clockwise rotation of the baseline as seen on screen, in radians.
COLOR: an RGBA.
FONT: :mono for drawing text, one cell per code point on a 0.6 em grid; :ui or :ui-strong for
the proportional faces of the user interface, whose widths the renderer measures."
  (string "" :type string :read-only t)
  (x 0f0 :type single-float :read-only t)
  (y 0f0 :type single-float :read-only t)
  (size 12f0 :type single-float :read-only t)
  (angle 0f0 :type single-float :read-only t)
  (color nil :type (or null rgba) :read-only t)
  (font :mono :type keyword :read-only t))

(defstruct (draw-list (:constructor %make-draw-list))
  "Everything a renderer needs for one frame.

VERTICES: x y u v r g b a for every vertex; only the first VERTEX-COUNT are meaningful.
VERTEX-COUNT: vertices written so far.
COMMANDS: draw commands in painter's order.
TEXT-RUNS: text runs referenced by :text commands.
WHITE-U: u of a fully opaque white texel, set by the renderer's texture atlas.
WHITE-V: v of that texel."
  (vertices (make-array (* +initial-vertex-capacity+ +floats-per-vertex+) :element-type 'single-float)
   :type (simple-array single-float (*)))
  (vertex-count 0 :type fixnum)
  (commands (make-array 64 :adjustable t :fill-pointer 0) :type (and vector (not simple-array)))
  (text-runs (make-array 64 :adjustable t :fill-pointer 0) :type (and vector (not simple-array)))
  (white-u 0f0 :type single-float)
  (white-v 0f0 :type single-float))

(defun make-draw-list ()
  "Create an empty draw list."
  (%make-draw-list))

(defun draw-list-reset (list)
  "Empty LIST for the next frame, keeping its storage.

LIST: a draw list."
  (setf (draw-list-vertex-count list) 0
        (fill-pointer (draw-list-commands list)) 0
        (fill-pointer (draw-list-text-runs list)) 0)
  list)

(defun ensure-vertex-room (list
                           vertices)
  "Grow the vertex array so that VERTICES more vertices fit.

LIST: a draw list.
VERTICES: number of vertices about to be added."
  (let* ((needed (* (+ (draw-list-vertex-count list) vertices) +floats-per-vertex+))
         (array (draw-list-vertices list)))
    (when (> needed (length array))
      (let ((bigger (make-array (max needed (* 2 (length array))) :element-type 'single-float)))
        (replace bigger array :end2 (* (draw-list-vertex-count list) +floats-per-vertex+))
        (setf (draw-list-vertices list) bigger)))))

(defun note-primitives (list
                        kind
                        first-vertex
                        vertices)
  "Record VERTICES primitives of KIND starting at FIRST-VERTEX, merging with the previous
command when it has the same kind.

LIST: a draw list.
KIND: :lines or :triangles.
FIRST-VERTEX: index of the first vertex just written.
VERTICES: number of vertices just written."
  (let* ((commands (draw-list-commands list))
         (last (and (plusp (length commands)) (aref commands (1- (length commands))))))
    (if (and last (eq (draw-command-kind last) kind)
             (= (+ (draw-command-start last) (draw-command-count last)) first-vertex))
        (incf (draw-command-count last) vertices)
        (vector-push-extend (make-draw-command kind
                                               first-vertex
                                               vertices) commands))))

(declaim (inline write-vertex))
(defun write-vertex (list
                     x
                     y
                     color)
  "Append one vertex using the white texel.

LIST: a draw list with room for one more vertex.
X: x in pixels.
Y: y in pixels.
COLOR: an RGBA."
  (declare (type single-float x y)
           (type rgba color))
  (let ((array (draw-list-vertices list))
        (base (* (draw-list-vertex-count list) +floats-per-vertex+)))
    (setf (aref array base) x
          (aref array (+ base 1)) y
          (aref array (+ base 2)) (draw-list-white-u list)
          (aref array (+ base 3)) (draw-list-white-v list)
          (aref array (+ base 4)) (rgba-r color)
          (aref array (+ base 5)) (rgba-g color)
          (aref array (+ base 6)) (rgba-b color)
          (aref array (+ base 7)) (rgba-a color))
    (incf (draw-list-vertex-count list))))

(defun add-line (list
                 x1
                 y1
                 x2
                 y2
                 color)
  "Append one line segment, in pixels.

LIST: a draw list.
X1: start x.
Y1: start y.
X2: end x.
Y2: end y.
COLOR: an RGBA."
  (declare (type single-float x1 y1 x2 y2))
  (ensure-vertex-room list 2)
  (let ((first (draw-list-vertex-count list)))
    (write-vertex list
                  x1
                  y1
                  color)
    (write-vertex list
                  x2
                  y2
                  color)
    (note-primitives list
                     :lines
                     first
                     2)))

(defun add-triangle (list
                     x1
                     y1
                     x2
                     y2
                     x3
                     y3
                     color)
  "Append one filled triangle, in pixels.

LIST: a draw list.
X1: first corner x.
Y1: first corner y.
X2: second corner x.
Y2: second corner y.
X3: third corner x.
Y3: third corner y.
COLOR: an RGBA."
  (declare (type single-float x1 y1 x2 y2 x3 y3))
  (ensure-vertex-room list 3)
  (let ((first (draw-list-vertex-count list)))
    (write-vertex list
                  x1
                  y1
                  color)
    (write-vertex list
                  x2
                  y2
                  color)
    (write-vertex list
                  x3
                  y3
                  color)
    (note-primitives list
                     :triangles
                     first
                     3)))

(defun add-filled-rectangle (list
                             x
                             y
                             width
                             height
                             color)
  "Append a filled rectangle as two triangles, in pixels.

LIST: a draw list.
X: left edge.
Y: top edge.
WIDTH: width.
HEIGHT: height.
COLOR: an RGBA."
  (declare (type single-float x y width height))
  (let ((right (+ x width))
        (bottom (+ y height)))
    (add-triangle list
                  x
                  y
                  right
                  y
                  right
                  bottom
                  color)
    (add-triangle list
                  x
                  y
                  right
                  bottom
                  x
                  bottom
                  color)))

(defun add-rectangle-outline (list
                              x
                              y
                              width
                              height
                              color)
  "Append the four edges of a rectangle as lines, in pixels.

LIST: a draw list.
X: left edge.
Y: top edge.
WIDTH: width.
HEIGHT: height.
COLOR: an RGBA."
  (declare (type single-float x y width height))
  (let ((right (+ x width))
        (bottom (+ y height)))
    (add-line list
              x
              y
              right
              y
              color)
    (add-line list
              right
              y
              right
              bottom
              color)
    (add-line list
              right
              bottom
              x
              bottom
              color)
    (add-line list
              x
              bottom
              x
              y
              color)))

(defun add-text (list
                 string
                 x
                 y
                 size
                 color
                 &key (angle 0f0)
                      (font :mono))
  "Append a text run.

LIST: a draw list.
STRING: the characters to draw.
X: x of the baseline start, in pixels.
Y: y of the baseline start, in pixels (down).
SIZE: em size in pixels.
COLOR: an RGBA.
ANGLE: counter-clockwise rotation in radians.
FONT: :mono, :ui or :ui-strong."
  (let* ((runs (draw-list-text-runs list))
         (commands (draw-list-commands list))
         (last (and (plusp (length commands)) (aref commands (1- (length commands))))))
    (vector-push-extend (make-text-run string
                                       x
                                       y
                                       size
                                       angle
                                       color
                                       font) runs)
    (if (and last (eq (draw-command-kind last) :text)
             (= (+ (draw-command-start last) (draw-command-count last)) (1- (length runs))))
        (incf (draw-command-count last))
        (vector-push-extend (make-draw-command :text
                                               (1- (length runs))
                                               1) commands))))

(defun draw-list-line-count (list)
  "Return the number of line segments in LIST.

LIST: a draw list."
  (loop for command across (draw-list-commands list)
        when (eq (draw-command-kind command) :lines)
          sum (floor (draw-command-count command) 2)))

(defun draw-list-triangle-count (list)
  "Return the number of triangles in LIST.

LIST: a draw list."
  (loop for command across (draw-list-commands list)
        when (eq (draw-command-kind command) :triangles)
          sum (floor (draw-command-count command) 3)))
