(in-package #:borba-cad/domain)

(defstruct (rgb (:constructor make-rgb (red
                                        green
                                        blue)))
  "An 8-bit-per-channel color.

RED: red channel, 0 to 255.
GREEN: green channel, 0 to 255.
BLUE: blue channel, 0 to 255."
  (red 0 :type (unsigned-byte 8) :read-only t)
  (green 0 :type (unsigned-byte 8) :read-only t)
  (blue 0 :type (unsigned-byte 8) :read-only t))

(defconstant +default-layer-id+ 0
  "Id of the layer every drawing starts with; it cannot be deleted.")

(defconstant +default-layer-gray+ 235
  "Gray level of the default layer color: near-white, readable on the dark canvas.")

(defun default-layer-name ()
  "Return the name of the default layer."
  "0")

(defun default-layer-color ()
  "Return the color of the default layer."
  (make-rgb +default-layer-gray+
            +default-layer-gray+
            +default-layer-gray+))

(defparameter *layer-palette-channels*
  '((235 235 235)
    (255 120 80)
    (120 200 255)
    (160 220 120)
    (255 220 100)
    (200 160 255)
    (255 150 200)
    (100 220 200))
  "Red, green and blue (0 to 255) of the colors offered to layers: new layers take the next one and the
color button cycles through them.")

(defparameter *layer-palette*
  (mapcar (lambda (channels)
            (apply #'make-rgb channels))
          *layer-palette-channels*)
  "The palette as RGB values.")

(defun palette-color-after (color)
  "Return the palette color that follows COLOR, or the first when COLOR is not in the palette.

COLOR: an RGB color."
  (let ((position (position color *layer-palette* :test #'equalp)))
    (nth (if position
             (mod (1+ position) (length *layer-palette*))
             0)
         *layer-palette*)))

(defun palette-color-at (index)
  "Return the palette color at INDEX, counting round the palette: the color a new layer is given.

INDEX: a whole number, such as the id the next layer will have."
  (nth (mod index (length *layer-palette*)) *layer-palette*))

(defstruct (layer (:constructor %make-layer (id
                                             name
                                             color
                                             visible
                                             locked))
                  (:copier nil))
  "A drawing layer. Layers are immutable; the drawing replaces them when they change.

ID: unique, never reused within a drawing.
NAME: non-empty, unique within the drawing.
COLOR: RGB color applied to every entity on the layer.
VISIBLE: when false the layer is neither drawn nor pickable.
LOCKED: when true the layer's entities are drawn but cannot be picked or edited."
  (id 0 :type (integer 0) :read-only t)
  (name "" :type string :read-only t)
  (color (default-layer-color) :type rgb :read-only t)
  (visible t :type boolean :read-only t)
  (locked nil :type boolean :read-only t))
