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
