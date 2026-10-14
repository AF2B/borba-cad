(in-package #:borba-cad/scene)

(defstruct (theme (:constructor make-theme (&key canvas-background
                                                 grid-minor
                                                 grid-major
                                                 axis
                                                 selection
                                                 selection-fill
                                                 preview
                                                 snap-marker
                                                 panel-background
                                                 panel-border
                                                 text
                                                 text-dim
                                                 button
                                                 button-hover
                                                 button-active
                                                 accent
                                                 danger
                                                 field-background
                                                 overlay-background)))
  "Every color the application draws with.

CANVAS-BACKGROUND: drawing area.
GRID-MINOR: minor grid lines.
GRID-MAJOR: major grid lines.
AXIS: the x and y axes.
SELECTION: selected entities.
SELECTION-FILL: translucent window-selection fill.
PREVIEW: entity being created.
SNAP-MARKER: snap indicator.
PANEL-BACKGROUND: toolbar and side panels.
PANEL-BORDER: separators and outlines.
TEXT: primary text.
TEXT-DIM: secondary text.
BUTTON: idle button.
BUTTON-HOVER: button under the pointer.
BUTTON-ACTIVE: pressed or toggled-on button.
ACCENT: highlights.
DANGER: error messages.
FIELD-BACKGROUND: text fields.
OVERLAY-BACKGROUND: translucent background of the performance overlay."
  canvas-background grid-minor grid-major axis selection selection-fill preview snap-marker
  panel-background panel-border text text-dim button button-hover button-active accent danger
  field-background overlay-background)

(defparameter *dark-theme-channels*
  '((:canvas-background 0.075 0.082 0.098 1.0)
    (:grid-minor 1.0 1.0 1.0 0.055)
    (:grid-major 1.0 1.0 1.0 0.13)
    (:axis 0.45 0.62 0.95 0.55)
    (:selection 1.0 0.67 0.2 1.0)
    (:selection-fill 0.3 0.6 1.0 0.14)
    (:preview 0.35 0.85 1.0 0.95)
    (:snap-marker 0.55 1.0 0.45 1.0)
    (:panel-background 0.105 0.115 0.14 1.0)
    (:panel-border 0.22 0.24 0.29 1.0)
    (:text 0.9 0.92 0.95 1.0)
    (:text-dim 0.58 0.62 0.7 1.0)
    (:button 0.16 0.175 0.21 1.0)
    (:button-hover 0.22 0.245 0.3 1.0)
    (:button-active 0.2 0.42 0.78 1.0)
    (:accent 0.4 0.7 1.0 1.0)
    (:danger 1.0 0.45 0.4 1.0)
    (:field-background 0.06 0.066 0.08 1.0)
    (:overlay-background 0.02 0.025 0.035 0.82))
  "Red, green, blue and opacity, each in [0, 1], of every color of the dark theme, by the
keyword of the THEME slot that holds it.")

(defun default-theme ()
  "Return the dark theme used by the application."
  (apply #'make-theme
         (loop for (slot . channels) in *dark-theme-channels*
               append (list slot (apply #'make-rgba channels)))))
