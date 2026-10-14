(in-package #:borba-cad/scene)

(defstruct (theme (:constructor make-theme (&key canvas-background
                                                 grid-minor
                                                 grid-major
                                                 axis-x
                                                 axis-y
                                                 selection
                                                 selection-fill
                                                 crossing-fill
                                                 preview
                                                 snap-marker
                                                 panel-background
                                                 panel-border
                                                 title-bar-top
                                                 title-bar-bottom
                                                 text
                                                 text-dim
                                                 text-faint
                                                 button
                                                 button-hover
                                                 button-active
                                                 accent
                                                 accent-soft
                                                 accent-contrast
                                                 danger
                                                 warning
                                                 field-background
                                                 overlay-background
                                                 shadow
                                                 scrim)))
  "Every color the application draws with.

CANVAS-BACKGROUND: drawing area.
GRID-MINOR: minor grid lines.
GRID-MAJOR: major grid lines.
AXIS-X: the x axis, the horizontal line through the origin.
AXIS-Y: the y axis, the vertical line through the origin.
SELECTION: selected entities.
SELECTION-FILL: translucent fill of a window selection, which takes what lies inside.
CROSSING-FILL: translucent fill of a crossing selection, which takes what it touches.
PREVIEW: entity being created.
SNAP-MARKER: snap indicator.
PANEL-BACKGROUND: panels and cards.
PANEL-BORDER: separators and outlines.
TITLE-BAR-TOP: top of the title bar's shading.
TITLE-BAR-BOTTOM: bottom of the title bar's shading.
TEXT: primary text.
TEXT-DIM: secondary text.
TEXT-FAINT: tertiary text and disabled items.
BUTTON: idle button.
BUTTON-HOVER: button under the pointer.
BUTTON-ACTIVE: toggled-on button; a soft tint of the accent.
ACCENT: highlights and the primary action.
ACCENT-SOFT: translucent accent for the background of active items.
ACCENT-CONTRAST: text on a solid accent.
DANGER: error messages.
WARNING: cautions.
FIELD-BACKGROUND: text fields.
OVERLAY-BACKGROUND: translucent background of the performance overlay.
SHADOW: drop shadows of cards and dialogs.
SCRIM: the veil drawn over the window behind a dialog."
  canvas-background grid-minor grid-major axis-x axis-y selection selection-fill crossing-fill
  preview snap-marker panel-background panel-border title-bar-top title-bar-bottom text text-dim
  text-faint button button-hover button-active accent accent-soft accent-contrast danger warning
  field-background overlay-background shadow scrim)

(defparameter *dark-theme-channels*
  '((:canvas-background 0.039 0.059 0.102 1.0)
    (:grid-minor 0.55 0.65 0.85 0.07)
    (:grid-major 0.55 0.65 0.85 0.16)
    (:axis-x 1.0 0.42 0.42 0.6)
    (:axis-y 0.3 0.85 0.55 0.6)
    (:selection 1.0 0.72 0.3 1.0)
    (:selection-fill 0.3 0.65 1.0 0.14)
    (:crossing-fill 0.2 0.85 0.55 0.14)
    (:preview 0.36 0.78 1.0 0.95)
    (:snap-marker 0.13 0.83 0.63 1.0)
    (:panel-background 0.059 0.086 0.149 1.0)
    (:panel-border 0.133 0.188 0.302 1.0)
    (:title-bar-top 0.078 0.114 0.192 1.0)
    (:title-bar-bottom 0.059 0.086 0.149 1.0)
    (:text 0.91 0.933 0.976 1.0)
    (:text-dim 0.576 0.643 0.765 1.0)
    (:text-faint 0.373 0.443 0.58 1.0)
    (:button 0.09 0.125 0.216 1.0)
    (:button-hover 0.122 0.169 0.278 1.0)
    (:button-active 0.13 0.83 0.63 0.17)
    (:accent 0.13 0.83 0.63 1.0)
    (:accent-soft 0.13 0.83 0.63 0.17)
    (:accent-contrast 0.024 0.169 0.133 1.0)
    (:danger 1.0 0.42 0.42 1.0)
    (:warning 1.0 0.72 0.3 1.0)
    (:field-background 0.043 0.071 0.125 1.0)
    (:overlay-background 0.02 0.03 0.06 0.86)
    (:shadow 0.0 0.0 0.0 0.5)
    (:scrim 0.02 0.03 0.07 0.62))
  "Red, green, blue and opacity, each in [0, 1], of every color of the dark theme, by the
keyword of the THEME slot that holds it.")

(defun default-theme ()
  "Return the dark theme used by the application."
  (apply #'make-theme
         (loop for (slot . channels) in *dark-theme-channels*
               append (list slot (apply #'make-rgba channels)))))
