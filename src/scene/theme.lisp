(in-package #:borba-cad/scene)

(defstruct (theme (:constructor make-theme (&key kind
                                                 canvas-background
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

KIND: :dark or :light, which says how the colors of the layers are drawn on this canvas.
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
  (kind :dark :type (member :dark :light))
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

(defparameter *light-theme-channels*
  '((:canvas-background 0.980 0.982 0.988 1.0)
    (:grid-minor 0.25 0.32 0.45 0.07)
    (:grid-major 0.25 0.32 0.45 0.17)
    (:axis-x 0.85 0.15 0.15 0.65)
    (:axis-y 0.10 0.60 0.30 0.65)
    (:selection 0.0 0.40 0.85 1.0)
    (:selection-fill 0.0 0.45 0.95 0.12)
    (:crossing-fill 0.10 0.65 0.35 0.14)
    (:preview 0.0 0.45 0.85 0.9)
    (:snap-marker 0.0 0.58 0.45 1.0)
    (:panel-background 1.0 1.0 1.0 1.0)
    (:panel-border 0.870 0.885 0.915 1.0)
    (:title-bar-top 1.0 1.0 1.0 1.0)
    (:title-bar-bottom 0.965 0.971 0.982 1.0)
    (:text 0.090 0.110 0.165 1.0)
    (:text-dim 0.340 0.380 0.470 1.0)
    (:text-faint 0.590 0.620 0.690 1.0)
    (:button 0.945 0.953 0.969 1.0)
    (:button-hover 0.890 0.910 0.945 1.0)
    (:button-active 0.0 0.55 0.43 0.16)
    (:accent 0.0 0.55 0.43 1.0)
    (:accent-soft 0.0 0.55 0.43 0.14)
    (:accent-contrast 1.0 1.0 1.0 1.0)
    (:danger 0.80 0.16 0.16 1.0)
    (:warning 0.78 0.46 0.0 1.0)
    (:field-background 1.0 1.0 1.0 1.0)
    (:overlay-background 1.0 1.0 1.0 0.92)
    (:shadow 0.08 0.12 0.25 0.28)
    (:scrim 0.10 0.12 0.20 0.38))
  "Red, green, blue and opacity, each in [0, 1], of every color of the light theme, by the
keyword of the THEME slot that holds it.")

(defun theme-from-channels (kind
                            channels)
  "Return a theme made of a list of colors.

KIND: :dark or :light.
CHANNELS: a list of (slot red green blue opacity), one for every color of a theme."
  (apply #'make-theme
         :kind kind
         (loop for (slot . values) in channels
               append (list slot (apply #'make-rgba values)))))

(defun default-theme ()
  "Return the dark theme, the one the application starts with."
  (theme-from-channels :dark *dark-theme-channels*))

(defun light-theme ()
  "Return the light theme: a white canvas and panels, and the same accents darkened to read on white."
  (theme-from-channels :light *light-theme-channels*))

(defun theme-named (name)
  "Return the theme called NAME, or NIL when there is none.

NAME: :dark or :light, or their names as text."
  (let ((key (if (stringp name)
                 (find-symbol (string-upcase name) :keyword)
                 name)))
    (case key
      (:dark (default-theme))
      (:light (light-theme)))))

(defun theme-name (theme)
  "Return the name under which a theme is kept between runs.

THEME: a theme."
  (string-downcase (symbol-name (theme-kind theme))))

;;; The colors of the layers on a canvas

(defconstant +light-ink-lightness+ 0.40f0
  "Greatest lightness (0 to 1) a layer color has on the light canvas: a lighter one is darkened to this, keeping
its hue, so that a pastel that reads on the dark canvas reads on the white one.")

(defconstant +light-neutral-ink-lightness+ 0.12f0
  "Greatest lightness a layer color that is nearly gray has on the light canvas: near-black, as the white of
a drawing on a dark canvas is the black of a drawing on paper.")

(defconstant +neutral-saturation+ 0.12f0
  "Saturation (0 to 1) below which a color counts as gray.")

(defun rgb->hsl (red
                 green
                 blue)
  "Return the hue (0 to 6), the saturation and the lightness (both 0 to 1) of a color.

RED: red channel, 0 to 1.
GREEN: green channel, 0 to 1.
BLUE: blue channel, 0 to 1."
  (let* ((top (max red green blue))
         (bottom (min red green blue))
         (delta (- top bottom))
         (lightness (/ (+ top bottom) 2f0))
         (saturation (if (zerop delta)
                         0f0
                         (/ delta (- 1f0 (abs (- (* 2f0 lightness) 1f0))))))
         (hue (cond ((zerop delta) 0f0)
                    ((= top red) (mod (/ (- green blue) delta) 6f0))
                    ((= top green) (+ 2f0 (/ (- blue red) delta)))
                    (t (+ 4f0 (/ (- red green) delta))))))
    (values hue saturation lightness)))

(defun hsl->rgb (hue
                 saturation
                 lightness)
  "Return the red, green and blue channels (0 to 1) of a color.

HUE: 0 to 6.
SATURATION: 0 to 1.
LIGHTNESS: 0 to 1."
  (let* ((chroma (* (- 1f0 (abs (- (* 2f0 lightness) 1f0))) saturation))
         (middle (* chroma (- 1f0 (abs (- (mod hue 2f0) 1f0)))))
         (offset (- lightness (/ chroma 2f0))))
    (multiple-value-bind (red green blue)
        (cond ((< hue 1f0) (values chroma middle 0f0))
              ((< hue 2f0) (values middle chroma 0f0))
              ((< hue 3f0) (values 0f0 chroma middle))
              ((< hue 4f0) (values 0f0 middle chroma))
              ((< hue 5f0) (values middle 0f0 chroma))
              (t (values chroma 0f0 middle)))
      (values (+ red offset) (+ green offset) (+ blue offset)))))

(defun theme-ink (theme
                  color)
  "Return the color a layer color is drawn in on the canvas of a theme: as it is on the dark canvas, and darkened
on the light one when it would be too pale to see, a gray going to near-black.

THEME: the theme.
COLOR: an RGBA, the color of a layer."
  (if (eq (theme-kind theme) :dark)
      color
      (multiple-value-bind (hue saturation lightness) (rgb->hsl (rgba-r color)
                                                                (rgba-g color)
                                                                (rgba-b color))
        (let ((limit (if (< saturation +neutral-saturation+)
                         +light-neutral-ink-lightness+
                         +light-ink-lightness+)))
          (if (<= lightness limit)
              color
              (multiple-value-bind (red green blue) (hsl->rgb hue
                                                              saturation
                                                              limit)
                (make-rgba red
                           green
                           blue
                           (rgba-a color))))))))
