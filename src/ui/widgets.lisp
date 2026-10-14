(in-package #:borba-cad/ui)

(defstruct (widget (:constructor make-widget (kind
                                              id
                                              x
                                              y
                                              width
                                              height
                                              &key label
                                                   active
                                                   (enabled t)
                                                   blocked
                                                   swatch
                                                   action
                                                   edit
                                                   (align :center)
                                                   color
                                                   hint)))
  "One user-interface element.

KIND: :panel, :button, :label, :row, :field or :swatch.
ID: identifies the widget for focus and tests.
X: left edge in pixels.
Y: top edge in pixels.
WIDTH: width in pixels.
HEIGHT: height in pixels.
LABEL: the text shown.
ACTIVE: toggled on, selected or current.
ENABLED: when false the widget is dimmed and ignores clicks.
BLOCKED: when true a modal covers the widget; it is drawn but ignores the pointer.
SWATCH: an RGBA drawn as a color chip.
ACTION: function of no arguments called on click.
EDIT: the EDIT-STATE of a field that has the keyboard focus, or NIL.
ALIGN: :center or :left text alignment.
COLOR: text color override, an RGBA.
HINT: tooltip-like text shown in the status bar while hovering."
  kind id (x 0f0 :type single-float) (y 0f0 :type single-float)
  (width 0f0 :type single-float) (height 0f0 :type single-float)
  label active enabled blocked swatch action edit align color hint)

(defun interactive-widget-p (widget)
  "Return true for widgets that react to the pointer.

WIDGET: a widget."
  (and (member (widget-kind widget) '(:button :row :field))
       (widget-enabled widget)
       (not (widget-blocked widget))))

(defun widget-at (widgets
                  x
                  y)
  "Return the topmost interactive widget under the point, or NIL. Widgets later in the list
are on top.

WIDGETS: list of widgets in drawing order.
X: pointer x in pixels.
Y: pointer y in pixels."
  (let ((found nil))
    (dolist (widget widgets found)
      (when (and (interactive-widget-p widget)
                 (<= (widget-x widget) x (+ (widget-x widget) (widget-width widget)))
                 (<= (widget-y widget) y (+ (widget-y widget) (widget-height widget))))
        (setf found widget)))))

(defun baseline-for (widget
                     size)
  "Return the y of the text baseline that centers one line of SIZE vertically in WIDGET.

WIDGET: a widget.
SIZE: em size in pixels."
  (+ (widget-y widget) (/ (widget-height widget) 2) (* 0.35f0 size)))

(defconstant +small-widget-width+ 48f0
  "Widgets narrower than this use the small padding, so short labels such as V and L still fit.")

(defconstant +small-padding+ 2f0
  "Horizontal padding of small widgets, in pixels.")

(defun label-padding (widget)
  "Return the horizontal padding used inside WIDGET.

WIDGET: a widget."
  (if (< (widget-width widget) +small-widget-width+) +small-padding+ +padding+))

(defun draw-label (list
                   widget
                   theme
                   color)
  "Draw the label of WIDGET, truncated to fit and aligned.

LIST: the draw list.
WIDGET: a widget.
THEME: the theme.
COLOR: an RGBA for the text."
  (declare (ignore theme))
  (when (widget-label widget)
    (let* ((available (- (widget-width widget) (* 2 (label-padding widget))
                         (if (widget-swatch widget) (+ +padding+ 14f0) 0f0)))
           (text (fit-text (widget-label widget) available))
           (text-x (+ (widget-x widget)
                      (if (widget-swatch widget) (+ +padding+ 14f0) 0f0)
                      (if (eq (widget-align widget) :center)
                          (/ (- (widget-width widget) (text-width-pixels text)) 2)
                          (label-padding widget)))))
      (scene:add-text list
                      text
                      (if (eq (widget-align widget) :center)
                          (max (+ (widget-x widget) 2f0) text-x)
                          text-x)
                      (baseline-for widget +font-pixels+)
                      +font-pixels+
                      color))))

(defun dim-color (color)
  "Return COLOR at reduced opacity, for disabled widgets.

COLOR: an RGBA."
  (scene:make-rgba (scene:rgba-r color)
                   (scene:rgba-g color)
                   (scene:rgba-b color)
                   (* 0.45f0 (scene:rgba-a color))))

(defun draw-widget (list
                    widget
                    theme
                    pointer-x
                    pointer-y)
  "Draw one widget.

LIST: the draw list.
WIDGET: the widget.
THEME: the theme.
POINTER-X: pointer x in pixels, for hover highlighting.
POINTER-Y: pointer y in pixels."
  (let* ((x (widget-x widget)) (y (widget-y widget))
         (width (widget-width widget)) (height (widget-height widget))
         (hover (and (interactive-widget-p widget)
                     (<= x pointer-x (+ x width)) (<= y pointer-y (+ y height))))
         (text-color (cond ((widget-color widget))
                           ((widget-enabled widget) (scene:theme-text theme))
                           (t (scene:theme-text-dim theme)))))
    (ecase (widget-kind widget)
      (:panel
       (scene:add-filled-rectangle list
                                   x
                                   y
                                   width
                                   height
                                   (scene:theme-panel-background theme))
       (scene:add-rectangle-outline list
                                    x
                                    y
                                    width
                                    height
                                    (scene:theme-panel-border theme)))
      (:button
       (scene:add-filled-rectangle list
                                   x
                                   y
                                   width
                                   height
                                   (cond ((widget-active widget) (scene:theme-button-active theme))
                                         (hover (scene:theme-button-hover theme))
                                         (t (scene:theme-button theme))))
       (scene:add-rectangle-outline list
                                    x
                                    y
                                    width
                                    height
                                    (scene:theme-panel-border theme))
       (draw-label list
                   widget
                   theme
                   (if (widget-enabled widget) text-color (dim-color text-color))))
      (:row
       (when (or (widget-active widget) hover)
         (scene:add-filled-rectangle list
                                     x
                                     y
                                     width
                                     height
                                     (if (widget-active widget)
                                         (scene:theme-button-active theme)
                                         (scene:theme-button-hover theme))))
       (when (widget-swatch widget)
         (scene:add-filled-rectangle list
                                     (+ x +padding+)
                                     (+ y 5f0)
                                     14f0
                                     (- height 10f0)
                                     (widget-swatch widget))
         (scene:add-rectangle-outline list
                                      (+ x +padding+)
                                      (+ y 5f0)
                                      14f0
                                      (- height 10f0)
                                      (scene:theme-panel-border theme)))
       (draw-label list
                   widget
                   theme
                   text-color))
      (:label
       (draw-label list
                   widget
                   theme
                   text-color))
      (:swatch
       (scene:add-filled-rectangle list
                                   x
                                   y
                                   width
                                   height
                                   (widget-swatch widget))
       (scene:add-rectangle-outline list
                                    x
                                    y
                                    width
                                    height
                                    (scene:theme-panel-border theme)))
      (:field
       (scene:add-filled-rectangle list
                                   x
                                   y
                                   width
                                   height
                                   (scene:theme-field-background theme))
       (scene:add-rectangle-outline list
                                    x
                                    y
                                    width
                                    height
                                    (if (widget-edit widget)
                                        (scene:theme-accent theme)
                                        (scene:theme-panel-border theme)))
       (let ((edit (widget-edit widget)))
         (if edit
             (let* ((text (edit-state-text edit))
                    (text-x (+ x +padding+))
                    (baseline (baseline-for widget +font-pixels+))
                    (caret-x (+ text-x (text-width-pixels (subseq text 0 (edit-state-caret edit))))))
               (when (edit-state-replace-on-type edit)
                 (scene:add-filled-rectangle list
                                             text-x
                                             (+ y 3f0)
                                             (text-width-pixels text)
                                             (- height 6f0)
                                             (scene:theme-selection-fill theme)))
               (scene:add-text list
                               text
                               text-x
                               baseline
                               +font-pixels+
                               (scene:theme-text theme))
               (scene:add-line list
                               caret-x
                               (+ y 4f0)
                               caret-x
                               (- (+ y height) 4f0)
                               (scene:theme-accent theme)))
             (draw-label list
                         widget
                         theme
                         (if hover (scene:theme-text theme) text-color))))))))

(defun draw-widgets (list
                     theme
                     widgets
                     pointer-x
                     pointer-y)
  "Draw every widget in order.

LIST: the draw list.
THEME: the theme.
WIDGETS: list of widgets in drawing order.
POINTER-X: pointer x in pixels, for hover highlighting.
POINTER-Y: pointer y in pixels."
  (dolist (widget widgets)
    (draw-widget list
                 widget
                 theme
                 pointer-x
                 pointer-y)))

(defun draw-overlay (list
                     theme
                     lines
                     x
                     y)
  "Draw the performance overlay: a translucent box with one text line per entry.

LIST: the draw list.
THEME: the theme.
LINES: list of strings.
X: left edge in pixels.
Y: top edge in pixels."
  (when lines
    (let* ((size +small-font-pixels+)
           (line-height (* 1.35f0 size))
           (width (+ (* 2 +padding+)
                     (reduce #'max lines :key (lambda (line) (text-width-pixels line size)))))
           (height (+ (* 2 +padding+) (* line-height (length lines)))))
      (scene:add-filled-rectangle list
                                  x
                                  y
                                  width
                                  height
                                  (scene:theme-overlay-background theme))
      (loop for line in lines
            for row from 0
            do (scene:add-text list
                               line
                               (+ x +padding+)
                               (+ y +padding+ (* line-height (1+ row)) (* -0.3f0 size))
                               size
                               (scene:theme-text theme))))))
