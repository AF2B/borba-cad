(in-package #:borba-cad/ui)

;;;; Widgets: a flat list of rectangles that carry what is needed to draw them and to react to the
;;;; pointer. The application rebuilds the list from its state every frame, so what is drawn and
;;;; what is hit always agree.

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
                                                   hint
                                                   shortcut
                                                   icon
                                                   (variant :default)
                                                   (font :ui)
                                                   (size +font-pixels+)
                                                   (trailing 0f0))))
  "One user-interface element.

KIND: :panel, :card, :button, :label, :row, :field, :swatch, :scrim, :separator, :icon, :logo or
:tooltip.
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
ALIGN: :left, :center or :right text alignment.
COLOR: text or icon color override, an RGBA.
HINT: what the widget does, shown as a tooltip and in the status bar while hovering.
SHORTCUT: the keys that do the same, shown beside the hint, or NIL.
ICON: the name of the icon drawn in a button, row or on its own.
VARIANT: for buttons, :default, :primary, :ghost, :tool (palette), :icon (title bar) or :chip
(a pill that toggles); for panels, :default, :title-bar, :status-bar, :palette, :side or :console.
FONT: :ui, :ui-strong or :mono.
SIZE: em size of the text in pixels.
TRAILING: pixels at the right edge that the text must stay clear of, where other widgets sit
over the widget."
  kind
  id
  (x 0f0 :type single-float)
  (y 0f0 :type single-float)
  (width 0f0 :type single-float)
  (height 0f0 :type single-float)
  label
  active
  enabled
  blocked
  swatch
  action
  edit
  align
  color
  hint
  shortcut
  icon
  variant
  font
  size
  trailing)

(defun interactive-widget-p (widget)
  "Return true for widgets that react to the pointer.

WIDGET: a widget."
  (and (member (widget-kind widget) '(:button :row :field))
       (widget-enabled widget)
       (not (widget-blocked widget))))

(defun widget-contains-p (widget
                          x
                          y)
  "Return true when the point lies inside WIDGET.

WIDGET: a widget.
X: point x in pixels.
Y: point y in pixels."
  (and (<= (widget-x widget) x (+ (widget-x widget) (widget-width widget)))
       (<= (widget-y widget) y (+ (widget-y widget) (widget-height widget)))))

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
                 (widget-contains-p widget
                                    x
                                    y))
        (setf found widget)))))
