(in-package #:borba-cad/ui)

;;;; The visual tokens of the widgets: widths of strokes, opacities, sizes of the small parts that
;;;; decorate buttons, rows, fields and tooltips. Colors live in the theme; what is here is
;;;; everything else that gives the interface its look, in one place to be tuned.

;;; Strokes

(defconstant +hairline+ 1f0
  "Width of a border or a separator, in pixels.")

(defconstant +focus-ring-width+ 1.5f0
  "Width of the outline of a field that has the keyboard focus, in pixels.")

(defconstant +caret-width+ 1.5f0
  "Width of the text caret, in pixels.")

(defconstant +caret-inset+ 6f0
  "Space above and below the caret inside its field, in pixels.")

;;; Text

(defconstant +baseline-offset+ 0.36f0
  "How far below the vertical middle of a line its baseline sits, as a fraction of the em size.")

;;; Shadows

(defconstant +card-shadow-spread+ 10f0
  "How far the shadow of a card reaches beyond it, in pixels.")

(defconstant +card-shadow-opacity+ 0.5f0
  "Share of the theme's shadow opacity a card casts.")

(defconstant +tooltip-shadow-spread+ 8f0
  "How far the shadow of a tooltip reaches beyond it, in pixels.")

(defconstant +tooltip-shadow-opacity+ 0.6f0
  "Share of the theme's shadow opacity a tooltip casts.")

;;; Buttons

(defconstant +tool-radius+ 10f0
  "Corner radius of a button of the tool palette, in pixels.")

(defconstant +primary-hover-opacity+ 0.88f0
  "Opacity of a primary button under the pointer.")

(defconstant +primary-disabled-opacity+ 0.35f0
  "Opacity of a primary button that cannot be pressed.")

(defconstant +active-outline-opacity+ 0.5f0
  "Opacity of the accent outline of a toggled-on button.")

(defconstant +chip-active-outline-opacity+ 0.6f0
  "Opacity of the accent outline of a toggled-on chip.")

(defconstant +indicator-width+ 3f0
  "Width of the accent bar that marks the active tool or the current row, in pixels.")

(defconstant +indicator-radius+ 1.5f0
  "Corner radius of that bar, in pixels.")

(defconstant +tool-indicator-height+ 18f0
  "Height of the bar beside the active tool, in pixels.")

(defconstant +tool-indicator-offset+ 6f0
  "How far left of the active tool's button its bar sits, in pixels.")

(defconstant +row-indicator-margin+ 6f0
  "Space above and below the bar of the current row, in pixels.")

;;; Chips

(defconstant +chip-dot-center+ 12f0
  "Distance from the left edge of a chip to the center of its on/off dot, in pixels.")

(defconstant +chip-dot-radius+ 3f0
  "Radius of the on/off dot of a chip, in pixels.")

(defconstant +chip-dot-inset+ 14f0
  "Room the dot takes before the label of a chip, in pixels.")

;;; Rows and labels

(defconstant +swatch-radius+ 6f0
  "Radius of the color dot of a list row, in pixels.")

(defconstant +swatch-ring-radius+ 5.5f0
  "Radius of the thin ring that keeps a dark color dot visible on a dark row, in pixels.")

(defconstant +swatch-ring-opacity+ 0.25f0
  "Opacity of that ring.")

(defconstant +swatch-inset+ 18f0
  "Room the color dot takes before the label of a row, in pixels.")

(defconstant +row-icon-inset+ 24f0
  "Room an icon takes before the label of a row, in pixels.")

(defconstant +label-dot-center+ 4f0
  "Distance from the left edge of a label to the center of its dot, in pixels.")

(defconstant +label-dot-radius+ 3.5f0
  "Radius of the dot before a label, in pixels.")

(defconstant +label-dot-inset+ 14f0
  "Room the dot takes before the text of a label, in pixels.")

(defconstant +label-icon-inset+ 22f0
  "Room an icon takes before the text of a label, in pixels.")

;;; Fields

(defconstant +selection-margin+ 2f0
  "Space around selected text, left and right, in pixels.")

(defconstant +selection-inset+ 4f0
  "Space above and below selected text inside its field, in pixels.")

(defconstant +selection-radius+ 3f0
  "Corner radius of the highlight behind selected text, in pixels.")

;;; Overlay

(defconstant +overlay-line-spacing+ 1.4f0
  "Distance between the lines of the performance overlay, as a fraction of the em size.")

(defconstant +overlay-baseline-lift+ -0.3f0
  "How far above the bottom of its line the baseline of an overlay line sits, as a fraction of the em size.")

;;; Tooltips

(defconstant +tooltip-padding+ 10f0
  "Space between a tooltip's edge and its text, in pixels.")

(defconstant +key-cap-padding+ 5f0
  "Space between a key cap's edge and its letters, in pixels.")

(defconstant +key-cap-inset+ 5f0
  "Space above and below a key cap inside its tooltip, in pixels.")

(defconstant +key-cap-radius+ 4f0
  "Corner radius of a key cap, in pixels.")
