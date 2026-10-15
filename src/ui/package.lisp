(defpackage #:borba-cad/ui
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:i18n #:borba-cad/i18n)
                    (#:geo #:borba-cad/geometry)
                    (#:scene #:borba-cad/scene))
  (:documentation "Declarative user-interface widgets: layout of the window, drawing and hit
testing of buttons, labels, rows and text fields into the draw list, single-line text
editing, and the performance overlay. No input device and no renderer appear here.")
  (:export
   ;; Metrics
   #:+font-pixels+
   #:+small-font-pixels+
   #:+heading-font-pixels+
   #:+row-height+
   #:+padding+
   #:+gap+
   #:+radius+
   #:+small-radius+
   #:+card-radius+
   #:+toolbar-height+
   #:+tool-palette-width+
   #:+status-height+
   #:+side-panel-width+
   #:+panel-margin+
   #:+icon-size+
   #:+tool-icon-size+
   #:+small-icon-size+
   #:+icon-button-size+
   #:+tooltip-height+
   #:+chip-height+
   #:+chip-padding+
   #:*text-measurer*
   #:fixed-advance-width
   #:text-width-pixels
   #:fit-text
   #:wrap-text
   #:visible-text-window
   ;; Layout
   #:rect
   #:make-rect
   #:rect-x
   #:rect-y
   #:rect-width
   #:rect-height
   #:rect-contains-p
   #:layout
   #:compute-layout
   #:layout-toolbar
   #:layout-tool-palette
   #:layout-canvas
   #:layout-side-panel
   #:layout-layers-panel
   #:layout-inspector-panel
   #:layout-status-bar
   #:layout-console
   #:+console-height+
   ;; Icons
   #:icon-names
   #:icon-shapes
   #:draw-icon
   ;; Widgets
   #:widget
   #:make-widget
   #:widget-kind
   #:widget-id
   #:widget-x
   #:widget-y
   #:widget-width
   #:widget-height
   #:widget-label
   #:widget-active
   #:widget-enabled
   #:widget-blocked
   #:widget-swatch
   #:widget-action
   #:widget-edit
   #:widget-align
   #:widget-color
   #:widget-hint
   #:widget-shortcut
   #:widget-icon
   #:widget-variant
   #:widget-font
   #:widget-size
   #:widget-trailing
   #:widget-contains-p
   #:draw-logo
   #:tooltip-width
   #:draw-widgets
   #:widget-at
   #:interactive-widget-p
   #:draw-overlay
   ;; Text editing
   #:edit-state
   #:make-edit-state
   #:edit-state-text
   #:edit-state-caret
   #:edit-state-maximum-length
   #:edit-state-replace-on-type
   #:edit-insert
   #:edit-backspace
   #:edit-delete
   #:edit-move
   #:edit-select-all-replace
   #:edit-handle-key))
