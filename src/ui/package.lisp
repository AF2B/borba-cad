(defpackage #:borba-cad/ui
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:geo #:borba-cad/geometry)
                    (#:scene #:borba-cad/scene))
  (:documentation "Declarative user-interface widgets: layout of the window, drawing and hit
testing of buttons, labels, rows and text fields into the draw list, single-line text
editing, and the performance overlay. No input device and no renderer appear here.")
  (:export
   ;; Metrics
   #:+font-pixels+
   #:+small-font-pixels+
   #:+row-height+
   #:+padding+
   #:+toolbar-height+
   #:+status-height+
   #:+side-panel-width+
   #:text-width-pixels
   #:fit-text
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
   #:layout-canvas
   #:layout-side-panel
   #:layout-layers-panel
   #:layout-inspector-panel
   #:layout-status-bar
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
   #:edit-insert
   #:edit-backspace
   #:edit-delete
   #:edit-move
   #:edit-select-all-replace
   #:edit-handle-key))
