(defpackage #:borba-cad/app
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:sbcl #:borba-cad/sbcl-support)
                    (#:geo #:borba-cad/geometry)
                    (#:dom #:borba-cad/domain)
                    (#:view #:borba-cad/viewport)
                    (#:persist #:borba-cad/persistence)
                    (#:obs #:borba-cad/observability)
                    (#:scene #:borba-cad/scene)
                    (#:ui #:borba-cad/ui))
  (:documentation "The application: state, input events, tools, operations, panels and frame
building. It is headless: a window system feeds it events and draws the draw list it builds.")
  (:export
   ;; Events
   #:pointer-event
   #:make-pointer-event
   #:pointer-event-action
   #:pointer-event-button
   #:pointer-event-x
   #:pointer-event-y
   #:pointer-event-modifiers
   #:wheel-event
   #:make-wheel-event
   #:wheel-event-delta
   #:wheel-event-x
   #:wheel-event-y
   #:wheel-event-modifiers
   #:key-event
   #:make-key-event
   #:key-event-key
   #:key-event-action
   #:key-event-modifiers
   #:text-event
   #:make-text-event
   #:text-event-text
   #:resize-event
   #:make-resize-event
   #:resize-event-width
   #:resize-event-height
   #:resize-event-scale
   #:file-drop-event
   #:make-file-drop-event
   #:file-drop-event-path
   #:quit-event
   #:make-quit-event
   ;; Constants
   #:+pick-radius-pixels+
   #:+snap-radius-pixels+
   #:+zoom-step-per-notch+
   #:+drag-threshold-pixels+
   ;; State
   #:app
   #:make-app
   #:app-config
   #:app-telemetry
   #:app-theme
   #:app-drawing
   #:app-history
   #:app-viewport
   #:app-selection
   #:app-tool
   #:app-file-path
   #:app-prompt
   #:app-message
   #:app-focus
   #:app-edit
   #:app-draw-list
   #:app-cursor
   #:app-translation
   #:app-window
   #:app-overlay-visible
   #:app-needs-redraw
   #:app-quit-requested
   #:app-last-scene-result
   #:app-last-error
   #:app-layer-scroll
   #:app-inspector-scroll
   #:app-window-width
   #:app-window-height
   #:app-dirty-p
   #:app-title
   #:app-wants-text-input-p
   #:app-request-redraw
   #:selection-ids
   #:selected-entities
   ;; Cursor
   #:cursor
   #:cursor-raw-x
   #:cursor-raw-y
   #:cursor-x
   #:cursor-y
   #:cursor-snap-kind
   ;; Operations
   #:operation-new
   #:operation-open
   #:operation-save
   #:operation-save-as
   #:operation-undo
   #:operation-redo
   #:operation-delete-selection
   #:operation-select-all
   #:operation-zoom-extents
   #:operation-set-tool
   #:operation-toggle-grid
   #:operation-toggle-snap
   #:operation-toggle-object-snap
   #:operation-cycle-display-unit
   #:operation-toggle-overlay
   #:operation-quit
   #:operation-add-layer
   #:operation-rename-layer
   #:operation-delete-layer
   #:operation-toggle-layer-visible
   #:operation-toggle-layer-locked
   #:operation-cycle-layer-color
   #:operation-set-current-layer
   #:operation-assign-selection-layer
   #:operation-edit-property
   #:load-drawing-into-app
   #:save-app-drawing
   #:commit-changes
   #:report-error
   #:set-message
   ;; Tools
   #:tool
   #:select-tool
   #:line-tool
   #:rectangle-tool
   #:circle-tool
   #:polyline-tool
   #:text-tool
   #:dimension-tool
   #:make-tool
   #:tool-kind
   #:tool-prompt
   #:tool-preview-entities
   #:tool-in-progress-p
   ;; File dialog
   #:app-dialog
   #:file-dialog
   #:file-dialog-mode
   #:file-dialog-directory
   #:file-dialog-entries
   #:file-dialog-selected
   #:file-dialog-scroll
   #:file-dialog-capacity
   #:file-dialog-show-all
   #:file-dialog-filter-edit
   #:file-dialog-name-edit
   #:file-dialog-location-edit
   #:file-dialog-focus
   #:file-dialog-message
   #:file-dialog-message-severity
   #:file-dialog-places
   #:file-dialog-recent
   #:file-dialog-continuation
   #:file-entry
   #:file-entry-name
   #:file-entry-kind
   #:file-entry-path
   #:file-entry-size
   #:file-entry-modified
   #:open-file-dialog
   #:close-file-dialog
   #:visible-entries
   #:selected-entry
   #:list-entries
   #:breadcrumbs
   #:default-places
   #:typed-path
   #:create-folder
   #:open-directory
   #:open-parent-directory
   #:select-entry
   #:move-selection
   #:format-modified-time
   #:format-entry-size
   ;; Clipboard
   #:app-clipboard-reader
   #:app-clipboard-writer
   #:clipboard-text
   #:set-clipboard-text
   #:single-line
   #:edit-with-clipboard
   ;; Prompts
   #:prompt
   #:prompt-kind
   #:prompt-title
   #:prompt-message
   #:prompt-edit
   #:prompt-choices
   #:open-text-prompt
   #:open-choice-prompt
   #:close-prompt
   ;; Input, frame, scenario
   #:app-handle-event
   #:app-resize
   #:app-render
   #:app-status-texts
   #:build-widgets
   #:format-property-value
   #:demo-steps
   #:demo-save-path
   #:scenario-click
   #:scenario-drag
   #:scenario-key
   #:scenario-type
   #:scenario-wheel
   #:apply-benchmark-input
   #:write-demo-drawing
   #:format-model-number))
