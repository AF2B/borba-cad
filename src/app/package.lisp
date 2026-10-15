(defpackage #:borba-cad/app
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:i18n #:borba-cad/i18n)
                    (#:dxf #:borba-cad/dxf)
                    (#:script #:borba-cad/script)
                    (#:image #:borba-cad/image)
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
   #:window-state-event
   #:make-window-state-event
   #:window-state-event-kind
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
   #:app-window-crossing
   #:app-polygon-sides
   #:app-window-width
   #:app-window-height
   #:app-dirty-p
   #:app-title
   #:app-wants-text-input-p
   #:app-request-redraw
   #:selection-ids
   #:selected-entities
   #:select-ids
   #:selection-grips
   #:grip-under-pointer
   #:app-grip-drag
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
   #:operation-import-dxf
   #:operation-export-dxf
   #:open-welcome
   #:close-welcome
   #:app-welcome
   #:welcome-recents
   #:welcome-highlight
   #:app-image-capturer
   #:operation-export-image
   #:export-image-from-app
   #:operation-toggle-console
   #:operation-run-script
   #:operation-export-script
   #:run-script-file-in-app
   #:export-script-from-app
   #:console-open-p
   #:console-say
   #:console-submit
   #:app-console
   #:console-lines
   #:console-scroll
   #:console-focused-p
   #:console-history
   #:console-pending
   #:app-layout
   #:import-dxf-into-app
   #:export-dxf-from-app
   #:operation-copy
   #:operation-cut
   #:operation-paste
   #:clipboard-drawing
   #:operation-set-language
   #:load-drawing-into-app
   #:save-app-drawing
   #:commit-changes
   #:report-error
   #:report-internal-error
   #:set-message
   #:expire-message
   #:clear-message
   ;; Tools
   #:tool
   #:select-tool
   #:line-tool
   #:rectangle-tool
   #:circle-tool
   #:polyline-tool
   #:text-tool
   #:dimension-tool
   #:arc-tool
   #:ellipse-tool
   #:polygon-tool
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
   ;; Menus
   #:app-menu
   #:menu
   #:menu-kind
   #:close-menu
   #:open-menu
   #:toggle-menu
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
   #:show-notice
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
