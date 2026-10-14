(defpackage #:borba-cad/scene
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:geo #:borba-cad/geometry)
                    (#:dom #:borba-cad/domain)
                    (#:view #:borba-cad/viewport))
  (:documentation "The boundary between the headless application and any renderer: a draw list
of screen-space vertices and text runs, a theme, and the builder that turns a drawing, a
viewport and the interaction state into a draw list.")
  (:export
   ;; Colors and theme
   #:rgba
   #:make-rgba
   #:rgba-r
   #:rgba-g
   #:rgba-b
   #:rgba-a
   #:rgb->rgba
   #:theme
   #:default-theme
   #:theme-canvas-background
   #:theme-grid-minor
   #:theme-grid-major
   #:theme-axis-x
   #:theme-axis-y
   #:theme-crossing-fill
   #:theme-title-bar-top
   #:theme-title-bar-bottom
   #:theme-text-faint
   #:theme-accent-soft
   #:theme-accent-contrast
   #:theme-warning
   #:theme-shadow
   #:theme-scrim
   #:theme-selection
   #:theme-selection-fill
   #:theme-preview
   #:theme-snap-marker
   #:theme-panel-background
   #:theme-panel-border
   #:theme-text
   #:theme-text-dim
   #:theme-button
   #:theme-button-hover
   #:theme-button-active
   #:theme-accent
   #:theme-danger
   #:theme-field-background
   #:theme-overlay-background
   ;; Draw list
   #:+floats-per-vertex+
   #:draw-list
   #:make-draw-list
   #:draw-list-reset
   #:draw-list-vertices
   #:draw-list-vertex-count
   #:draw-list-commands
   #:draw-list-text-runs
   #:draw-list-white-u
   #:draw-list-white-v
   #:draw-command
   #:draw-command-kind
   #:draw-command-start
   #:draw-command-count
   #:text-run
   #:text-run-string
   #:text-run-x
   #:text-run-y
   #:text-run-size
   #:text-run-angle
   #:text-run-font
   #:text-run-color
   #:add-line
   #:add-triangle
   #:add-filled-rectangle
   #:add-rectangle-outline
   #:add-text
   #:+minimum-shape-segments+
   #:+maximum-shape-segments+
   #:shape-segments
   #:with-alpha
   #:add-thick-line
   #:add-disc
   #:add-ring
   #:add-arc-stroke
   #:rounded-rectangle-points
   #:add-rounded-rectangle
   #:add-rounded-rectangle-outline
   #:add-gradient-rectangle
   #:add-shadow
   #:add-dashed-line
   #:add-polygon
   #:draw-list-line-count
   #:draw-list-triangle-count
   ;; Scene
   #:+circle-chordal-error-pixels+
   #:+circle-minimum-segments+
   #:+circle-maximum-segments+
   #:+minimum-text-pixels+
   #:+culling-margin-pixels+
   #:+snap-marker-half-size-pixels+
   #:circle-segment-count
   #:scene-state
   #:make-scene-state
   #:scene-state-selected-ids
   #:scene-state-translation
   #:scene-state-preview-entities
   #:scene-state-window
   #:scene-state-snap
   #:scene-result
   #:scene-result-drawn-entities
   #:scene-result-culled-entities
   #:build-scene
   #:add-drawing-entities
   #:add-grid
   #:add-snap-marker))
