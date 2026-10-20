(in-package #:borba-cad/app)

(defun make-app (config
                 &key telemetry
                      drawing
                      file-path
                      (width 1440d0)
                      (height 900d0)
                      theme)
  "Create the application around a drawing (a new empty one by default).

CONFIG: the configuration.
TELEMETRY: the observability hub, or NIL.
DRAWING: the drawing to edit, or NIL for a new one.
FILE-PATH: the file it came from, or NIL.
WIDTH: initial logical window width.
HEIGHT: initial logical window height.
THEME: the color theme, or NIL for the one kept from the last run, and the dark one when there is none."
  (let* ((drawing (or drawing (dom:make-drawing)))
         (app (%make-app :config config
                         :telemetry telemetry
                         :theme (or theme
                                    (and (base:config-theme config)
                                         (scene:theme-named (base:config-theme config)))
                                    (saved-theme config)
                                    (scene:default-theme))
                         :drawing drawing
                         :history (dom:make-history)
                         :file-path file-path
                         :saved-revision (dom:drawing-revision drawing)
                         :window-width (float width 1d0)
                         :window-height (float height 1d0))))
    (let ((language (or (base:config-language config) (saved-language config))))
      (when language
        (setf i18n:*language* language)))
    (setf (app-interface-zoom app) (or (saved-interface-zoom config) 1d0))
    (setf (app-tool app) (make-tool :select)
          (app-viewport app) (fit-viewport-to-window app (view:default-viewport 1000d0 600d0)))
    (operation-zoom-extents app)
    app))

(defun app-render (app)
  "Rebuild the draw list for the current state: canvas, panels, prompt and overlay. Returns the
scene result and clears the redraw flag.

APP: the application."
  (let* ((started (sbcl:monotonic-nanoseconds))
         (list (app-draw-list app))
         (drawing (app-drawing app))
         (selection (app-selection app))
         (cursor (app-cursor app))
         (theme (app-theme app))
         (tool (app-tool app)))
    (expire-message app)
    (scene:draw-list-reset list)
    (let* ((result (if (layout-mode-p app)
                       (build-layout-scene app list)
                       (scene:build-scene list
                                          drawing
                                          (app-viewport app)
                                          (scene:make-scene-state
                                           :selected-ids (and (plusp (hash-table-count selection)) selection)
                                           :translation (app-translation app)
                                           :preview-entities (tool-preview-entities tool app)
                                           :window (app-window app)
                                           :window-crossing (app-window-crossing app)
                                           :grips (selection-grips app)
                                           :hot-grip (grip-under-pointer app)
                                           :snap (and cursor (cursor-snap-kind cursor) (null (app-prompt app))
                                                      (list (cursor-x cursor) (cursor-y cursor)
                                                            (cursor-snap-kind cursor))))
                                          theme))))
      (ui:draw-widgets list
                       theme
                       (build-widgets app)
                       (float (app-pointer-x app) 1f0)
                       (float (app-pointer-y app) 1f0))
      (when (and (app-overlay-visible app) (app-telemetry app))
        (ui:draw-overlay list
                         theme
                         (obs:telemetry-overlay-lines (app-telemetry app))
                         (+ (float (view:viewport-origin-x (app-viewport app)) 1f0) ui:+padding+)
                         (+ (float (view:viewport-origin-y (app-viewport app)) 1f0) ui:+padding+)))
      (when (app-telemetry app)
        (obs:telemetry-set-entity-counts (app-telemetry app)
                                         (dom:drawing-entity-count drawing)
                                         (scene:scene-result-drawn-entities result))
        (obs:record-operation-result (app-telemetry app)
                                     "render.full-redraw"
                                     (- (sbcl:monotonic-nanoseconds) started)
                                     :debug
                                     :entity-count (dom:drawing-entity-count drawing)
                                     :drawn-entities (scene:scene-result-drawn-entities result)
                                     :vertices (scene:draw-list-vertex-count list)
                                     :result :success))
      (setf (app-last-scene-result app) result
            (app-needs-redraw app) nil)
      result)))
