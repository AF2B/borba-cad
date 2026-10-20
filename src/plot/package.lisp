(defpackage #:borba-cad/plot
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:i18n #:borba-cad/i18n)
                    (#:geo #:borba-cad/geometry)
                    (#:dom #:borba-cad/domain))
  (:documentation "Plotting a drawing on a sheet of paper, to scale: the sheet sizes, the scale and where the drawing
goes on the sheet, the entities flattened into strokes, filled triangles and text in sheet coordinates, and two
writers of that, one of a PDF file made by hand and one of an SVG file.")
  (:export
   ;; Settings
   #:*paper-sizes*
   #:paper-name
   #:paper-size
   #:plot-settings
   #:make-plot-settings
   #:plot-settings-paper
   #:plot-settings-ratio
   #:plot-settings-monochrome
   #:plot-settings-line-width
   #:plot-settings-margin
   #:parse-plot-scale
   #:scale-text
   ;; Planning
   #:plot-plan
   #:plan-plot
   #:plot-plan-width
   #:plot-plan-height
   #:plot-plan-ratio
   #:plot-plan-fits
   #:plot-plan-items
   #:plot-plan-settings
   #:plot-plan-entity-count
   ;; Items
   #:plot-stroke
   #:plot-stroke-points
   #:plot-stroke-closed
   #:plot-stroke-color
   #:plot-stroke-width
   #:make-plot-stroke
   #:plot-fill
   #:plot-fill-points
   #:plot-fill-color
   #:plot-text
   #:plot-text-string
   #:plot-text-x
   #:plot-text-y
   #:plot-text-height
   #:plot-text-angle
   #:plot-text-color
   #:make-plot-text
   #:make-plot-fill
   ;; Sheets
   #:sheet-size
   #:sheet-frame
   #:sheet-body
   #:title-cells
   #:title-block-items
   #:fitted-text
   #:viewport-rectangle
   #:viewport-items
   #:plan-sheet
   #:plan-sheets
   #:default-viewport
   #:visible-entities
   #:entities-bounds
   #:fit-ratio
   #:clip-item
   #:clip-stroke
   #:clip-fill
   #:+frame-margin+
   #:+title-block-height+
   #:+default-viewport-inset+
   ;; Writing
   #:render-pdf
   #:render-sheets-pdf
   #:plans-pdf
   #:render-svg
   #:+points-per-millimeter+))
