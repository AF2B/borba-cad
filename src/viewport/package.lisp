(defpackage #:borba-cad/viewport
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:geo #:borba-cad/geometry))
  (:documentation "The only place where world coordinates (millimetres, +y up) meet screen
coordinates (logical pixels, +y down). Zoom, pan, fitting and the adaptive grid.")
  (:export
   ;; Typed points
   #:world-point
   #:make-world-point
   #:world-point-x
   #:world-point-y
   #:screen-point
   #:make-screen-point
   #:screen-point-x
   #:screen-point-y
   ;; Viewport
   #:+minimum-scale+
   #:+maximum-scale+
   #:+default-visible-width+
   #:+fit-margin-pixels+
   #:viewport
   #:make-viewport
   #:default-viewport
   #:viewport-center-x
   #:viewport-center-y
   #:viewport-scale
   #:viewport-width
   #:viewport-height
   #:viewport-origin-x
   #:viewport-origin-y
   #:clamp-scale
   #:world->screen
   #:screen->world
   #:world-point->screen
   #:screen-point->world
   #:world-length->pixels
   #:pixels->world-length
   #:resize-viewport
   #:pan-viewport
   #:zoom-viewport-at
   #:fit-viewport-to-bounds
   #:viewport-visible-bounds
   ;; Grid
   #:+minimum-grid-spacing-pixels+
   #:+grid-major-interval+
   #:+maximum-grid-lines-per-axis+
   #:grid-level
   #:grid-level-spacing
   #:grid-level-major-interval
   #:compute-grid-level
   #:visible-grid-range))
