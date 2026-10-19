(defpackage #:borba-cad/shapes
  (:use #:cl)
  (:local-nicknames (#:base #:borba-cad/base)
                    (#:i18n #:borba-cad/i18n)
                    (#:geo #:borba-cad/geometry)
                    (#:dom #:borba-cad/domain)
                    (#:persist #:borba-cad/persistence)
                    (#:sbcl #:borba-cad/sbcl-support))
  (:documentation "The library of shapes that can be placed in a drawing: ornaments such as the acanthus leaves of
ironwork, kept by the trade they belong to and the group they are in, made by procedures or read from SVG files, and
turned into the blocks that the drawing places.")
  (:export
   ;; Curves
   #:flatten-cubic
   #:flatten-quadratic
   #:+curve-tolerance+
   ;; Shapes, groups and areas
   #:contour
   #:make-contour
   #:contour-points
   #:contour-closed
   #:shape
   #:make-shape
   #:shape-id
   #:shape-name
   #:shape-name-argument
   #:shape-title
   #:shape-contours
   #:shape-height
   #:shape-width
   #:shape-preview-polylines
   #:group
   #:make-group
   #:group-id
   #:group-title
   #:group-shapes
   #:area
   #:make-area
   #:area-id
   #:area-title
   #:area-groups
   ;; The catalog
   #:built-in-areas
   #:all-areas
   #:find-shape
   #:shape-block-name
   #:shape-block-definition
   ;; SVG
   #:parse-svg-path
   #:read-svg-contours
   #:read-svg-shape
   #:load-user-areas))
