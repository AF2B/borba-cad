(defpackage #:borba-cad/geometry
  (:use #:cl)
  (:documentation "Computational geometry on world coordinates (double-float millimetres).
No screen, pixel or rendering concepts appear here.")
  (:import-from #:borba-cad/base
                #:+length-tolerance+)
  (:export
   ;; Bounds
   #:bounds
   #:make-bounds
   #:bounds-min-x
   #:bounds-min-y
   #:bounds-max-x
   #:bounds-max-y
   #:bounds-from-corners
   #:bounds-width
   #:bounds-height
   #:bounds-center-x
   #:bounds-center-y
   #:bounds-union
   #:bounds-intersect-p
   #:bounds-contains-bounds-p
   #:bounds-contains-point-p
   #:expand-bounds
   ;; Scalar helpers
   #:distance
   #:normalize-angle
   #:segment-length
   #:segment-angle
   #:snap-coordinate
   ;; Point-to-shape distances
   #:point-segment-distance
   #:closest-point-on-segment
   #:point-circle-outline-distance
   #:point-rectangle-outline-distance
   #:point-polyline-distance
   #:point-bounds-distance
   #:point-oriented-box-distance
   #:bounds-near-point-p
   ;; Clipping
   #:clip-segment-to-bounds
   ;; Crossing
   #:segment-touches-bounds-p
   #:polyline-touches-bounds-p
   #:circle-outline-touches-bounds-p
   #:oriented-box-touches-bounds-p
   ;; Measures
   #:circle-circumference
   #:circle-area
   #:rectangle-area
   #:rectangle-perimeter
   #:polyline-length
   #:polyline-vertex-count
   #:polyline-bounds
   #:circle-bounds
   ;; Text extents
   #:+text-advance-ratio+
   #:+text-ascent-ratio+
   #:+text-descent-ratio+
   #:text-width
   #:oriented-box-bounds
   ;; Aligned dimension layout
   #:+dimension-arrow-length-ratio+
   #:+dimension-arrow-width-ratio+
   #:+dimension-extension-gap-ratio+
   #:+dimension-extension-overshoot-ratio+
   #:+dimension-text-gap-ratio+
   #:dimension-layout
   #:dimension-layout-extension-lines
   #:dimension-layout-dimension-line
   #:dimension-layout-arrowheads
   #:dimension-layout-text-x
   #:dimension-layout-text-y
   #:dimension-layout-text-angle
   #:dimension-layout-text-width
   #:dimension-layout-text-height
   #:dimension-layout-bounds
   #:compute-dimension-layout))
