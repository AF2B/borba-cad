(in-package #:borba-cad/ui)

;;;; Icons drawn from strokes and fills on a 24 by 24 grid, so that they stay crisp at any size
;;;; and take the color of the text beside them. Each icon is a list of shapes:
;;;;   (:line x1 y1 x2 y2)        a stroke
;;;;   (:polyline x y x y ...)    connected strokes
;;;;   (:loop x y x y ...)        a closed outline
;;;;   (:circle cx cy r)          a ring
;;;;   (:disc cx cy r)            a filled circle
;;;;   (:fill x y x y ...)        a filled convex polygon
;;;;   (:arc cx cy r from to)     a stroked arc, in degrees clockwise from the right
;;;;   (:rrect x y w h r)         a rounded rectangle outline
;;;;   (:frrect x y w h r)        a filled rounded rectangle

(defconstant +icon-grid+ 24f0
  "Side of the grid that icons are drawn on.")

(defconstant +icon-stroke+ 1.8f0
  "Stroke width of an icon, in grid units.")

(defconstant +minimum-icon-stroke-pixels+ 1.2f0
  "Thinnest stroke an icon is drawn with, in pixels, so that small icons stay visible.")

(defun mirrored-icon (shapes)
  "Return SHAPES flipped left to right on the grid, for icons that come in pairs.

SHAPES: the shape list of an icon made only of lines, polylines, loops and fills."
  (mapcar (lambda (shape)
            (cons (first shape)
                  (loop for (x y) on (rest shape) by #'cddr
                        append (list (- 24 x) y))))
          shapes))

(defparameter *undo-shapes*
  '((:polyline 8 4.5 4 8.5 8 12.5)
    (:polyline 4.5 8.5 14 8.5 19.5 12.75 14 17.5 9 17.5))
  "A hook arrow pointing back.")

(defparameter *icons*
  (list
   (cons :select '((:fill 6 3.5 6 19 10 15.5)
                   (:fill 6 3.5 10 15.5 18 14.5)
                   (:fill 10 15.5 12.6 14.5 18 14.5)
                   (:fill 10 15.5 13 21.5 15.5 20.3)
                   (:fill 10 15.5 15.5 20.3 12.6 14.5)))
   (cons :line '((:line 5 19 19 5)
                 (:disc 5 19 2.4)
                 (:disc 19 5 2.4)))
   (cons :rectangle '((:rrect 4.5 6.5 15 11 2)))
   (cons :circle '((:circle 12 12 7.5)))
   (cons :polyline '((:polyline 4 17 9 7 14 15 20 6)
                     (:disc 4 17 2)
                     (:disc 9 7 2)
                     (:disc 14 15 2)
                     (:disc 20 6 2)))
   (cons :arc '((:arc 12 17 8.5 190 350)
                (:disc 3.6 15.5 1.9)
                (:disc 20.4 15.5 1.9)))
   (cons :ellipse '((:loop 12 5.5 16.6 6.4 19.6 9 20.5 12 19.6 15 16.6 17.6
                           12 18.5 7.4 17.6 4.4 15 3.5 12 4.4 9 7.4 6.4)))
   (cons :polygon '((:loop 12 3.8 19.1 8 19.1 16 12 20.2 4.9 16 4.9 8)))
   (cons :text '((:polyline 5 20 12 4 19 20)
                 (:line 8 14.5 16 14.5)))
   (cons :dimension '((:line 4 12 20 12)
                      (:fill 4 12 8.5 9.5 8.5 14.5)
                      (:fill 20 12 15.5 9.5 15.5 14.5)
                      (:line 4 6 4 18)
                      (:line 20 6 20 18)))
   (cons :hatch '((:rrect 4.5 5.5 15 13 1.5)
                  (:line 8 18 16 6)
                  (:line 12 18 18 9)
                  (:line 5.5 15 10 8)))
   (cons :solid-fill '((:fill 5 6 19 6 19 18 5 18)))
   (cons :radial-dimension '((:circle 10.5 13.5 6.5)
                             (:line 10.5 13.5 20 4.5)
                             (:disc 10.5 13.5 1.2)
                             (:fill 15.6 8.9 14 13 17.5 9.8)))
   (cons :insert '((:rrect 3.5 3.5 7 7 1)
                   (:rrect 13.5 13.5 7 7 1)
                   (:polyline 10.5 7 17 7 17 13.5)
                   (:fill 17 13.5 14.8 10.8 19.2 10.8)))
   (cons :make-block '((:rrect 4.5 4.5 15 15 1.5)
                       (:line 12 8 12 16)
                       (:line 8 12 16 12)))
   (cons :explode '((:rrect 8.5 8.5 7 7 1)
                    (:line 4 4 7 7)
                    (:line 20 4 17 7)
                    (:line 4 20 7 17)
                    (:line 20 20 17 17)))
   (cons :trim '((:line 15 4 15 20)
                 (:line 3 12 15 12)
                 (:line 18 12 19 12)
                 (:line 21 12 22 12)))
   (cons :extend '((:line 19 4 19 20)
                   (:line 3 12 12 12)
                   (:line 14.5 12 15.5 12)
                   (:fill 19 12 16 9.8 16 14.2)))
   (cons :offset '((:line 4 17 17 4)
                   (:line 8 21 21 8)))
   (cons :fillet '((:line 6 4 6 14)
                   (:line 12 20 20 20)
                   (:arc 12 14 6 90 180)))
   (cons :radius-dimension '((:circle 10.5 13.5 6.5)
                             (:line 10.5 13.5 20 4.5)
                             (:disc 10.5 13.5 1.2)
                             (:fill 15.6 8.9 14 13 17.5 9.8)))
   (cons :diameter-dimension '((:circle 12 12 7)
                               (:line 5 19 19 5)
                               (:fill 5 19 5 14.5 9.5 19)
                               (:fill 19 5 14.5 5 19 9.5)))
   (cons :angular-dimension '((:line 4 20 20 20)
                              (:line 4 20 16 5)
                              (:arc 4 20 10 311 360)
                              (:fill 14 20 14.8 16.5 17.5 18.5)))
   (cons :new '((:loop 6 3.5 14 3.5 19 8.5 19 20.5 6 20.5)
                (:polyline 14 3.5 14 8.5 19 8.5)
                (:line 12.5 12.5 12.5 17.5)
                (:line 10 15 15 15)))
   (cons :open '((:loop 3 6 9.5 6 11.5 8.5 21 8.5 21 19.5 3 19.5)
                 (:line 3 12.5 21 12.5)))
   (cons :save '((:loop 4 4 17 4 20 7 20 20 4 20)
                 (:loop 8 4 8 9 16 9 16 4)
                 (:rrect 7 13 10 7 1)))
   (cons :save-as '((:loop 3.5 4 15 4 17.5 6.5 17.5 16.5 3.5 16.5)
                    (:loop 6.5 4 6.5 8 13 8 13 4)
                    (:line 18.5 14.5 18.5 21.5)
                    (:line 15 18 22 18)))
   (cons :undo *undo-shapes*)
   (cons :redo (mirrored-icon *undo-shapes*))
   (cons :grid '((:rrect 4 4 16 16 2)
                 (:line 9.3 4 9.3 20)
                 (:line 14.7 4 14.7 20)
                 (:line 4 9.3 20 9.3)
                 (:line 4 14.7 20 14.7)))
   (cons :snap '((:circle 12 12 5)
                 (:line 12 3 12 8)
                 (:line 12 16 12 21)
                 (:line 3 12 8 12)
                 (:line 16 12 21 12)))
   (cons :object-snap '((:rrect 5 5 14 14 1.5)
                        (:disc 5 5 2.8)
                        (:disc 19 19 2.8)))
   (cons :unit '((:rrect 3 8 18 8 1.5)
                 (:line 7 8 7 11)
                 (:line 11 8 11 12)
                 (:line 15 8 15 11)
                 (:line 19 8 19 12)))
   (cons :extents '((:polyline 4 9 4 4 9 4)
                    (:polyline 15 4 20 4 20 9)
                    (:polyline 20 15 20 20 15 20)
                    (:polyline 9 20 4 20 4 15)
                    (:rrect 9.5 9.5 5 5 1)))
   (cons :perf '((:polyline 4 17 9 11 13 14 20 6)
                 (:line 4 20.5 20 20.5)))
   (cons :layers '((:loop 12 4 21 9 12 14 3 9)
                   (:polyline 3 13 12 18 21 13)))
   (cons :eye '((:polyline 2.5 12 7 6.5 12 5 17 6.5 21.5 12)
                (:polyline 2.5 12 7 17.5 12 19 17 17.5 21.5 12)
                (:disc 12 12 3)))
   (cons :eye-off '((:polyline 2.5 12 7 6.5 12 5 17 6.5 21.5 12)
                    (:polyline 2.5 12 7 17.5 12 19 17 17.5 21.5 12)
                    (:line 4 4 20 20)))
   (cons :lock '((:rrect 5 11 14 10 2)
                 (:arc 12 9 4 180 360)
                 (:line 8 9 8 11)
                 (:line 16 9 16 11)
                 (:disc 12 16 1.4)))
   (cons :unlock '((:rrect 5 11 14 10 2)
                   (:arc 12 9 4 180 300)
                   (:line 8 9 8 11)
                   (:disc 12 16 1.4)))
   (cons :plus '((:line 12 5 12 19)
                 (:line 5 12 19 12)))
   (cons :minus '((:line 5 12 19 12)))
   (cons :close '((:line 6 6 18 18)
                  (:line 18 6 6 18)))
   (cons :check '((:polyline 5 12.5 10 17.5 19 7)))
   (cons :trash '((:line 5 7 19 7)
                  (:polyline 10 7 10 4.5 14 4.5 14 7)
                  (:loop 7 7 8 20 16 20 17 7)
                  (:line 10.5 11 10.5 16)
                  (:line 13.5 11 13.5 16)))
   (cons :edit '((:loop 4 20 4.5 15.5 16 4 20 8 8.5 19.5)))
   (cons :pencil '((:loop 4 20 4.5 15.5 16 4 20 8 8.5 19.5)
                   (:line 12.5 7.5 16.5 11.5)))
   (cons :caret '((:fill 7 4 18 12 7 20)))
   (cons :more '((:disc 5.5 12 1.8)
                 (:disc 12 12 1.8)
                 (:disc 18.5 12 1.8)))
   (cons :sun '((:circle 12 12 4)
                (:line 19.0 12.0 21.6 12.0)
                (:line 16.9 16.9 18.8 18.8)
                (:line 12.0 19.0 12.0 21.6)
                (:line 7.1 16.9 5.2 18.8)
                (:line 5.0 12.0 2.4 12.0)
                (:line 7.1 7.1 5.2 5.2)
                (:line 12.0 5.0 12.0 2.4)
                (:line 16.9 7.1 18.8 5.2)))
   (cons :moon '((:loop 11.0 3.6 8.4 4.3 6.2 5.8 4.6 7.8 3.7 10.3 3.6 12.9 4.3 15.5 5.7 17.7 7.8 19.4 10.2 20.3
                 12.9 20.5 15.4 19.8 17.6 18.4 19.3 16.3 20.3 13.9 18.5 14.8 16.5 15.2 14.5 15.0 12.6 14.2
                 11.0 12.9 9.9 11.2 9.3 9.3 9.3 7.2 9.8 5.3)))
   (cons :globe '((:circle 12 12 8.5)
                  (:line 3.5 12 20.5 12)
                  (:polyline 12 3.5 9 7.5 8 12 9 16.5 12 20.5)
                  (:polyline 12 3.5 15 7.5 16 12 15 16.5 12 20.5)))
   (cons :palette '((:circle 12 12 8)
                    (:disc 8.5 10 1.6)
                    (:disc 12 7.5 1.6)
                    (:disc 15.5 10 1.6)))
   (cons :file '((:loop 6 3.5 14 3.5 19 8.5 19 20.5 6 20.5)
                 (:polyline 14 3.5 14 8.5 19 8.5)))
   (cons :folder '((:loop 3 6 9.5 6 11.5 8.5 21 8.5 21 19.5 3 19.5)))
   (cons :folder-up '((:polyline 7 11 12 6 17 11)
                      (:line 12 6 12 19)))
   (cons :new-folder '((:loop 3 6 9.5 6 11.5 8.5 21 8.5 21 19.5 3 19.5)
                       (:line 12 11.5 12 16.5)
                       (:line 9.5 14 14.5 14)))
   (cons :home '((:polyline 4 11 12 4 20 11)
                 (:polyline 6 10 6 20 18 20 18 10)
                 (:rrect 10 14 4 6 0.5)))
   (cons :clock '((:circle 12 12 8)
                  (:polyline 12 7 12 12 15.5 14)))
   (cons :chevron-right '((:polyline 9 5 16 12 9 19)))
   (cons :chevron-down '((:polyline 5 9 12 16 19 9)))
   (cons :search '((:circle 10.5 10.5 6)
                   (:line 15 15 20 20)))
   (cons :info '((:circle 12 12 8.5)
                 (:line 12 11 12 17)
                 (:disc 12 7.6 1.1)))
   (cons :warning '((:loop 12 4 21 19.5 3 19.5)
                    (:line 12 10 12 14.5)
                    (:disc 12 17 1.1)))
   (cons :console '((:rrect 3 5 18 14 2)
                    (:polyline 7 10 10 12.5 7 15)
                    (:line 12 15.5 16 15.5)))
   (cons :copy '((:rrect 8.5 8.5 11 11 2)
                 (:polyline 15.5 8.5 15.5 5 5.5 5 5.5 15 8.5 15)))
   (cons :paste '((:rrect 5 6 14 15 2)
                  (:rrect 9 3.5 6 4 1)))
   (cons :import '((:line 12 4 12 15)
                   (:polyline 7.5 10.5 12 15 16.5 10.5)
                   (:polyline 4 15 4 20 20 20 20 15)))
   (cons :export '((:line 12 15 12 4)
                   (:polyline 7.5 8.5 12 4 16.5 8.5)
                   (:polyline 4 15 4 20 20 20 20 15)))
   (cons :move '((:line 12 3 12 21)
                 (:line 3 12 21 12)
                 (:polyline 9 6 12 3 15 6)
                 (:polyline 9 18 12 21 15 18)
                 (:polyline 6 9 3 12 6 15)
                 (:polyline 18 9 21 12 18 15)))
   (cons :rotate '((:arc 12 12 7 20 320)
                   (:polyline 17 4 18.5 8.5 14 9.5)))
   (cons :mirror '((:line 12 3 12 21)
                   (:loop 4 8 9 12 4 16)
                   (:fill 20 8 15 12 20 16)))
   (cons :scale '((:loop 4 14 4 20 10 20 10 14)
                  (:polyline 4 4 20 4 20 20)
                  (:polyline 8 16 20 4)
                  (:polyline 14 4 20 4 20 10)))
   (cons :ruler '((:loop 3 8 21 8 21 16 3 16)
                  (:line 7 8 7 12)
                  (:line 11 8 11 11)
                  (:line 15 8 15 12)
                  (:line 19 8 19 11)))
   (cons :angle '((:polyline 4 20 20 20)
                  (:line 4 20 16 6)
                  (:arc 4 20 9 311 360)))
   (cons :area '((:loop 4 17 7 5 20 8 18 19)
                 (:line 10 8 15 17)
                 (:line 14 7 17 13))))
  "Every icon by name.")

(defun icon-names ()
  "Return the names of the available icons."
  (mapcar #'car *icons*))

(defun icon-shapes (name)
  "Return the shape list of the icon NAME, or NIL when there is none.

NAME: an icon keyword."
  (cdr (assoc name *icons*)))

(defun icon-thickness (scale)
  "Return the stroke width in pixels of an icon drawn at SCALE pixels per grid unit.

SCALE: pixels per grid unit."
  (max +minimum-icon-stroke-pixels+ (* +icon-stroke+ scale)))

(defun draw-stroke (list
                    x1
                    y1
                    x2
                    y2
                    thickness
                    color)
  "Draw a straight stroke with round ends.

LIST: the draw list.
X1: start x.
Y1: start y.
X2: end x.
Y2: end y.
THICKNESS: stroke width in pixels.
COLOR: an RGBA."
  (scene:add-thick-line list
                        x1
                        y1
                        x2
                        y2
                        thickness
                        color)
  (when (> thickness 1.8f0)
    (scene:add-disc list
                    x1
                    y1
                    (* 0.5f0 thickness)
                    color)
    (scene:add-disc list
                    x2
                    y2
                    (* 0.5f0 thickness)
                    color)))

(defun draw-point-chain (list
                         points
                         closed
                         thickness
                         color)
  "Draw strokes through a list of points, with round joints.

LIST: the draw list.
POINTS: list of (x . y) conses in pixels.
CLOSED: true to join the last point to the first.
THICKNESS: stroke width in pixels.
COLOR: an RGBA."
  (loop for (first second) on (if closed (append points (list (first points))) points)
        while second
        do (draw-stroke list
                        (car first)
                        (cdr first)
                        (car second)
                        (cdr second)
                        thickness
                        color)))

(defun draw-icon-shape (list
                        shape
                        x
                        y
                        scale
                        color)
  "Draw one shape of an icon.

LIST: the draw list.
SHAPE: a shape as described at the top of this file.
X: left edge of the icon in pixels.
Y: top edge of the icon in pixels.
SCALE: pixels per grid unit.
COLOR: an RGBA."
  (let ((thickness (icon-thickness scale)))
    (flet ((px (grid-x) (+ x (* scale grid-x)))
           (py (grid-y) (+ y (* scale grid-y)))
           (points (coordinates)
             (loop for (grid-x grid-y) on coordinates by #'cddr
                   collect (cons (+ x (* scale grid-x)) (+ y (* scale grid-y))))))
      (destructuring-bind (kind &rest arguments) shape
        (ecase kind
          (:line
           (destructuring-bind (x1 y1 x2 y2) arguments
             (draw-stroke list
                          (px x1)
                          (py y1)
                          (px x2)
                          (py y2)
                          thickness
                          color)))
          (:polyline
           (draw-point-chain list
                             (points arguments)
                             nil
                             thickness
                             color))
          (:loop
           (draw-point-chain list
                             (points arguments)
                             t
                             thickness
                             color))
          (:circle
           (destructuring-bind (center-x center-y radius) arguments
             (scene:add-ring list
                             (px center-x)
                             (py center-y)
                             (* scale radius)
                             thickness
                             color)))
          (:disc
           (destructuring-bind (center-x center-y radius) arguments
             (scene:add-disc list
                             (px center-x)
                             (py center-y)
                             (* scale radius)
                             color)))
          (:fill
           (scene:add-polygon list
                              (points arguments)
                              color))
          (:arc
           (destructuring-bind (center-x center-y radius from to) arguments
             (scene:add-arc-stroke list
                                   (px center-x)
                                   (py center-y)
                                   (* scale radius)
                                   (* from (/ (float pi 1f0) 180))
                                   (* to (/ (float pi 1f0) 180))
                                   thickness
                                   color)))
          (:rrect
           (destructuring-bind (rect-x rect-y width height radius) arguments
             (scene:add-rounded-rectangle-outline list
                                                  (+ (px rect-x) (* 0.5f0 thickness))
                                                  (+ (py rect-y) (* 0.5f0 thickness))
                                                  (- (* scale width) thickness)
                                                  (- (* scale height) thickness)
                                                  (* scale radius)
                                                  thickness
                                                  color)))
          (:frrect
           (destructuring-bind (rect-x rect-y width height radius) arguments
             (scene:add-rounded-rectangle list
                                          (px rect-x)
                                          (py rect-y)
                                          (* scale width)
                                          (* scale height)
                                          (* scale radius)
                                          color))))))))

(defun draw-icon (list
                  name
                  x
                  y
                  size
                  color)
  "Draw the icon NAME inside the square whose top-left corner is (X, Y). An unknown name draws
nothing.

LIST: the draw list.
NAME: an icon keyword from ICON-NAMES.
X: left edge in pixels.
Y: top edge in pixels.
SIZE: side of the square in pixels.
COLOR: an RGBA."
  (let ((scale (/ size +icon-grid+)))
    (dolist (shape (icon-shapes name))
      (draw-icon-shape list
                       shape
                       x
                       y
                       scale
                       color))))
