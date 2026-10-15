;;;; A small house plan, drawn by commands. Run it with More > Run script.
;;;; Lengths are millimetres, so (m 9) is nine metres; angles are degrees.
;;;; The whole script is one step of undo: Ctrl+Z takes the house away.

(display-unit :meter)
(grid-spacing 100)

(defun door (x y width)
  "A door in a vertical wall at X, from Y up: its leaf, open, and the arc it swings through."
  (with-layer ("doors" :color :cyan)
    (line x y (+ x width) y)
    (arc x y width 0 90)))

;; The outer wall, 150 mm thick, and the two partitions with a gap for each door.
(layer "walls" :color :orange)
(rect 0 0 (m 9) (m 6))
(rect 150 150 (- (m 9) 150) (- (m 6) 150))
(line (m 5.5) 150 (m 5.5) (m 1.9))
(line (m 5.5) (m 2.8) (m 5.5) (- (m 6) 150))
(line (m 5.5) (m 3.5) (m 7) (m 3.5))
(line (m 7.9) (m 3.5) (- (m 9) 150) (m 3.5))

(door (m 5.5) (m 1.9) 900)

;; Three windows along the front wall.
(with-layer ("windows" :color :blue)
  (dotimes (index 3)
    (rect-size (+ (m 1) (* index (m 2.5))) 0 (m 1.2) 150)))

;; The names of the rooms.
(with-layer ("text" :color :white)
  (text (m 1.5) (m 3) "Living room" :height 250)
  (text (m 6) (m 1) "Kitchen" :height 250)
  (text (m 6) (m 4.5) "Bedroom" :height 250))

;; The size of the house, written on the drawing.
(with-layer ("dimensions" :color :green)
  (dimension 0 0 (m 9) 0 (m -0.8) :unit :meter :decimals 2)
  (dimension (m 9) 0 (m 9) (m 6) (m 0.8) :unit :meter :decimals 2))

(layer "walls")
(fit)
(say "House plan drawn:" (entity-count) "entities.")
