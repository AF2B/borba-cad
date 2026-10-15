;;;; A rosette: twelve ellipses turned a little each around the same center, inside a circle.
;;;; A script is a program, so a loop and a little arithmetic draw what would take many clicks.

(layer "rosette" :color :magenta)
(circle 0 0 (cm 42))
(dotimes (step 12)
  (ellipse 0 0 (cm 40) (cm 12) (* step 15)))
(polygon 0 0 (cm 12) 6)
(fit)
(say "Rosette drawn:" (entity-count) "entities.")
