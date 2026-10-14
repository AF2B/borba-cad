(in-package #:borba-cad/domain)

(defstruct (text-entity (:include entity)
                        (:conc-name text-)
                        (:constructor %make-text (id
                                                  layer-id
                                                  bounds
                                                  x
                                                  y
                                                  string
                                                  height
                                                  angle))
                        (:copier nil))
  "A single-line text annotation laid out on a fixed-advance grid (see geometry text extents).

X: x of the baseline start (insertion point).
Y: y of the baseline start.
STRING: the text, one character cell per code point.
HEIGHT: text height (em size) in millimetres.
ANGLE: direction of the baseline in radians, counter-clockwise from +x, in [0, 2 pi)."
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (string "" :type string :read-only t)
  (height 1d0 :type double-float :read-only t)
  (angle 0d0 :type double-float :read-only t))

(defun printable-text-p (string)
  "Return true when STRING is non-empty, at most +MAXIMUM-TEXT-LENGTH+ characters long and
free of control characters.

STRING: the candidate text."
  (and (stringp string)
       (<= 1 (length string) +maximum-text-length+)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code 32) (/= code 127))))
              string)))

(defun text-box-bounds (x
                        y
                        character-count
                        height
                        angle)
  "Return the nominal bounds of a text line.

X: x of the baseline start.
Y: y of the baseline start.
CHARACTER-COUNT: number of characters.
HEIGHT: text height.
ANGLE: direction of the baseline in radians."
  (geo:oriented-box-bounds x
                           y
                           (geo:text-width character-count height)
                           (* geo:+text-ascent-ratio+ height)
                           (* geo:+text-descent-ratio+ height)
                           angle))

(defun make-text (&key id
                       layer-id
                       x
                       y
                       string
                       height
                       (angle 0d0))
  "Create a validated text annotation. Signals INVALID-ENTITY when an invariant is violated.

ID: positive entity id.
LAYER-ID: id of the layer.
X: x of the insertion point, in millimetres.
Y: y of the insertion point.
STRING: non-empty single-line text without control characters; copied.
HEIGHT: text height (em size), above the length tolerance.
ANGLE: direction of the baseline in radians, counter-clockwise from +x; any finite angle, kept
reduced to [0, 2 pi)."
  (require-identifiers :text
                       id
                       layer-id)
  (require-coordinate :text
                      "x"
                      x)
  (require-coordinate :text
                      "y"
                      y)
  (require-positive-length :text
                           "height"
                           height)
  (require-angle :text
                 "angle"
                 angle)
  (unless (printable-text-p string)
    (invalid-entity-failure :text
                            "the text must be 1 to ~d characters without control characters"
                            +maximum-text-length+))
  (let* ((copy (coerce (copy-seq string) '(simple-array character (*))))
         (turn (geo:normalize-angle angle))
         (bounds (text-box-bounds x
                                  y
                                  (length copy)
                                  height
                                  turn)))
    (require-bounds-in-range :text bounds)
    (%make-text id
                layer-id
                bounds
                x
                y
                copy
                height
                turn)))

(defun update-text (text
                    &key (id (entity-id text))
                         (layer-id (entity-layer-id text))
                         (x (text-x text))
                         (y (text-y text))
                         (string (text-string text))
                         (height (text-height text))
                         (angle (text-angle text)))
  "Return a validated copy of TEXT with the given fields replaced.

TEXT: the text entity to copy.
ID: replacement id.
LAYER-ID: replacement layer id.
X: replacement insertion x.
Y: replacement insertion y.
STRING: replacement text.
HEIGHT: replacement height.
ANGLE: replacement baseline direction in radians."
  (make-text :id id
             :layer-id layer-id
             :x x
             :y y
             :string string
             :height height
             :angle angle))

(defmethod entity-kind ((text text-entity))
  :text)

(defmethod entity-pick-distance ((text text-entity)
                                 x
                                 y)
  (geo:point-oriented-box-distance x
                                   y
                                   (text-x text)
                                   (text-y text)
                                   (geo:text-width (length (text-string text)) (text-height text))
                                   (* geo:+text-ascent-ratio+ (text-height text))
                                   (* geo:+text-descent-ratio+ (text-height text))
                                   (text-angle text)))

(defmethod entity-translated ((text text-entity)
                              dx
                              dy)
  (update-text text
               :x (+ (text-x text) dx)
               :y (+ (text-y text) dy)))

(defmethod entity-snap-points ((text text-entity))
  (list (list (text-x text) (text-y text) :endpoint)))

(defmethod entity-properties ((text text-entity))
  (list (make-property :x
                       (i18n:tr "X")
                       :coordinate
                       (text-x text)
                       t)
        (make-property :y
                       (i18n:tr "Y")
                       :coordinate
                       (text-y text)
                       t)
        (make-property :string
                       (i18n:tr "Text")
                       :text
                       (text-string text)
                       t)
        (make-property :height
                       (i18n:tr "Height")
                       :length
                       (text-height text)
                       t)
        (make-property :angle
                       (i18n:tr "Angle")
                       :angle
                       (text-angle text)
                       t)))

(defmethod entity-derived-values ((text text-entity))
  (list (make-derived-value (i18n:tr "Width")
                            :length
                            (geo:text-width (length (text-string text)) (text-height text)))))

(defmethod entity-with-property ((text text-entity)
                                 key
                                 value)
  (ecase key
    (:x (update-text text :x value))
    (:y (update-text text :y value))
    (:string (update-text text :string value))
    (:height (update-text text :height value))
    (:angle (update-text text :angle value))))

(defmethod entity-with-layer ((text text-entity)
                              layer-id)
  (update-text text :layer-id layer-id))

(defmethod entity-with-identity ((text text-entity)
                                 id
                                 layer-id)
  (update-text text
               :id id
               :layer-id layer-id))

(defmethod entity-touches-bounds-p ((text text-entity)
                                    bounds)
  (geo:oriented-box-touches-bounds-p (text-x text)
                                     (text-y text)
                                     (geo:text-width (length (text-string text)) (text-height text))
                                     (* geo:+text-ascent-ratio+ (text-height text))
                                     (* geo:+text-descent-ratio+ (text-height text))
                                     (text-angle text)
                                     bounds))

(defmethod entity-grips ((text text-entity))
  (let ((width (geo:text-width (length (text-string text)) (text-height text)))
        (angle (text-angle text)))
    (list (make-grip :move
                     (text-x text)
                     (text-y text)
                     :move)
          (make-grip :angle
                     (+ (text-x text) (* width (cos angle)))
                     (+ (text-y text) (* width (sin angle)))
                     :point))))

(defmethod entity-with-grip ((text text-entity)
                             grip
                             x
                             y)
  (ecase (grip-id grip)
    (:move (moved-by-grip text
                          grip
                          x
                          y))
    (:angle (update-text text :angle (atan (- y (text-y text)) (- x (text-x text)))))))
