(in-package #:borba-cad/app)

;;;; Input events. The window system translates its native events into these values; tests build
;;;; them directly. Positions are logical window pixels (double-float). Modifiers are a list
;;;; drawn from :shift, :control and :alt.

(defstruct (pointer-event (:constructor make-pointer-event (action
                                                            button
                                                            x
                                                            y
                                                            &optional modifiers)))
  "A pointer movement or button transition.

ACTION: :move, :down or :up.
BUTTON: :left, :middle or :right for :down and :up; NIL for :move.
X: window x in logical pixels.
Y: window y in logical pixels.
MODIFIERS: modifier keys held."
  (action :move :type keyword :read-only t)
  (button nil :read-only t)
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (modifiers nil :type list :read-only t))

(defstruct (wheel-event (:constructor make-wheel-event (delta
                                                        x
                                                        y
                                                        &optional modifiers)))
  "A mouse-wheel step.

DELTA: notches; positive scrolls away from the user (zoom in).
X: pointer window x.
Y: pointer window y.
MODIFIERS: modifier keys held."
  (delta 0d0 :type double-float :read-only t)
  (x 0d0 :type double-float :read-only t)
  (y 0d0 :type double-float :read-only t)
  (modifiers nil :type list :read-only t))

(defstruct (key-event (:constructor make-key-event (key
                                                    &optional modifiers (action :down))))
  "A key press or release.

KEY: keyword such as :a, :escape, :enter, :backspace, :delete, :space, :left, :home, :f3.
MODIFIERS: modifier keys held.
ACTION: :down or :up."
  (key :a :type keyword :read-only t)
  (modifiers nil :type list :read-only t)
  (action :down :type keyword :read-only t))

(defstruct (text-event (:constructor make-text-event (text)))
  "Text produced by the keyboard layout, one or more code points.

TEXT: the characters."
  (text "" :type string :read-only t))

(defstruct (resize-event (:constructor make-resize-event (width
                                                          height
                                                          &optional (scale 1d0))))
  "The window changed size.

WIDTH: new logical width.
HEIGHT: new logical height.
SCALE: display scale (framebuffer pixels per logical pixel)."
  (width 1d0 :type double-float :read-only t)
  (height 1d0 :type double-float :read-only t)
  (scale 1d0 :type double-float :read-only t))

(defstruct (file-drop-event (:constructor make-file-drop-event (path)))
  "A file was dropped on the window.

PATH: the file's path as the window system gives it."
  (path "" :type string :read-only t))

(defstruct (window-state-event (:constructor make-window-state-event (kind)))
  "The window changed in a way that matters to input.

KIND: :focus-lost (another window took the keyboard; releases of keys and buttons that happen
elsewhere never arrive) or :pointer-left (the pointer left the window)."
  (kind :focus-lost :type keyword :read-only t))

(defstruct (quit-event (:constructor make-quit-event ()))
  "The user asked to close the window.")

(defun modifier-p (modifiers
                   modifier)
  "Return true when MODIFIER is among MODIFIERS.

MODIFIERS: list of modifier keywords.
MODIFIER: :shift, :control or :alt."
  (and (member modifier modifiers) t))
