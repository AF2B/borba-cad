;;;; An init file for Borba CAD. Copy it to ~/.config/borba-cad/init.lisp (on Windows, %APPDATA%\BorbaCAD\init.lisp) and
;;;; the program runs it every time it starts; "Settings: Run the init file again" in the command palette (Ctrl+K) runs
;;;; it again without leaving the program, and the option --no-init starts without it.
;;;;
;;;; It is Lisp, like the console. What it can do is define: commands, keys that run them, and hooks that run when
;;;; something happens. A command can draw and edit with everything the console knows, and is one step of undo.
;;;; Drawing at the top of the file, outside a command, is refused.

;;; A command: a name, what it does, and the commands to run. It is listed in the command palette, Ctrl+K.

(defcommand "Draw a fence post"
  "A post 50 mm by 1 m, with a cap, with its foot at the origin"
  (rect-size 0 0 50 1000)
  (rect-size -10 1000 70 20))

(defcommand "Draw a row of posts"
  "Five posts, 1.2 m apart, on a layer of their own"
  (layer "posts" :color :orange)
  (dotimes (index 5)
    (rect-size (* index 1200) 0 50 1000)))

;;; A command is plain Lisp, so it can ask the drawing questions too.

(defcommand "How many things?"
  "Says how many entities the drawing has"
  (say (format nil "~d entities" (entity-count))))

;;; Keys: Ctrl or Alt with a letter, or a function key. They run a command of yours, or any command the palette lists.

(bind-key "Ctrl+Alt+P" "Draw a fence post")
(bind-key "F6" "View: Zoom to fit")

;;; Hooks: :startup, :drawing-opened, :before-save and :after-save.

(add-hook :startup
          (lambda ()
            (say "Init file ready. Ctrl+K opens the command palette.")))

(add-hook :drawing-opened
          (lambda ()
            (layer "notes" :color :green)))
