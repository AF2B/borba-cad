;;;; Command-line tools behind the Makefile targets that are not builds or tests: the standalone
;;;; executable, the log summary, the shipped example and the API reference.

(in-package #:borba-cad/scripts)

(require :sb-introspect)

;;; Dependencies

(defun install-dependencies ()
  "Download and compile the third-party systems every project system needs, then list each
one with the Quicklisp release it came from, so the exact versions in use are on record."
  (let ((names '()))
    (dolist (system *project-systems*)
      (dolist (dependency (third-party-dependencies system))
        (pushnew dependency names :test #'string=)))
    (setf names (sort names #'string<))
    (with-quiet-progress
      (apply #'quickload-quietly names))
    (format t "~&Quicklisp dist ~a~%"
            (uiop:symbol-call '#:ql-dist '#:version (uiop:symbol-call '#:ql-dist '#:find-dist "quicklisp")))
    (dolist (name names)
      (let ((system (uiop:symbol-call '#:ql-dist '#:find-system name)))
        (format t "  ~22a ~a~%" name
                (if system
                    (uiop:symbol-call '#:ql-dist '#:name (uiop:symbol-call '#:ql-dist '#:release system))
                    "(not from Quicklisp)"))))))

;;; Standalone executable

(defun build-executable (&key (output "build/borba-cad"))
  "Load the window program and save it as a standalone executable (an SBCL image with the
runtime options baked in). Exits the process when the image has been written.

OUTPUT: path of the executable, relative to the project root."
  (let ((path (project-path output)))
    (ensure-directories-exist path)
    (with-quiet-progress
      (apply #'quickload-quietly (third-party-dependencies "borba-cad/platform"))
      (asdf:load-system "borba-cad/platform"))
    ;; cl-freetype2 creates its library handle while it loads; the handle dangles in a saved
    ;; image, so the program opens its own at startup.
    (setf (symbol-value (find-symbol "*LIBRARY*" "FREETYPE2")) nil)
    (sb-ext:gc :full t)
    (format t "~&Saving ~a~%" (uiop:native-namestring path))
    (finish-output)
    (sb-ext:save-lisp-and-die path
                              :executable t
                              :toplevel (lambda ()
                                          (uiop:symbol-call '#:borba-cad/platform
                                                            '#:executable-main))
                              :save-runtime-options t)))

;;; Logs

(defun logs-summary (&key directory)
  "Print the summary of the log files in DIRECTORY, or fail when there are none.

DIRECTORY: log directory; defaults to var/log/ of the project."
  (quickload-quietly "borba-cad")
  (let* ((log-directory (if directory
                            (uiop:ensure-directory-pathname directory)
                            (project-path "var/log/")))
         (files (uiop:symbol-call '#:borba-cad/observability '#:discover-log-files log-directory)))
    (unless files
      (fail "no log files in ~a; run the program first (make run)" (uiop:native-namestring log-directory)))
    (uiop:symbol-call '#:borba-cad/observability
                      '#:format-log-summary
                      (uiop:symbol-call '#:borba-cad/observability '#:summarize-log-files files)
                      *standard-output*)))

;;; Example

(defun make-example (&key (output "examples/floor-plan.bcad"))
  "Regenerate the shipped example drawing by running the demonstration scenario headlessly.

OUTPUT: path of the example, relative to the project root."
  (quickload-quietly "borba-cad")
  (let* ((config (uiop:symbol-call '#:borba-cad/base '#:default-config :project-root *project-root*))
         (entities (uiop:symbol-call '#:borba-cad/app
                                     '#:write-demo-drawing
                                     config
                                     (uiop:native-namestring (project-path output)))))
    (format t "~&Wrote ~a with ~d entities.~%" output entities)))

;;; API reference

(defun reference-packages ()
  "Return the project's packages that make up the public API, sorted by name."
  (sort (remove-if-not (lambda (package)
                         (let ((name (package-name package)))
                           (and (own-system-p (string-downcase name))
                                (not (member name *undocumented-package-names* :test #'string=)))))
                       (list-all-packages))
        #'string< :key #'package-name))

(defun symbol-kind (symbol)
  "Return the words that classify the definition(s) behind an exported symbol.

SYMBOL: an exported symbol."
  (cond ((macro-function symbol) "macro")
        ((and (fboundp symbol) (typep (fdefinition symbol) 'generic-function)) "generic function")
        ((fboundp symbol) "function")
        ((and (find-class symbol nil) (subtypep symbol 'condition)) "condition")
        ((find-class symbol nil) "type")
        ((constantp symbol) "constant")
        ((boundp symbol) "variable")
        (t "symbol")))

(defun signature-line (symbol)
  "Return the call form of a function or macro as text, or NIL for other symbols.

SYMBOL: an exported symbol."
  (when (fboundp symbol)
    (let* ((function (or (macro-function symbol) (fdefinition symbol)))
           (lambda-list (sb-introspect:function-lambda-list function))
           (*package* (find-package :keyword))
           (*print-case* :downcase))
      (format nil "(~a~{ ~a~})" (string-downcase (symbol-name symbol))
              (mapcar (lambda (item) (string-downcase (princ-to-string item))) lambda-list)))))

(defun symbol-documentation-text (symbol)
  "Return the docstring of an exported symbol, whichever kind of definition it names.

SYMBOL: an exported symbol."
  (or (and (fboundp symbol) (documentation symbol 'function))
      (and (find-class symbol nil) (documentation symbol 'type))
      (and (boundp symbol) (documentation symbol 'variable))
      "(undocumented)"))

(defun parameter-line-p (line)
  "Return the position of the colon when LINE starts with an upper-case parameter name and a
colon (the project's docstring convention), otherwise NIL.

LINE: one line of a docstring."
  (let ((colon (position #\: line)))
    (and colon
         (plusp colon)
         (< (1+ colon) (length line))
         (char= #\Space (char line (1+ colon)))
         (every (lambda (character)
                  (or (upper-case-p character) (digit-char-p character) (find character "-*+")))
                (subseq line 0 colon))
         colon)))

(defun markdown-docstring (documentation)
  "Return a docstring as Markdown: each parameter line becomes a list item.

DOCUMENTATION: the docstring."
  (with-output-to-string (stream)
    (dolist (line (uiop:split-string documentation :separator '(#\Newline)))
      (let ((colon (parameter-line-p line)))
        (if colon
            (format stream "- `~a`:~a~%" (subseq line 0 colon) (subseq line (1+ colon)))
            (format stream "~a~%" line))))))

(defun write-package-reference (package
                                stream)
  "Write the Markdown reference of one package.

PACKAGE: the package.
STREAM: the output stream."
  (format stream "## ~a~%~%" (package-name package))
  (let ((package-documentation (documentation package t)))
    (when package-documentation
      (format stream "~a~%~%" package-documentation)))
  (let ((symbols '()))
    (do-external-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (push symbol symbols)))
    (dolist (symbol (sort symbols #'string< :key #'symbol-name))
      (format stream "### `~a` (~a)~%~%" (string-downcase (symbol-name symbol)) (symbol-kind symbol))
      (let ((signature (signature-line symbol)))
        (when signature
          (format stream "```lisp~%~a~%```~%~%" signature)))
      (format stream "~a~%" (markdown-docstring (symbol-documentation-text symbol))))))

(defun generate-api-reference (&key (output "docs/reference.md"))
  "Write the API reference, generated from the docstrings of the exported symbols.

OUTPUT: path of the Markdown file, relative to the project root."
  (dolist (system *lint-documented-systems*)
    (with-quiet-progress
      (apply #'quickload-quietly (third-party-dependencies system))
      (asdf:load-system system)))
  (with-open-file (stream (project-path output)
                          :direction :output
                          :if-exists :supersede
                          :external-format :utf-8)
    (format stream "# Borba CAD API reference~%~%")
    (format stream "Generated by `make docs` from the docstrings of the exported symbols; do not edit by hand.~%~%")
    (dolist (package (reference-packages))
      (write-package-reference package stream)))
  (format t "~&Wrote ~a~%" output))

;;; Interactive session

(defparameter *repl-nicknames*
  '(("base" "BORBA-CAD/BASE")
    ("sbcl" "BORBA-CAD/SBCL-SUPPORT")
    ("geo" "BORBA-CAD/GEOMETRY")
    ("dom" "BORBA-CAD/DOMAIN")
    ("view" "BORBA-CAD/VIEWPORT")
    ("persist" "BORBA-CAD/PERSISTENCE")
    ("obs" "BORBA-CAD/OBSERVABILITY")
    ("scene" "BORBA-CAD/SCENE")
    ("ui" "BORBA-CAD/UI")
    ("app" "BORBA-CAD/APP"))
  "Local nicknames available in the REPL package, as (nickname package-name).")

(defun start-repl (&key (systems '("borba-cad")))
  "Load SYSTEMS, create the BORBA-CAD/USER package with a short nickname for every module and
make it current, so that an interactive session starts ready to explore the program.

SYSTEMS: names of the systems to load."
  (with-quiet-progress
    (dolist (system systems)
      (apply #'quickload-quietly (third-party-dependencies system))
      (asdf:load-system system)))
  (let ((nicknames (remove-if-not (lambda (entry) (find-package (second entry))) *repl-nicknames*)))
    (unless (find-package "BORBA-CAD/USER")
      (let ((package (make-package "BORBA-CAD/USER" :use '("COMMON-LISP"))))
        (loop for (nickname package-name) in nicknames
              do (sb-ext:add-package-local-nickname (string-upcase nickname) package-name package))))
    (setf *package* (find-package "BORBA-CAD/USER"))
    (format t "~&Borba CAD REPL. Loaded: ~{~a~^, ~}.~%Nicknames: ~{~a~^ ~}.~%"
            systems
            (mapcar #'first nicknames))
    (format t "Try: (dom:make-drawing)   (app:make-app (base:default-config))   (describe 'dom:make-line)~%~%")))
