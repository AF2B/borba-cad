;;;; Static checks that the compiler cannot make. `make lint` runs LINT after the strict build
;;;; has already proved that the code compiles without warnings.
;;;;
;;;; Source rules: ASCII only, no tabs, no trailing blanks, a final newline, lines of at most
;;;; 120 columns, no placeholders, no unsafe reading or evaluation of external data.
;;;; Documentation rule: every exported symbol is documented, and every parameter of an
;;;; exported function or macro is described in its docstring.
;;;; Layout rule: a call to one of the project's own functions with three or more parameters
;;;; puts each parameter on its own line (keyword and value count as one parameter).
;;;; Architecture rule: the headless core never depends on a window, GPU or foreign library.

(in-package #:borba-cad/scripts)

(require :sb-introspect)

(defconstant +maximum-line-length+ 120
  "Longest source line accepted, in columns.")

(defparameter *lint-directories*
  '("src/" "test-support/" "tests/" "platform-tests/" "bench/" "scripts/")
  "Directories whose Lisp files are checked, relative to the project root.")

(defparameter *forbidden-core-dependencies*
  '("cffi" "cl-opengl" "cl-freetype2" "sdl2" "borba-cad/platform" "borba-cad/bench-gui")
  "Systems that the headless core must never depend on, directly or indirectly.")

(defparameter *case-sensitive-markers* '("TODO" "FIXME" "XXX" "HACK")
  "Words that mark unfinished work, matched in upper case only.")

(defparameter *case-insensitive-markers* '("not implemented" "unimplemented" "placeholder" "stub")
  "Phrases that mark unfinished work, matched in any case.")

(defparameter *unsafe-forms* '("ignore-errors" "read-from-string" "(eval " "(cl:read ")
  "Forms that hide failures or evaluate external data.")

(defparameter *lint-exempt-from-scan* '("scripts/lint.lisp")
  "Files that name the markers and forms in order to look for them.")

(defun lint-files ()
  "Return the pathnames of the Lisp files that the source rules apply to."
  (loop for directory in *lint-directories*
        append (directory (merge-pathnames "**/*.lisp" (project-path directory)))))

(defun relative-name (path)
  "Return PATH as a string relative to the project root.

PATH: an absolute pathname inside the project."
  (enough-namestring path *project-root*))

(defun file-lines (path)
  "Return the lines of the file at PATH, decoded as UTF-8.

PATH: the file."
  (with-open-file (stream path :external-format :utf-8)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun ends-with-newline-p (path)
  "Return true when the file at PATH is empty or ends with a newline.

PATH: the file."
  (with-open-file (stream path :element-type '(unsigned-byte 8))
    (let ((length (file-length stream)))
      (or (zerop length)
          (progn (file-position stream (1- length))
                 (= 10 (read-byte stream)))))))

(defun line-violations (path)
  "Return the violations of the per-line source rules in the file at PATH.

PATH: the file."
  (let ((name (relative-name path))
        (exempt (member (relative-name path) *lint-exempt-from-scan* :test #'string=))
        (violations '()))
    (flet ((violation (line-number control &rest arguments)
             (push (format nil "~a:~d: ~?" name line-number control arguments) violations)))
      (loop for line in (handler-case (file-lines path)
                          (error () (push (format nil "~a: is not valid UTF-8" name) violations) nil))
            for number from 1
            do (when (find-if (lambda (character) (> (char-code character) 127)) line)
                 (violation number "non-ASCII character (write it with CODE-CHAR)"))
               (when (find #\Tab line)
                 (violation number "tab character"))
               (when (and (plusp (length line)) (char= #\Space (char line (1- (length line)))))
                 (violation number "trailing blank"))
               (when (> (length line) +maximum-line-length+)
                 (violation number "line has ~d columns; the limit is ~d" (length line) +maximum-line-length+))
               (unless exempt
                 (dolist (form *unsafe-forms*)
                   (when (search form line)
                     (violation number "forbidden form ~a" form)))
                 (dolist (marker *case-sensitive-markers*)
                   (when (search marker line)
                     (violation number "unfinished-work marker ~a" marker)))
                 (dolist (marker *case-insensitive-markers*)
                   (when (search marker line :test #'char-equal)
                     (violation number "unfinished-work marker ~s" marker))))))
    (unless (ends-with-newline-p path)
      (push (format nil "~a: does not end with a newline" name) violations))
    (nreverse violations)))

;;; Layout: one parameter per line

(defparameter *layout-checked-directories* '("src/" "test-support/" "bench/" "scripts/")
  "Directories whose calls must follow the layout rule; test code keeps short fixture calls on
one line, where the point of the line is to read inputs and expectation together.")

(defconstant +parameters-per-line-threshold+ 3
  "Number of parameters from which a call must put each one on its own line.")

(defstruct (source-node (:constructor make-source-node (kind
                                          text
                                          line)))
  "One element of a source file, as far as the layout check needs to know it.

KIND: :atom, :list or :comment.
TEXT: the text of an atom, or the quote prefix of a list.
LINE: the line on which it starts.
CHILDREN: for a list, its elements in order."
  kind
  text
  line
  (children nil))

(defun skip-string-end (text
                        start)
  "Return the index just past the string literal that opens at START.

TEXT: the source.
START: index of the opening double quote."
  (let ((index (1+ start)))
    (loop while (and (< index (length text)) (char/= #\" (char text index)))
          do (when (char= #\\ (char text index))
               (incf index))
             (incf index))
    (1+ index)))

(defun skip-block-comment-end (text
                               start)
  "Return the index just past the block comment that opens at START.

TEXT: the source.
START: index of the opening #|."
  (let ((depth 1)
        (index (+ start 2)))
    (loop while (and (plusp depth) (< index (length text)))
          do (cond ((string= "|#" text :start2 index :end2 (min (length text) (+ index 2)))
                    (decf depth)
                    (incf index 2))
                   ((string= "#|" text :start2 index :end2 (min (length text) (+ index 2)))
                    (incf depth)
                    (incf index 2))
                   (t (incf index))))
    index))

(defun atom-end (text
                 start)
  "Return the index just past the atom that begins at START.

TEXT: the source.
START: index of the first character of the atom."
  (let ((index (if (and (< (+ start 2) (length text))
                        (char= #\# (char text start))
                        (char= #\\ (char text (1+ start))))
                   (+ start 3)
                   (1+ start))))
    (loop while (and (< index (length text))
                     (not (find (char text index) '(#\Space #\Tab #\Newline #\Return #\( #\) #\" #\;))))
          do (incf index))
    index))

(defun parse-source (text)
  "Parse Lisp source text into its top-level nodes. Strings, comments, character literals and
quote prefixes are recognized; nothing is evaluated.

TEXT: the source."
  (let ((stack (list (make-source-node :list
                                       nil
                                       1)))
        (index 0)
        (line 1))
    (flet ((add (node)
             (push node (source-node-children (first stack)))))
      (loop while (< index (length text))
            do (let ((character (char text index)))
                 (cond ((char= #\Newline character)
                        (incf line)
                        (incf index))
                       ((find character '(#\Space #\Tab #\Return))
                        (incf index))
                       ((char= #\; character)
                        (let ((end (or (position #\Newline text :start index) (length text))))
                          (add (make-source-node :comment
                                                 (subseq text index end)
                                                 line))
                          (setf index end)))
                       ((and (char= #\# character)
                             (< (1+ index) (length text))
                             (char= #\| (char text (1+ index))))
                        (let ((end (skip-block-comment-end text index)))
                          (add (make-source-node :comment
                                                 "#|"
                                                 line))
                          (incf line (count #\Newline text :start index :end end))
                          (setf index end)))
                       ((char= #\" character)
                        (let ((end (skip-string-end text index)))
                          (add (make-source-node :atom
                                                 (subseq text index end)
                                                 line))
                          (incf line (count #\Newline text :start index :end end))
                          (setf index end)))
                       ((char= #\( character)
                        (push (make-source-node :list
                                                nil
                                                line) stack)
                        (incf index))
                       ((char= #\) character)
                        (let ((finished (pop stack)))
                          (setf (source-node-children finished) (nreverse (source-node-children finished)))
                          (when stack
                            (add finished)))
                        (incf index))
                       ((find character "'`,")
                        ;; A quote prefix: a following list is marked, an atom keeps the mark.
                        (if (and (< (1+ index) (length text)) (char= #\( (char text (1+ index))))
                            (progn (push (make-source-node :list
                                                           (string character)
                                                           line) stack)
                                   (incf index 2))
                            (let ((end (atom-end text (1+ index))))
                              (add (make-source-node :atom
                                                     (subseq text index end)
                                                     line))
                              (setf index end))))
                       (t
                        (let ((end (atom-end text index)))
                          (add (make-source-node :atom
                                                 (subseq text index end)
                                                 line))
                          (setf index end)))))))
    (nreverse (source-node-children (first stack)))))

(defun lambda-list-signature (lambda-list)
  "Describe how a call passes its arguments: how many positional parameters come first, and
whether the rest are keyword pairs (&key) or a free list (&rest, treated the same way).

LAMBDA-LIST: a list node, or NIL when the lambda list is unknown."
  (let ((positional 0)
        (keywords nil)
        (stopped nil))
    (dolist (item (remove :comment (and lambda-list (source-node-children lambda-list)) :key #'source-node-kind))
      (let ((text (and (eq :atom (source-node-kind item)) (source-node-text item))))
        (cond ((and text (member text '("&key" "&rest" "&body") :test #'string-equal))
               (setf stopped t
                     keywords t))
              ((and text (string-equal text "&optional")))
              ((and text (char= #\& (char text 0))))
              ((not stopped) (incf positional)))))
    (cons positional keywords)))

(defun structure-constructors (form)
  "Return the constructors that a DEFSTRUCT form defines, as (name . signature) conses.

FORM: a list node whose head is DEFSTRUCT."
  (let* ((elements (remove :comment (source-node-children form) :key #'source-node-kind))
         (spec (second elements))
         (name (if (eq :atom (source-node-kind spec))
                   (source-node-text spec)
                   (source-node-text (first (source-node-children spec)))))
         (explicit '()))
    (when (eq :list (source-node-kind spec))
      (dolist (option (rest (source-node-children spec)))
        (when (and (eq :list (source-node-kind option))
                   (string-equal ":constructor" (source-node-text (first (source-node-children option))))
                   (second (source-node-children option)))
          (let ((children (remove :comment (source-node-children option) :key #'source-node-kind)))
            (push (cons (string-downcase (source-node-text (second children)))
                        (if (third children)
                            (lambda-list-signature (third children))
                            (cons 0 t)))
                  explicit)))))
    (or explicit
        (list (cons (concatenate 'string "make-" (string-downcase name))
                    (cons 0 t))))))

(defun collect-function-names (nodes
                               table)
  "Record in TABLE the signatures of the functions and constructors that top-level forms define.

NODES: top-level nodes of one file.
TABLE: hash table from lower-case name to (positional-count . keyword-p)."
  (dolist (form nodes)
    (when (and (eq :list (source-node-kind form))
               (source-node-children form)
               (eq :atom (source-node-kind (first (source-node-children form)))))
      (let ((head (string-downcase (source-node-text (first (source-node-children form)))))
            (elements (remove :comment (source-node-children form) :key #'source-node-kind)))
        (cond ((and (member head '("defun" "defgeneric") :test #'string=)
                    (second elements))
               (setf (gethash (string-downcase (source-node-text (second elements))) table)
                     (lambda-list-signature (and (eq :list (source-node-kind (third elements)))
                                                 (third elements)))))
              ((and (string= head "defstruct")
                    (second elements))
               (loop for (name . signature) in (structure-constructors form)
                     do (setf (gethash name table) signature))))))))

(defun bare-name (text)
  "Return a symbol's name in lower case, without its package prefix.

TEXT: the written symbol."
  (string-downcase (subseq text (1+ (or (position #\: text :from-end t) -1)))))

(defun call-parameters (call
                        signature)
  "Return the parameters of a call as a list of the nodes that start them: the leading
positional ones one by one, then, when the function takes keywords, each keyword together with
the value that follows it.

CALL: a list node that is a call.
SIGNATURE: the (positional-count . keyword-p) of the function it calls."
  (let ((elements (remove :comment (rest (source-node-children call)) :key #'source-node-kind))
        (positional (car signature))
        (starts '()))
    (loop while elements
          do (let ((element (pop elements)))
               (push element starts)
               (if (plusp positional)
                   (decf positional)
                   (when (and (cdr signature)
                              (eq :atom (source-node-kind element))
                              (plusp (length (source-node-text element)))
                              (char= #\: (char (source-node-text element) 0))
                              elements)
                     (pop elements)))))
    (nreverse starts)))

(defun declaration-form-p (node)
  "Return true when NODE defines a structure, condition or class: its slot specifications
look like calls but are not.

NODE: a list node."
  (let ((head (first (source-node-children node))))
    (and head
         (eq :atom (source-node-kind head))
         (null (source-node-text node))
         (member (string-downcase (source-node-text head))
                 '("defstruct" "define-condition" "defclass")
                 :test #'string=))))

(defun layout-violations (path
                          functions)
  "Return the violations of the layout rule in the file at PATH.

PATH: the file.
FUNCTIONS: hash table from the project's function names to their signatures."
  (let ((name (relative-name path))
        (violations '()))
    (labels ((lambda-list-position-p (head
                                      position
                                      seen-lambda-list)
               ;; The lambda list is no call, even when its first variable shares a function's name.
               (cond ((member head '("defun" "defmacro" "defgeneric") :test #'string=) (= position 2))
                     ((string= head "lambda") (= position 1))
                     ((string= head "defmethod") (and (>= position 2) (not seen-lambda-list)))
                     (t nil)))
             (check (node lambda-list-p)
               ;; Quoted data is not code, and neither are the slot specifications of a structure.
               (when (and (eq :list (source-node-kind node))
                          (null (source-node-text node))
                          (not (declaration-form-p node)))
                 (let ((elements (remove :comment (source-node-children node) :key #'source-node-kind)))
                   (when (and elements
                              (not lambda-list-p)
                              (eq :atom (source-node-kind (first elements)))
                              (char/= #\: (char (source-node-text (first elements)) 0))
                              (gethash (bare-name (source-node-text (first elements))) functions))
                     (let ((lines (mapcar #'source-node-line
                                          (call-parameters node
                                                           (gethash (bare-name (source-node-text (first elements)))
                                                                    functions)))))
                       (when (and (>= (length lines) +parameters-per-line-threshold+)
                                  (notevery #'< lines (rest lines)))
                         (push (format nil "~a:~d: call to ~a puts several of its ~d parameters on one line"
                                       name
                                       (source-node-line node)
                                       (bare-name (source-node-text (first elements)))
                                       (length lines))
                               violations))))
                   (let ((head (and elements
                                    (eq :atom (source-node-kind (first elements)))
                                    (string-downcase (source-node-text (first elements)))))
                         (seen-lambda-list nil))
                     (loop for child in (source-node-children node)
                           for position from 0
                           do (let ((is-lambda-list (and head
                                                         (eq :list (source-node-kind child))
                                                         (null (source-node-text child))
                                                         (lambda-list-position-p head
                                                                                 position
                                                                                 seen-lambda-list))))
                                (when is-lambda-list
                                  (setf seen-lambda-list t))
                                (check child is-lambda-list))))))))
      (dolist (form (parse-source (with-open-file (stream path :external-format :utf-8)
                                    (let ((text (make-string (file-length stream))))
                                      (subseq text 0 (read-sequence text stream))))))
        (check form nil)))
    (nreverse violations)))

(defun project-function-names ()
  "Return a hash table from the name of every function and constructor the project defines to
its signature, a (positional-count . keyword-p) cons."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (file (lint-files))
      (collect-function-names (parse-source (with-open-file (stream file :external-format :utf-8)
                                              (let ((text (make-string (file-length stream))))
                                                (subseq text 0 (read-sequence text stream)))))
                              table))
    table))

(defun layout-files ()
  "Return the pathnames of the files the layout rule applies to."
  (remove-if-not (lambda (file)
                   (some (lambda (directory)
                           (let ((name (relative-name file)))
                             (and (>= (length name) (length directory))
                                  (string= directory name :end2 (length directory)))))
                         *layout-checked-directories*))
                 (lint-files)))

;;; Documentation

(defun lambda-list-variables (lambda-list)
  "Return the names of the variables of a lambda list, in order, flattening destructuring.

LAMBDA-LIST: a lambda list as reported by SB-INTROSPECT."
  (let ((variables '()))
    (labels ((walk (item)
               (cond ((null item))
                     ((and (symbolp item) (char= #\& (char (symbol-name item) 0))))
                     ((symbolp item) (push (symbol-name item) variables))
                     ((and (consp item) (keywordp (first item))))
                     ((consp item)
                      (if (and (consp (first item)) (keywordp (first (first item))))
                          (walk (second (first item)))
                          (walk (first item)))
                      (when (and (consp (first item)) (not (keywordp (first (first item)))))
                        (mapc #'walk (rest item)))))))
      (dolist (item lambda-list)
        (if (and (consp item) (consp (first item)) (not (keywordp (first (first item)))))
            (mapc #'walk item)
            (walk item))))
    (nreverse variables)))

(defun documents-parameter-p (documentation
                              name)
  "Return true when a docstring has a line that starts with NAME and a colon.

DOCUMENTATION: the docstring.
NAME: the parameter name in upper case."
  (let ((prefix (concatenate 'string name ":")))
    (with-input-from-string (stream documentation)
      (loop for line = (read-line stream nil nil)
            while line
            thereis (let ((trimmed (string-left-trim " " line)))
                      (and (>= (length trimmed) (length prefix))
                           (string= prefix trimmed :end2 (length prefix))))))))

(defun structure-function-p (symbol)
  "Return true when SYMBOL names a function generated by DEFSTRUCT: a constructor, accessor,
predicate or copier. Their documentation lives in the structure's docstring.

SYMBOL: a symbol."
  (let ((transform (sb-int:info :function :source-transform symbol)))
    (and (consp transform)
         (typep (car transform) 'sb-kernel:defstruct-description))))

(defun slot-reader-method-p (method
                             function-name)
  "Return true when METHOD reads a slot: an ordinary reader method, or a method specialized on
a single condition class whose name prefixes FUNCTION-NAME (SBCL's condition readers).

METHOD: a method of a generic function.
FUNCTION-NAME: the name of the generic function, as a string."
  (or (typep method 'sb-pcl::standard-reader-method)
      (let ((specializers (sb-mop:method-specializers method)))
        (and (= 1 (length specializers))
             (typep (first specializers) 'sb-pcl::condition-class)
             (let ((prefix (concatenate 'string (symbol-name (class-name (first specializers))) "-")))
               (and (> (length function-name) (length prefix))
                    (string= prefix function-name :end2 (length prefix))))))))

(defun reader-function-p (symbol)
  "Return true when SYMBOL names a generic function made only of slot readers, which a
condition or class documents through its own docstring.

SYMBOL: a symbol."
  (let ((function (and (fboundp symbol) (fdefinition symbol))))
    (and (typep function 'standard-generic-function)
         (let ((methods (sb-mop:generic-function-methods function)))
           (and methods
                (every (lambda (method) (slot-reader-method-p method (symbol-name symbol)))
                       methods))))))

(defun function-documentation-violations (symbol)
  "Return the documentation violations of the function or macro named by SYMBOL.

SYMBOL: an exported symbol with a function or macro definition."
  (let* ((macro (macro-function symbol))
         (function (or macro (fdefinition symbol)))
         (documentation (documentation symbol 'function))
         (label (format nil "~a:~a" (package-name (symbol-package symbol)) (symbol-name symbol))))
    (cond ((or (structure-function-p symbol) (reader-function-p symbol)) '())
          ((null documentation) (list (format nil "~a: no docstring" label)))
          (t (loop for variable in (lambda-list-variables (sb-introspect:function-lambda-list function))
                   unless (documents-parameter-p documentation variable)
                     collect (format nil "~a: the docstring does not describe ~a (write a line \"~a: ...\")"
                                     label variable variable))))))

(defun symbol-documentation-violations (symbol)
  "Return the documentation violations of the exported SYMBOL.

SYMBOL: an exported symbol."
  (let ((label (format nil "~a:~a" (package-name (symbol-package symbol)) (symbol-name symbol)))
        (violations '()))
    (when (fboundp symbol)
      (setf violations (function-documentation-violations symbol)))
    (when (and (boundp symbol) (null (documentation symbol 'variable)))
      (push (format nil "~a: variable or constant without a docstring" label) violations))
    (when (and (find-class symbol nil) (null (documentation symbol 'type)))
      (push (format nil "~a: type without a docstring" label) violations))
    violations))

(defun documentation-violations ()
  "Return the documentation violations of every exported symbol of the project's packages."
  (let ((violations '()))
    (dolist (system *lint-documented-systems*)
      (with-quiet-progress
        (apply #'quickload-quietly (third-party-dependencies system))
        (asdf:load-system system)))
    (dolist (package (list-all-packages))
      (let ((name (package-name package)))
        (when (and (own-system-p (string-downcase name))
                   (not (member name *undocumented-package-names* :test #'string=)))
          (do-external-symbols (symbol package)
            (when (eq (symbol-package symbol) package)
              (setf violations (append (symbol-documentation-violations symbol) violations)))))))
    (sort violations #'string<)))

;;; Architecture

(defun core-dependency-violations ()
  "Return a violation for every forbidden system in the dependency closure of the core."
  (let ((seen '())
        (violations '()))
    (labels ((visit (name path)
               (let ((key (string-downcase name)))
                 (unless (member key seen :test #'string=)
                   (push key seen)
                   (when (member key *forbidden-core-dependencies* :test #'string=)
                     (push (format nil "the core system depends on ~a (via ~{~a~^ -> ~})" key (reverse path))
                           violations))
                   (when (own-system-p key)
                     (dolist (dependency (asdf:system-depends-on (asdf:find-system key)))
                       (when (stringp dependency)
                         (visit dependency (cons key path)))))))))
      (visit "borba-cad" '()))
    violations))

;;; Entry point

(defun lint ()
  "Run every static check, print the violations and exit with status 1 when there are any."
  (let* ((files (lint-files))
         (functions (project-function-names))
         (violations (append (loop for file in files append (line-violations file))
                             (loop for file in (layout-files) append (layout-violations file functions))
                             (core-dependency-violations)
                             (documentation-violations))))
    (cond (violations
           (format *error-output* "~&~{~a~%~}~%LINT FAILED: ~d violation(s) in ~d files.~%"
                   violations (length violations) (length files))
           (fail "lint found ~d violation(s)" (length violations)))
          (t (format t "~&Lint OK: ~d files and every exported symbol checked, no violations.~%"
                     (length files))))))
