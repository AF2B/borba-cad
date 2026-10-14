(in-package #:borba-cad/i18n)

;;;; Languages. A message is written in English where it is used and passed through TR (or TRF for
;;;; a format control); the catalog of the current language maps that English text to its
;;;; translation. Texts kept in tables, where there is no call to make, are marked with MARK so that
;;;; they are found, and passed through TR where they are shown. A catalog is a data file, so that
;;;; a translator edits text and not code, and it is read when the system is loaded.

(defconstant +default-language+ :en-us
  "The language of the program when nothing else was chosen: English as written in the United States.")

(defvar *language* +default-language+
  "The language the interface speaks now, a keyword such as :en-us or :pt-br.")

(defparameter *default-language-name* "English (US)"
  "The name of the default language, in that language.")

(defparameter *catalog-directory* (asdf:system-relative-pathname "borba-cad" "src/i18n/")
  "The folder that holds the catalog files.")

(defparameter *catalog-files* '(:pt-br "pt-br.catalog")
  "The catalog file of each language besides the default one, in the order of the language menu.")

(defstruct (catalog (:constructor make-catalog (language
                                                name)))
  "The translations of one language.

LANGUAGE: the language keyword.
NAME: the name of the language, written in that language.
ENTRIES: hash table from the English text to its translation."
  language
  name
  (entries (make-hash-table :test 'equal)))

(defvar *catalogs* (make-hash-table :test 'eq)
  "The catalogs that were read, by language keyword.")

(defun read-catalog-file (path)
  "Read a catalog file, a list `(:language KEYWORD :name STRING :entries ((ENGLISH TRANSLATION) ...))`
in UTF-8, and return its catalog. Nothing in the file is evaluated.

PATH: the file."
  (with-open-file (stream path :external-format :utf-8)
    (let* ((*read-eval* nil)
           (*package* (find-package :keyword))
           (form (read stream))
           (catalog (make-catalog (getf form :language)
                                  (getf form :name))))
      (loop for (english translation) in (getf form :entries)
            do (setf (gethash english (catalog-entries catalog)) translation))
      catalog)))

(defun load-catalogs ()
  "Read every catalog file into the table of catalogs."
  (loop for (language file) on *catalog-files* by #'cddr
        do (let ((catalog (read-catalog-file (merge-pathnames file *catalog-directory*))))
             (setf (gethash language *catalogs*) catalog))))

(load-catalogs)

(defun supported-languages ()
  "Return the languages the program speaks, as keywords, the default one first."
  (cons +default-language+
        (loop for (language) on *catalog-files* by #'cddr
              collect language)))

(defun language-tag (language)
  "Return the tag of LANGUAGE as people write it, for example \"pt-BR\".

LANGUAGE: a language keyword."
  (let* ((name (string-downcase (symbol-name language)))
         (dash (position #\- name)))
    (if dash
        (concatenate 'string (subseq name 0 dash) "-" (string-upcase (subseq name (1+ dash))))
        name)))

(defun language-name (language)
  "Return the name of LANGUAGE in that language, as the language menu shows it.

LANGUAGE: a language keyword."
  (let ((catalog (gethash language *catalogs*)))
    (cond (catalog (catalog-name catalog))
          ((eq language +default-language+) *default-language-name*)
          (t (language-tag language)))))

(defun parse-language (text)
  "Return the language keyword that TEXT names, or NIL when the program does not speak it. The
tag is accepted in any case with a hyphen or an underscore (pt-BR, pt_br), and the bare language
(en, pt) stands for the one the program has.

TEXT: a language tag typed by the user."
  (let ((wanted (substitute #\- #\_ (string-downcase (string-trim " " text)))))
    (or (find-if (lambda (language)
                   (string= wanted (string-downcase (symbol-name language))))
                 (supported-languages))
        (find-if (lambda (language)
                   (let ((name (string-downcase (symbol-name language))))
                     (and (find #\- name)
                          (string= wanted (subseq name 0 (position #\- name))))))
                 (supported-languages)))))

(defun tr (text)
  "Return TEXT in the current language: its translation, or TEXT itself when the language is the
default or the catalog has none.

TEXT: the English text."
  (if (eq *language* +default-language+)
      text
      (let ((catalog (gethash *language* *catalogs*)))
        (or (and catalog (gethash text (catalog-entries catalog)))
            text))))

(defun trf (control
            &rest arguments)
  "Return CONTROL, translated, filled in with ARGUMENTS as FORMAT does.

CONTROL: the English format control.
ARGUMENTS: the values for its directives."
  (apply #'format nil (tr control) arguments))

(defun mark (text)
  "Return TEXT unchanged. It marks a text that sits in a table as one to be translated, so that
the catalog check finds it; where the text is shown it goes through TR.

TEXT: the English text."
  text)

(defmacro with-language ((language) &body body)
  "Run BODY speaking LANGUAGE.

LANGUAGE: a language keyword.
BODY: the forms to run."
  `(let ((*language* ,language))
     ,@body))

(defun catalog-translations (language)
  "Return the translations of LANGUAGE as a list of (english translation) lists, or NIL for the
default language.

LANGUAGE: a language keyword."
  (let ((catalog (gethash language *catalogs*)))
    (when catalog
      (loop for english being the hash-keys of (catalog-entries catalog) using (hash-value translation)
            collect (list english translation)))))

(defun catalog-language-p (language)
  "Return true when there is a catalog for LANGUAGE.

LANGUAGE: a language keyword."
  (and (gethash language *catalogs*) t))

(defparameter *directive-prefix-characters* "0123456789,+-#v:@"
  "The characters that may come between the tilde of a format directive and its letter: the
parameters and the modifiers.")

(defun format-directives (control)
  "Return the directives of a format control as a list of strings, in order and in lower case, so
that a translation can be checked to ask for the same values as the English text. A doubled tilde
is a tilde, not a directive.

CONTROL: a format control string."
  (let ((directives '())
        (index 0)
        (length (length control)))
    (loop while (< index length)
          do (if (char/= #\~ (char control index))
                 (incf index)
                 (let ((end (1+ index)))
                   (loop while (and (< end length) (find (char control end) *directive-prefix-characters*))
                         do (incf end))
                   (when (and (< end length) (char= #\' (char control end)))
                     (incf end 2))
                   (loop while (and (< end length) (find (char control end) *directive-prefix-characters*))
                         do (incf end))
                   (when (< end length)
                     (unless (char= #\~ (char control end))
                       (push (string-downcase (subseq control index (1+ end))) directives)))
                   (setf index (1+ end)))))
    (nreverse directives)))
