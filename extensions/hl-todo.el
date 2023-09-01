;;; hl-todo.el --- Highlight TODO and similar keywords  -*- lexical-binding:t -*-

;;; Commentary:

;; Highlight TODO and similar keywords in comments and strings.

;; You can either explicitly turn on `hl-todo-mode' in certain buffers
;; or use the global variant `global-hl-todo-mode', which enables
;; the local mode in all buffers.

;;; Code:

;; TODO: Do something
;; This something should do something

;; This shouldn't be highlighted anymore

(require 'cl-lib)

(eval-when-compile (require 'subr-x))

(defgroup hl-todo nil
  "Highlight TODO and similar keywords in comments and strings."
  :group 'font-lock-extra-types)

(defface hl-todo
  '((t (:bold t :foreground "#cc9393")))
  "Base face used to highlight TODO and similar keywords.
The faces used to highlight certain keywords are, by default,
created by inheriting this face and using the appropriate
color specified using the option `hl-todo-keyword-faces' as
foreground color."
  :group 'hl-todo)

(defcustom hl-todo-include-modes '(prog-mode text-mode)
  "Major-modes in which `hl-todo-mode' is activated.

This is used by `global-hl-todo-mode', which activates the local
`hl-todo-mode' in all buffers whose major-mode derive from one
of the modes listed here, but not from one of the modes listed
in `hl-todo-exclude-modes'."
  :package-version '(hl-todo . "2.1.0")
  :group 'hl-todo
  :type '(repeat function))

(defcustom hl-todo-exclude-modes '(org-mode)
  "Major-modes in which `hl-todo-mode' is not activated.

This is used by `global-hl-todo-mode', which activates the local
`hl-todo-mode' in all buffers whose major-mode derived from one
of the modes listed in `hl-todo-include-modes', but not from one
of the modes listed here."
  :package-version '(hl-todo . "3.1.0")
  :group 'hl-todo
  :type '(repeat function))

(defcustom hl-todo-text-modes '(text-mode)
  "Major-modes that are considered text-modes.

In buffers whose major-mode derives from one of the modes listed
here TODO keywords are always highlighted even if they are not
located inside a string."
  :package-version '(hl-todo . "2.1.0")
  :group 'hl-todo
  :type '(repeat function))

(defcustom hl-todo-keyword-faces
  '(("HOLD" . "#d0bf8f")
    ("TODO" . "#cc9393")
    ("NEXT" . "#dca3a3")
    ("THEM" . "#dc8cc3")
    ("PROG" . "#7cb8bb")
    ("OKAY" . "#7cb8bb")
    ("DONT" . "#5f7f5f")
    ("FAIL" . "#8c5353")
    ("DONE" . "#afd8af")
    ("NOTE"   . "#d0bf8f")
    ("KLUDGE" . "#d0bf8f")
    ("HACK"   . "#d0bf8f")
    ("TEMP"   . "#d0bf8f")
    ("FIXME"  . "#cc9393")
    ("XXXX*"  . "#cc9393"))
  "An alist mapping keywords to colors/faces used to display them.

Each entry has the form (KEYWORD . COLOR).  KEYWORD is used as
part of a regular expression.  If (regexp-quote KEYWORD) is not
equal to KEYWORD, then it is ignored by `hl-todo-insert-keyword'.
Instead of a color (a string), each COLOR may alternatively be a
face.

The syntax class of the characters at either end has to be `w'
\(which means word) in `hl-todo--syntax-table' (which derives
from `text-mode-syntax-table').

This package, like most of Emacs, does not use POSIX regexp
backtracking.  See info node `(elisp)POSIX Regexp' for why that
matters.  If you have two keywords \"TODO-NOW\" and \"TODO\", then
they must be specified in that order.  Alternatively you could
use \"TODO\\(-NOW\\)?\"."

  :package-version '(hl-todo . "3.5.0")
  :group 'hl-todo
  :type '(repeat (cons (string :tag "Keyword")
                       (choice :tag "Face   "
                               (string :tag "Color")
                               (sexp :tag "Face")))))

(defvar-local hl-todo--regexp nil)
(defvar-local hl-todo--keywords nil)

(defun hl-todo--regexp ()
  (or hl-todo--regexp (hl-todo--setup-regexp)))

(defun hl-todo--setup-regexp ()
  (when-let ((bomb (assoc "???" hl-todo-keyword-faces)))
    ;; If the user customized this variable before we started to
    ;; treat the strings as regexps, then the string "???" might
    ;; still be present.  We have to remove it because it results
    ;; in the regexp search taking forever.
    (setq hl-todo-keyword-faces (delete bomb hl-todo-keyword-faces)))
  (setq hl-todo--regexp
        (concat "\\(\\<"
                "\\(" (mapconcat #'car hl-todo-keyword-faces "\\|") "\\)"
                "\\>.*$\\)")))

(defun hl-todo--setup ()
  (hl-todo--setup-regexp)
  (setq hl-todo--keywords
        `((,(lambda (bound) (hl-todo--search nil bound))
           (1 (hl-todo--get-face) prepend t))))
  (font-lock-add-keywords nil hl-todo--keywords t))

(defvar hl-todo--syntax-table (copy-syntax-table text-mode-syntax-table))

(defvar syntax-ppss-table) ; Silence Emacs 25's byte-compiler.

(defun hl-todo--search (&optional regexp bound backward)
  (unless regexp
    (setq regexp hl-todo--regexp))
  (cl-block nil
    (while (let ((case-fold-search nil)
                 (syntax-ppss-table (syntax-table)))
             (with-syntax-table hl-todo--syntax-table
               (funcall (if backward #'re-search-backward #'re-search-forward)
                        regexp bound t)))
      (cond ((or (apply #'derived-mode-p hl-todo-text-modes)
                 (hl-todo--inside-comment-or-string-p))
             (cl-return t))
            ((and bound (funcall (if backward #'<= #'>=) (point) bound))
             (cl-return nil))))))

(defun hl-todo--inside-comment-or-string-p ()
  (nth 8 (syntax-ppss)))

(defun hl-todo--get-face ()
  (let ((keyword (match-string 2)))
    (hl-todo--combine-face
     (cdr (cl-find-if (lambda (elt)
                        (string-match-p (format "\\`%s\\'" (car elt))
                                        keyword))
                      hl-todo-keyword-faces)))))

(defun hl-todo--combine-face (face)
  (if (stringp face)
      `((:foreground ,face)
        hl-todo)
    face))

;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight TODO and similar keywords in comments and strings."
  :lighter ""
  :group 'hl-todo
  (if hl-todo-mode
      (hl-todo--setup)
    (font-lock-remove-keywords nil hl-todo--keywords))
  (when font-lock-mode
    (jit-lock-mode 1)))

;;;###autoload
(define-globalized-minor-mode global-hl-todo-mode
  hl-todo-mode hl-todo--turn-on-mode-if-desired)

(defun hl-todo--turn-on-mode-if-desired ()
  (when (and (apply #'derived-mode-p hl-todo-include-modes)
             (not (apply #'derived-mode-p hl-todo-exclude-modes))
             (not (bound-and-true-p enriched-mode)))
    (hl-todo-mode 1)))

(provide 'hl-todo)
;;; hl-todo.el ends here
