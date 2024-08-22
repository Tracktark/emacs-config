;;; hl-todo.el --- Highlight TODO and similar keywords  -*- lexical-binding:t -*-

;;; Commentary:

;; Highlight TODO and similar keywords in comments and strings.

;; You can either explicitly turn on `hl-todo-mode' in certain buffers
;; or use the global variant `global-hl-todo-mode', which enables
;; the local mode in all buffers.

;;; Code:

(defgroup hl-todo nil
  "Highlight TODO and similar keywords in comments and strings."
  :group 'font-lock-extra-types)

(defcustom hl-todo-keyword-faces
  '(("TODO" warning bold))
  "An alist mapping keywords to faces.
Each entry has the form of (KEYWORD FACE...) where KEYWORD is a regular expression
and FACE is the face used to display this keyword."
  :group 'hl-todo)

(defvar-local hl-todo--keywords nil)

(defun hl-todo--anchor-regexp (keyword)
  (rx (literal comment-start) (* space) (group (regexp keyword) ":" (* nonl)) eol))

(defun hl-todo--create-font-lock-keyword (keyword-def)
  (let ((keyword (car keyword-def))
        (face (cdr keyword-def)))
    `(,(hl-todo--anchor-regexp keyword)
      (1 ,face prepend)
      (,(rx bol (* space) (literal comment-start) (* space) (group (* nonl)) eol)
       (hl-todo--find-end-of-comment)
       nil
       (1 ,face prepend)))))

(defun hl-todo--whole-line-comment-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at (rx (* space) (literal comment-start)))))

(defun hl-todo--find-end-of-comment ()
  (when (hl-todo--whole-line-comment-p)
    (save-excursion
      (forward-line 1)
      (end-of-line)
      (while (and (hl-todo--whole-line-comment-p) (< (point) (point-max)))
        (forward-line 1)
        (end-of-line))
      (when (not (hl-todo--whole-line-comment-p))
        (forward-line -1))
      (end-of-line)
      (point))))
  
  
(defun hl-todo--setup ()
  (setq hl-todo--keywords (mapcar #'hl-todo--create-font-lock-keyword hl-todo-keyword-faces))
  (font-lock-add-keywords nil hl-todo--keywords t))
  
;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight TODO and similar keywords in comments and strings."
  :lighter ""
  :group 'hl-todo
  (if hl-todo-mode
      (hl-todo--setup)
    (font-lock-remove-keywords nil hl-todo--keywords)))
  ;; (when font-lock-mode
  ;;   (jit-lock-mode 1)))

(provide 'hl-todo)
;;; hl-todo.el ends here
