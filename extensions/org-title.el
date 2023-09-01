;;; org-title.el --- Center org title
;;; Commentary:
;; This package centers the title in an org document and hides the options under the title
;;; Code:

(defvar-local my/org-property-overlay nil
  "The overlay for properties under the title.")
(defvar-local my/org-title-overlay nil
  "The overlay for the title in an org document.")

(defun my/org-cycle-property-visibility ()
  "Toggle the visibility of properties under the title."
  (interactive)
  (when my/org-property-overlay
    (overlay-put my/org-property-overlay 'invisible (not (overlay-get my/org-property-overlay 'invisible)))))

(defun my/org-hide-properties ()
  "Create an overlay over properties under title in org documents."
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (when (equal (buffer-substring-no-properties (point-at-bol) (point-at-eol)) ":OPTIONS:")
      (let ((start (point-at-bol)))
        (if (re-search-forward "^:END:$" nil t)
            (progn
              (setq-local my/org-property-overlay (make-overlay (1- start) (point-at-eol) nil t))
              (overlay-put my/org-property-overlay 'invisible t))
          (user-error "No :END: found"))))))

(defun my/org-recenter-title (&rest _)
  "Center the title of an org document.
This should only be called after my/org-center-title."
  (save-excursion
    (goto-char (point-min))
    (let* ((char-pixel-width (window-font-width nil 'org-document-title))
           (line-char-length (- (line-end-position) (line-beginning-position) 7))
           (line-pixel-length (* char-pixel-width line-char-length))
           (window-pixel-width (window-body-width nil t))
           (space-pixel-length (/ (- window-pixel-width line-pixel-length) 2))
           (space-char-length (/ space-pixel-length char-pixel-width)))
      (overlay-put my/org-title-overlay 'before-string (make-string space-char-length ? )))))

(defun my/org-center-title ()
  "Create an overlay over the title in an org document and center it."
  (let ((overlay (overlays-at 10)))
        (if (> (length overlay) 0)
            (progn
              (setq-local my/org-title-overlay (car overlay))
              (my/org-recenter-title))
          (save-excursion
            (goto-char (point-min))
            (when (string-prefix-p "#+TITLE:" (buffer-substring-no-properties (point-at-bol) (point-at-eol)) t)
              (setq-local my/org-title-overlay (make-overlay 9 (point-at-eol)))
              (my/org-recenter-title)
              (overlay-put my/org-title-overlay 'modification-hooks '(my/org-recenter-title))
              (let ((title-keymap (make-sparse-keymap)))
                (define-key title-keymap "\t" #'my/org-cycle-property-visibility)
                (overlay-put my/org-title-overlay 'keymap title-keymap))
              (add-hook 'window-size-change-functions #'my/org-recenter-title nil t))))))


(provide 'org-title)
;;; org-title.el ends here
