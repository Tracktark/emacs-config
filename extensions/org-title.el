;;; org-title.el --- Center org title
;;; Commentary:
;; This package centers the title in an org document and hides the options under the title
;;; Code:

(defvar-local rz/org-property-overlay nil
  "The overlay for properties under the title.")
(defvar-local rz/org-title-overlay nil
  "The overlay for the title in an org document.")

(defun rz/org-cycle-property-visibility ()
  "Toggle the visibility of properties under the title."
  (interactive)
  (when rz/org-property-overlay
    (overlay-put rz/org-property-overlay 'invisible (not (overlay-get rz/org-property-overlay 'invisible)))))

(defun rz/org-hide-properties ()
  "Create an overlay over properties under title in org documents."
  (save-excursion
    (goto-char (point-min))
    (forward-line)
    (when (equal (buffer-substring-no-properties (point-at-bol) (point-at-eol)) ":OPTIONS:")
      (let ((start (point-at-bol)))
        (if (re-search-forward "^:END:$" nil t)
            (progn
              (setq-local rz/org-property-overlay (make-overlay (1- start) (point-at-eol) nil t))
              (overlay-put rz/org-property-overlay 'invisible t))
          (user-error "No :END: found"))))))

(defun rz/org-recenter-title (&rest _)
  "Center the title of an org document.
This should only be called after rz/org-center-title."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (rx bol "#+TITLE:") nil t)
      (let* ((char-pixel-width (window-font-width nil 'org-document-title))
             (line-char-length (- (line-end-position) (line-beginning-position) 7))
             (line-pixel-length (* char-pixel-width line-char-length))
             (window-pixel-width (window-body-width nil t))
             (space-pixel-length (/ (- window-pixel-width line-pixel-length) 2))
             (space-char-length (/ space-pixel-length char-pixel-width)))
        (overlay-put rz/org-title-overlay 'before-string (make-string space-char-length 32))))))

(defun rz/org-center-title ()
  "Create an overlay over the title in an org document and center it."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (rx bol "#+TITLE:") nil t)
      (setq-local rz/org-title-overlay (let ((overlays (overlays-at (+ (match-end 0) 1))))
                                         (if (> (length overlays) 0) (car overlays) (make-overlay (match-end 0) (pos-eol)))))
      (rz/org-recenter-title)
      (overlay-put rz/org-title-overlay 'modification-hooks '(rz/org-recenter-title))
      (add-hook 'window-size-change-functions #'rz/org-recenter-title nil t))))


(provide 'org-title)
;;; org-title.el ends here
