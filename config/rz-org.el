;; rz-org.el --- Org mode setup

(defun rz/org-agenda-init ()
  (setq org-agenda-window-setup 'current-window
        org-agenda-files '("todo.org")
        org-agenda-start-on-weekday 1
        org-agenda-todo-ignore-scheduled 'all)
  (add-hook 'server-after-make-frame-hook 'rz/agenda)

  (with-eval-after-load 'org-agenda
    (setq org-agenda-sorting-strategy (cons '(todo priority-down deadline-up) org-agenda-sorting-strategy))))

(defun rz/archive-on-done ()
  (when (and (equal org-state "DONE")
             (equal buffer-file-name (expand-file-name "~/org/todo.org"))
             (y-or-n-p "Do you want to archive this item?"))
    (org-archive-subtree)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . rz/org-center-title)
         (org-mode . rz/org-hide-properties)
         (org-after-todo-state-change . rz/archive-on-done))
  :general
  (leader-def
    "C" '(org-capture :wk "Org Capture")
    "a" `(,(defun rz/agenda (&optional arg) (interactive "P")(org-agenda arg "n"))  :wk "Agenda"))
  (:keymaps 'org-mode-map
   :states 'normal
   "TAB" 'org-cycle
   "M-<return>" 'org-meta-return)
  (:keymaps 'org-mode-map
   :states 'insert
   "$" 'rz/org-auto-latex
   "M-<return>" 'org-meta-return)
  (leader-def
   :keymaps 'org-mode-map
   "n s" '(org-narrow-to-subtree :wk "Subtree"))
  (localleader-def
    :keymaps 'org-mode-map
    "d" '(org-deadline :wk "Deadline")
    "s" '(org-schedule :wk "Scheduled")
    "p" '(org-priority :wk "Priority")
    "e" '(org-export-dispatch :wk "Export"))
  :config
  (defun rz/org-auto-latex ()
    (interactive)
    (org-self-insert-command 1)
    (org-latex-preview))
  (defun rz/org-get-school-todo ()
    (interactive)
    (goto-char (point-min))
    (let* ((subjects '("SOJ Strojovo Orientované Jazyky"
                       "MatA1 Matematická Analýza 1"
                       "IV Internet Vecí"
                       "ANJ Anglický Jazyk Bc. 1"
                       "LogSys Logické Systémy"
                       "CisP Číslicové Počítače"
                       "SI Softvérové Inžinerstvo"
                       "TechP Techniky Programovania 2"
                       "MAS Modelovanie a Simulácia"))
           (selected (completing-read "Subject: " subjects nil t))
           (headline (car (split-string selected))))
      (re-search-forward (rx bol "** " (literal headline)))))
  (setq org-startup-indented t
        org-src-preserve-indentation t
        org-hidden-keywords '(title)
        org-hide-emphasis-markers t
        org-M-RET-may-split-line nil
        org-edit-src-content-indentation 0
        org-duration-format 'h:mm
        org-startup-folded 'showall
        org-startup-with-latex-preview t
        org-startup-with-inline-images t
        org-image-max-width 500
        org-clock-mode-line-total 'today
        org-archive-location ".archive/%s::datetree/"
        org-capture-templates '(("s" "School" entry (file+function "~/org/todo.org" rz/org-get-school-todo) "* TODO %?")
                                ("w" "Work" entry (file+headline "~/org/todo.org" "Work") "* TODO %?")))
  (with-eval-after-load 'ol
    (setq org-link-frame-setup (cons '(file . find-file) org-link-frame-setup)))
  (plist-put org-format-latex-options :scale 2.2)
  (rz/org-agenda-init))

(use-package org-dwim
  :after org
  :straight nil
  :demand t
  :config
  (general-define-key
    :keymaps 'org-mode-map
    :states 'normal
    "RET" 'rz/org-dwim-at-point))

(use-package evil-org
  :straight (:host github :repo "Somelauw/evil-org-mode")
  :after org
  :hook (org-mode . evil-org-mode))
(use-package evil-org-agenda
  :straight nil
  :after org-agenda
  :demand t
  :config
  (evil-org-agenda-set-keys))

(use-package org-tempo
  :straight nil
  :after org)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-title
  :after org
  :straight nil)

(use-package org-roam
  :general
  (leader-def
    "r" '(:ignore t :wk "roam")
    "r r" '(org-roam-node-find :wk "Find Note")
    "r i" '(org-roam-node-insert :wk "Insert Link")
    "r n" '(org-roam-capture :wk "Create Note"))
  (localleader-def
    :keymaps 'org-mode-map
    "r" '(org-roam-node-insert :wk "Insert Roam Link")
    "t" '(org-roam-tag-add :wk "Add tag"))
  :custom
  (org-roam-directory (file-truename (concat org-directory "/roam")))
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags: 25}" 'face 'org-tag)))
  (org-roam-capture-templates '(("d" "default" plain "%?"
                                 :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title:${title}\n%(save-current-buffer (find-file \"%f\") (if (not org-file-tags) \"\" (concat \"#+filetags: :\" (mapconcat 'substring-no-properties org-file-tags \":\") \":\")))\n")
                                 :unnarrowed t)))
  (org-roam-db-node-include-function (lambda () (not (member "fc" (org-get-tags)))))
  :config
  (org-roam-db-autosync-mode))
(use-package org-roam-ui
  :after org-roam)

(use-package org-anki
  :commands (org-anki-sync-entry))

(use-package ob-mermaid
  :after org
  :demand t
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (cons '(mermaid . t) org-babel-load-languages))
  (add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)
  (setq org-confirm-babel-evaluate nil)
  (add-to-list 'org-babel-default-header-args:mermaid '(:background-color . "transparent --scale 2")))

(use-package ox-reveal
  :demand t
  :after org
  :config
  (setq org-reveal-root "file:///usr/local/src/reveal.js"))
(provide 'rz-org)
