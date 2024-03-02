;;; rz-emacs.el --- Base emacs configuration

(require 'general)

;; Change personal details
(setq user-full-name "Richard Závodský"
      user-mail-address "zavodsky.richard1@gmail.com")

;; Keep Custom settings in a separate file
(setq custom-file (expand-file-name "custom-settings.el" user-emacs-directory))
(load custom-file t)

;; Performance settings
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

;; Short yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;; Automatically revert files after changes on disk
(global-auto-revert-mode t)

;; Backup and autosave config
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying 1
      delete-old-versions -1
      version-control t
      vc-make-backup-files t)
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

;; Minibuffer history
(savehist-mode 1)

;; Use 4 spaces for indentation by default
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Startup changes
(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil
      initial-major-mode 'text-mode)

(electric-pair-mode 1)
(global-subword-mode 1)
(winner-mode 1)
(general-def :states '(visual normal insert)
             "<mouse-8>" 'winner-undo
             "<mouse-9>" 'winner-redo)

(setq calendar-week-start-day 1)

(add-to-list 'display-buffer-alist
             '("\\*eshell\\*" . (display-buffer-at-bottom)))
(setq display-buffer-base-action '(display-buffer-same-window))

(use-package compile
  :config
  (setq compile-command "make -j8 -k "
        compilation-scroll-output 'first-error)
  (defun rz/open-compilation-if-failed (buffer string)
      "Display a compilation buffer if compilation didn't succeed."
      (when (or (> compilation-num-errors-found 0
                  (> compilation-num-warnings-found 0)))
          (pop-to-buffer buffer)))
  (add-hook 'compilation-finish-functions 'rz/open-compilation-if-failed)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(use-package recentf
  :straight nil
  :demand t
  :general
  (leader-def
    "f r" '(recentf :wk "Open recent file"))
  :config
  (recentf-mode)
  (run-at-time nil (* 5 60) 'recentf-save-list))

(use-package dired
  :straight nil
  :demand t
  :hook (dired-mode . dired-omit-mode)
  :config
  (setq dired-listing-switches "-halv --group-directories-first"
        dired-compress-directory-default-suffix ".zip"
        dired-compress-file-default-suffix ".zip"
        dired-dwim-target t
        dired-auto-revert-buffer 'dired-buffer-stale-p)
  (add-hook 'dired-mode-hook (defun rz/set-dired-keys ()
                                 (general-def
                                  :keymaps 'dired-mode-map
                                  :states 'normal
                                  "<mouse-2>" 'dired-mouse-find-file
                                  "<mouse-8>" 'dired-up-directory)))
  (with-eval-after-load 'dired-aux
    (add-to-list 'dired-compress-file-alist '("\\.zip\\'" . "zip %o %i"))))

(provide 'rz-emacs)
