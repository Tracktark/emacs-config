;; rz-util.el --- Utility changes to emacs

(use-package rg
  :general
  (leader-def
    "s" '(:ignore t :wk "search")
    "s s" '(rg-literal :wk "Literal")
    "s r" '(rg :wk "Regex")
    "s t" '(rz/rg-todo-project :wk "Find all todos")
    "s p" '(rz/rg-project :wk "Search in project"))
  :init
  (rg-define-search rz/rg-todo-project :query "TODO:" :files "*" :dir project)
  (rg-define-search rz/rg-project :files "*" :dir project))

(use-package undo-fu)

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*.el"))
  :init
  (vertico-mode 1)
  (setq completion-styles '(substring basic)
        vertico-count 7
        vertico-cycle t)
  :general
  (:keymaps 'vertico-map
    "C-j" 'vertico-next
    "C-k" 'vertico-previous))

(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless basic)))

(use-package vertico-directory
  :after vertico
  :straight nil
  :general
  (:keymaps 'vertico-map
            "RET" 'vertico-directory-enter
            "DEL" 'vertico-directory-delete-char))

(use-package which-key
  :init (which-key-mode 1))
(use-package helpful
  :general
  (leader-def
    "h" '(:ignore t :wk "help")
    "h k" '(helpful-key :wk "Describe key")
    "h f" '(helpful-callable :wk "Describe function")
    "h F" '(helpful-command :wk "Describe command")
    "h v" '(helpful-variable :wk "Describe variable"))
  (:keymaps 'helpful-mode-map
   :states 'normal
    "q" 'quit-window))

(use-package flycheck
  :demand t
  :config
  (global-flycheck-mode)
  :general
  (leader-def
   "c x" '(flycheck-list-errors :wk "Show error list"))
  (:states '(normal visual)
   "] e" '(flycheck-next-error :wk "Go to next error")
   "[ e" '(flycheck-previous-error :wk "Go to previous error"))
  (:keymaps 'flycheck-error-list-mode-map
   :states 'normal
   "q" 'quit-window))

(use-package company
  :general
  (:states 'insert
    "C-<tab>" 'company-complete)
  :init
  (setq company-idle-delay nil)
  (global-company-mode))

(use-package tree-sitter
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook 'tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :after tree-sitter
  :demand t)
(use-package evil-textobj-tree-sitter
  :after (tree-sitter evil)
  :demand t
  :general
  (:keymaps 'evil-outer-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
   "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))
   
  (:keymaps 'evil-inner-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
   "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
  (:states '(normal visual)
   "[f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t))
   "]f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer"))))

(use-package parinfer-rust-mode
  :hook ((emacs-lisp-mode . parinfer-rust-mode)
         (fennel-mode . parinfer-rust-mode)
         (parinfer-rust-mode . (lambda () (electric-pair-local-mode -1))))
  :config
  (setq parinfer-rust-troublesome-modes (delete 'electric-pair-mode parinfer-rust-troublesome-modes)))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-mode . (lambda () (setq-local evil-lookup-func 'lsp-describe-thing-at-point))))
  :commands (lsp lsp-deferred)
  :general
  (leader-def
   "c a" '(lsp-execute-code-action :wk "Code Action")
   "c l" '(:keymap lsp-command-map :wk "lsp"))
  (:states 'insert
   :keymaps 'lsp-mode-map
   "M-j" 'lsp-signature-next
   "M-k" 'lsp-signature-previous)
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-signature-doc-lines 1
        lsp-lens-enable nil)
  (add-to-list 'display-buffer-alist '("\*lsp-help\*" . (display-buffer-at-bottom))))

(use-package lsp-ui
  :config
  (setq lsp-diagnostics-attributes '()
        lsp-ui-doc-enable nil))

(use-package projectile
  :general
  (leader-def
    "p" '(:keymap projectile-command-map :wk "project"))
  :config
  (setq projectile-switch-project-action 'projectile-find-file)
  (projectile-mode))

(use-package perspective
  :general
  (leader-def
    "W" '(:ignore t :wk "workspaces")
    "W s" '(persp-switch :wk "Switch workspace")
    "W w" '(persp-switch :wk "Switch workspace")
    "W k" '(persp-remove-buffer :wk "Remove buffer from workspace")
    "W d" '(persp-kill :wk "Delete workspace")
    "W r" '(persp-rename :wk "Rename workspace")
    "W b" '(persp-switch-to-buffer :wk "Switch to buffer in another workspace")

    "b B" '(persp-switch-to-buffer :wk "Switch to buffer in another workspace")
    "b b" '(persp-switch-to-buffer* :wk "Switch buffers")
    "<backtab>" '(persp-switch-last :wk "Switch to previous workspace"))
  :init
  (setq persp-suppress-no-prefix-key-warning t
        persp-state-default-file (expand-file-name "persp-save" user-emacs-directory)
        persp-modestring-short t
        persp-purge-initial-persp-on-save t)
  (persp-mode 1))

(use-package persp-projectile
  :after (perspective projectile)
  :demand t
  :config
  (defun rz/projectile-find-file ()
    (interactive)
    (if (projectile-project-p
          (call-interactively 'projectile-find-file))
        (call-interactively 'projectile-persp-switch-project)))
  :general
  (:keymaps 'projectile-command-map
   "p" '(projectile-persp-switch-project :wk "Switch project"))
  (leader-def "SPC" '(rz/projectile-find-file :wk "Find in project")))

(use-package magit
  :general
  (leader-def
   "g" '(magit-status :wk "Magit"))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (add-to-list 'display-buffer-alist '("magit-diff" . (display-buffer-at-bottom))))

(use-package yasnippet
  :init
  (yas-global-mode 1))
(use-package yasnippet-snippets
  :after yasnippet)

(provide 'rz-util)
