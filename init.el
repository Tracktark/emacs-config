(setq straight-use-package-by-default t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eval-when-compile
  (straight-use-package 'use-package))

(use-package general
  :config
  (general-create-definer leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :keymaps 'override
    :states '(normal visual insert)))

(define-obsolete-variable-alias
  'native-comp-deferred-compilation-deny-list
  'native-comp-jit-compilation-deny-list
  "Renamed in emacs#95692f6")

(setq user-full-name "Richard Závodský"
      user-mail-address "zavodsky.richard1@gmail.com")

(setq custom-file (expand-file-name "custom-settings.el" user-emacs-directory))
(load custom-file t)

(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024))

(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)

(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      backup-by-copying 1
      delete-old-versions -1
      version-control t
      vc-make-backup-files t)
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save-list/" user-emacs-directory) t)))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode 0)

(blink-cursor-mode 0)

(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 140 :weight 'normal)

(savehist-mode 1)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(setq inhibit-startup-message t
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)
(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (internal-border-width . 40)
        (left-fringe . 15)
        (right-fringe . 5)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))

(setq-default line-spacing 5)

(add-to-list 'load-path (expand-file-name "extensions/" user-emacs-directory))

(electric-pair-mode 1)

(put 'narrow-to-region 'disabled nil)

(setq tramp-default-method "ssh")
(with-eval-after-load 'tramp
  (add-to-list 'tramp-connection-properties
               '(".*docker:.*" "remote-shell" "/bin/bash"))
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(pixel-scroll-precision-mode 1)
(setq mouse-wheel-progressive-speed nil
      pixel-scroll-precision-use-momentum t)

(add-to-list 'exec-path "/home/moss/.local/bin")

(setq frame-title-format "%b - Emacs")

(global-subword-mode 1)

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq compilation-scroll-output 'first-error)

(defvar-local my/compile-func 'recompile "Function to run when compiling.")
(defun my/compile () (interactive) (funcall my/compile-func))
(leader-def
 ";" '(pp-eval-expression :wk "Eval Elisp")
 ":" '(execute-extended-command :wk "M-x")
 "<return>" '(bookmark-jump :wk "Jump to Bookmark")

 "o" '(:ignore t :wk "open")
 "c" '(:ignore t :wk "code")
 "c c" '(my/compile :wk "Recompile")
 "c C" '(compile :wk "Compile")

 "f" '(:ignore t :wk "file")
 "f s" '(save-buffer :wk "Save file")
 "f f" '(find-file :wk "Find file")
 "f D" `(,(lambda ()
            (interactive)
            (when (y-or-n-p "Are you sure you want to delete this file?")
              (delete-file buffer-file-name))) :wk "Delete file")
 "f u"  `(,(defun my/sudo-open-file ()
             "Opens current file with sudo"
             (interactive)
             (unless buffer-file-name
               (user-error "Buffer is not associated with any file"))
             (find-file (concat "/sudo::" (expand-file-name buffer-file-name)))) :wk "Open current file with sudo")



 "b" '(:ignore t :wk "buffer")
 "b d" '(kill-current-buffer :wk "Kill buffer")
 "b b" '(switch-to-buffer :wk "Switch buffers")
 "b r" `(,(lambda () (interactive) (revert-buffer nil (not (buffer-modified-p)))) :wk "Revert buffer")

 "n" '(:ignore t :wk "narrow")
 "n w" '(widen :wk "Widen")
 "n f" '(narrow-to-defun :wk "Function")
 "n r" '(narrow-to-region :wk "Region"))

(general-def
 :keymaps 'override
 "ESC" 'keyboard-escape-quit)
(general-def
 :states '(normal visual insert)
 "C-=" 'text-scale-increase
 "C--" 'text-scale-decrease)
(general-def
 :states 'insert
 "C-<backspace>" (defun my/greedy-delete ()
                  (interactive)
                  (let ((beg-of-whitespace (save-excursion
                                             (skip-chars-backward " \t" (point-at-bol))
                                             (point))))
                    (if (equal (point) beg-of-whitespace)
                        (call-interactively 'backward-kill-word)
                      (delete-region beg-of-whitespace (point))))))

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
  (add-hook 'dired-mode-hook (defun my/set-dired-keys ()
                                 (general-def
                                  :keymaps 'dired-mode-map
                                  :states 'normal
                                  "<mouse-2>" 'dired-mouse-find-file
                                  "<mouse-8>" 'dired-up-directory)))
  (with-eval-after-load 'dired-aux
    (add-to-list 'dired-compress-file-alist '("\\.zip\\'" . "zip %o %i"))))

(use-package elegance
  :straight nil
  :general (leader-def
   "t" '(:ignore t :wk "theme")
   "t d" `(,(lambda () (interactive) (elegance-set-theme 'dark)) :wk "Switch to dark theme")
   "t l" `(,(lambda () (interactive) (elegance-set-theme 'light)) :wk "Switch to light theme"))
  :config
  (add-hook 'server-after-make-frame-hook 'elegance-refresh))

(use-package ligature
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" ">\\|<\\|=\\|//\\|:=\\|:+\\|[A-Z]")
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" ">\\|/\\|)\\|*+\\|[a-z]")
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                         (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode 1))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-responsive 'top
        highlight-indent-guides-method 'character)
  :config
  (add-hook 'elegance-theme-change-hook 'highlight-indent-guides-auto-set-faces)
  (add-hook 'server-after-make-frame-hook 'highlight-indent-guides-auto-set-faces 90))

(use-package rg
  :general (leader-def
             "s" '(:ignore t :wk "search")
             "s s" '(rg-literal :wk "Literal")
             "s r" '(rg :wk "Regex")
             "s t" '(my/rg-todo-project :wk "Find all todos")
             "s p" '(my/rg-project :wk "Search in project"))
  :init
  (rg-define-search my/rg-todo-project :query "TODO:" :files "*" :dir project)
  (rg-define-search my/rg-project :files "*" :dir project))

(use-package hl-todo
  :straight nil
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces '(
                                ("TODO" warning bold)
                                ("NOTE" success bold))))

(use-package evil
  :init
  (setq evil-want-fine-undo t
        evil-undo-system 'undo-fu
        evil-want-Y-yank-to-eol t
        evil-ex-substitute-global t
        evil-want-keybinding nil)
  (evil-mode 1)
  :general
  (leader-def
   "TAB" '(evil-switch-to-windows-last-buffer :wk ("Switch to other buffer"))
   "w" '(:ignore t :wk "window")
   "w d" '(evil-window-delete :wk "Close window")
   "w w" '(evil-window-next :wk "Next window")
   "w v" '(evil-window-vsplit :wk "VSplit window")
   "w s" '(evil-window-split :wk "HSplit window"))
  (general-def
    :states '(normal visual insert)
    "<mouse-8>" 'evil-switch-to-windows-last-buffer))

(use-package evil-nerd-commenter
  :general (:states '(normal visual)
   "g c" '(evilnc-comment-operator :wk "Comment/Uncomment")))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :general
  (:keymaps 'evil-collection-unimpaired-mode-map
   :states '(normal visual)
   "] e" nil
   "[ e" nil))

(use-package evil-surround
  :after evil
  :hook
  (magit-mode . (lambda () (evil-surround-mode -1)))
  :demand t
  :general
  (:states 'visual
   :keymaps 'evil-surround-mode-map
   "s" 'evil-surround-region
   "S" 'evil-Surround-region)
  :config
  (global-evil-surround-mode 1))

(use-package evil-numbers
  :after evil
  :general
  (:states '(normal visual)
   "g=" '(evil-numbers/inc-at-pt :wk "Increment number")
   "g-" '(evil-numbers/dec-at-pt :wk "Decrement number")))

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
  :config
  (setq completion-styles '(orderless basic)))

(use-package vertico-directory
  :after vertico
  :straight nil
  :general
  (:keymaps 'vertico-map
            "RET" 'vertico-directory-enter
            "DEL" 'vertico-directory-delete-char))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
  :init
  (setq doom-modeline-modal nil
        doom-modeline-major-mode-icon nil
        doom-modeline-buffer-encoding 'nondefault)
  :config
  (doom-modeline-mode 1))

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

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :hook ((org-mode . visual-line-mode)
         (org-mode . my/org-center-title)
         (org-mode . my/org-hide-properties))
  :general
  (:keymaps 'org-mode-map
   :states '(normal insert)
   "TAB" 'org-cycle
   "M-<return>" 'org-meta-return)
  (leader-def
   :keymaps 'org-mode-map
   "n s" '(org-narrow-to-subtree :wk "Subtree"))
  :config
  (setq org-startup-indented t
        org-src-preserve-indentation t
        org-hidden-keywords '(title)
        org-hide-emphasis-markers t
        org-M-RET-may-split-line nil
        org-edit-src-content-indentation 0
        org-duration-format 'h:mm
        org-startup-folded 'showall
        org-clock-mode-line-total 'today)
  (plist-put org-format-latex-options :scale 2.2))

(use-package org-tempo
  :straight nil
  :after org)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-title
  :after org
  :straight nil)

(use-package multi-vterm
  :general
  (leader-def "o t" '(multi-vterm-dedicated-toggle :wk "Toggle terminal"))
  (:keymaps 'vterm-mode-map
   :states 'insert
   "C-<right>" 'multi-vterm-next
   "C-<left>" 'multi-vterm-prev
   "C-d" 'vterm--self-insert)
  :config
  (evil-set-initial-state 'vterm-mode 'insert)
  (setq multi-vterm-dedicated-window-height-percent 30)
  (use-package vterm))

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
  :general (:states 'insert
  "C-<tab>" 'company-complete)
  :init
  (setq company-idle-delay nil)
  (global-company-mode))

(use-package docker-tramp)

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
   "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer"))
   )
  (:keymaps 'evil-inner-text-objects-map
   "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
   "a" (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))
  (:states '(normal visual)
   "[f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer" t))
   "]f" (lambda () (interactive) (evil-textobj-tree-sitter-goto-textobj "function.outer")))
)

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
  :general (leader-def
    "c a" '(lsp-execute-code-action :wk "Code Action")
    "c l" '(:keymap lsp-command-map :wk "lsp"))
    (:states 'insert
     :keymaps 'lsp-mode-map
     "M-j" 'lsp-signature-next
     "M-k" 'lsp-signature-previous)
  :config
  (setq lsp-headerline-breadcrumb-enable nil
        lsp-signature-doc-lines 1
        lsp-lens-enable nil))

(use-package lsp-ui
  :hook lsp-mode
  :config
  (setq lsp-diagnostics-attributes '()
        lsp-ui-doc-enable nil))

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'")

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :hook (lua-mode . lsp-deferred)
  :init
  (setq lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server"))

(use-package python
  :hook (python-mode . lsp-deferred)
  :straight nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode))

(use-package lsp-pyright
  :after python
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection '("pyright-langserver" "--stdio"))
                    :multi-root lsp-pyright-multi-root
                    :initialized-fn (lambda (workspace)
                                      (with-lsp-workspace workspace
                                                          (lsp--set-configuration
                                                           (make-hash-table :test 'equal))))
                    :major-modes '(python-mode)
                    :remote? t
                    :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                                   ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                                   ("pyright/endProgress" 'lsp-pyright--end-progress-callback))
                    :server-id 'pyright-remote)))

(use-package pyvenv
  :commands (pyvenv-activate pyvenv-workon))

(use-package cython-mode
  :mode "\\.pyx\\'")

(setq c-basic-offset 4)
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'c++-mode-hook 'lsp-deferred)

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package js
  :mode ("\\.js\\'" . js-mode)
  :hook (js-mode . lsp-deferred))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred))

(use-package web-mode
  :mode "\\.vue\\'"
  :mode "\\.html?\\'"
  :hook (web-mode . lsp-deferred)
  :config
  (add-hook 'web-mode-hook (defun vue-settings ()
                (when (string-suffix-p ".vue" buffer-file-name)
                  (setq-local web-mode-style-padding 0
                              web-mode-script-padding 0)))))

(use-package emmet-mode
  :hook web-mode
  :general
  (:keymaps 'emmet-mode-keymap
   "TAB" 'emmet-expand-line))

(use-package haskell-mode
  :mode "\\.hs\\'"
  :hook (haskell-mode . lsp-deferred)
        (haskell-mode . interactive-haskell-mode)
  :config
  (setq haskell-interactive-popup-errors nil))

(use-package lsp-haskell
  :after (haskell-mode lsp))

(use-package clojure-mode
  :mode "\\.cljs?\\'"
  :hook (clojure-mode . lsp-deferred)
        (clojurescript-mode . lsp-deferred))
(use-package cider
  :after clojure-mode)

(use-package glsl-mode
  :mode "\\.\\(?:vert\\|frag\\|glsl\\|geom\\)\\'")

(use-package kotlin-mode
  :mode ".kt")

(use-package sonic-pi
  :mode ("\\.spi\\'" . sonic-pi-mode)
  :init
  (setq sonic-pi-path "/usr/lib/sonic-pi/"
        sonic-pi-server-bin "server/bin/sonic-pi-server.rb")
  :general
  (:keymaps 'sonic-pi-mode-map
            "C-c C-s" (defun my/sonic-pi-stop-live-loop ()
                        (interactive)
                        (save-excursion
                          (re-search-backward "live_loop \\(:[^, ]+\\)")
                          (sonic-pi-osc-send-command-with-arg4 "save-and-run-buffer" "sonicpi-emacs" (buffer-name)
                                                               (format "live_loop %s do stop end" (match-string 1))
                                                               (buffer-name))
                          (hlt-highlight-region (match-beginning 1) (match-end 1) 'eval-sonic-pi-flash)
                          (run-at-time flash-time nil 'hlt-unhighlight-region)))))

(use-package nim-mode
  :mode "\\.nim\\(?:s|ble|\\.cfg\\)?\\'"
  :hook (nim-mode . lsp-deferred)
  :init
  (add-to-list 'exec-path "/home/moss/.nimble/bin"))

(use-package zig-mode
  :mode "\\.zig\\'"
  :hook (zig-mode . lsp-deferred)
  :init
  (setq zig-format-on-save nil))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package mcf-mode
  :straight nil
  :mode "\\.mcfunction\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package fennel-mode
  :mode "\\.fnl\\'")

(use-package projectile
  :general (leader-def
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
  (defun my/projectile-find-file ()
    (interactive)
    (if (projectile-project-p)
          (call-interactively 'projectile-find-file)
        (call-interactively 'projectile-persp-switch-project)))
  :general
  (:keymaps 'projectile-command-map
   "p" '(projectile-persp-switch-project :wk "Switch project"))
  (leader-def "SPC" '(my/projectile-find-file :wk "Find in project")))

(use-package magit
  :general (leader-def
    "g" '(magit-status :wk "Magit"))
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(use-package yasnippet
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after yasnippet)
