;; rz-lang.el --- Programming language setup

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
  :interpreter ("python" . python-mode)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (cons '(python . t) org-babel-load-languages)))
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

(defun rz/setup-c-mode ()
  (lsp-deferred)
  (c-set-offset 'innamespace 0)
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'rz/setup-c-mode)
(add-hook 'c++-mode-hook 'rz/setup-c-mode)

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
  (add-hook 'web-mode-hook (defun vue-settings ())
                (when (string-suffix-p ".vue" buffer-file-name)
                  (setq-local web-mode-style-padding 0
                              web-mode-script-padding 0))))
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

(use-package sonic-pi
  :mode ("\\.spi\\'" . sonic-pi-mode)
  :init
  (setq sonic-pi-path "/usr/lib/sonic-pi/"
        sonic-pi-server-bin "server/bin/sonic-pi-server.rb")
  :general
  (:keymaps 'sonic-pi-mode-map
            "C-c C-s" (defun rz/sonic-pi-stop-live-loop ()
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

(use-package lsp-java
  :demand t
  :hook (java-mode . lsp-deferred))
(use-package uva
  :straight nil
  :commands (uva-find-pdf)
  :general
  (leader-def
    "u" '(:ignore t :wk "uva")
    "u p" '(uva-find-pdf :wk "Go to pdf")
    "u s" '(uva-find-solution :wk "Go to solution")
    "u r" '(uva-run :wk "Run")
    "u R" '(uva-run-interactive :wk "Run interactively"))
  :config
  (setq doc-view-continuous t))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode))

(use-package erlang
  :mode ("\\.erl\\'" . erlang-mode))

(provide 'rz-lang)
