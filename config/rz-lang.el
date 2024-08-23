;; rz-lang.el --- Programming language setup

(use-package cmake-mode
  :mode "CMakeLists\\.txt\\'")

(use-package lua-mode
  :mode "\\.lua\\'"
  :interpreter "lua"
  :init
  (setq lsp-clients-lua-language-server-bin "/usr/bin/lua-language-server"))

(use-package python
  :straight nil
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :general
  (:keymaps 'python-mode-map
            "C-c C-c" (defun rz/python-send-dwim ()
                        (interactive)
                        (cond
                         ((use-region-p) (call-interactively 'python-shell-send-region))
                         ((python-info-current-defun) (python-shell-send-defun))
                         (t (python-shell-send-statement))))
            "C-c C-b" 'python-shell-send-buffer)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   (cons '(python . t) org-babel-load-languages))
  (use-package pyvenv
    :commands (pyvenv-activate pyvenv-workon)
    :config
    (defun pyvenv-workon-home ()
      (expand-file-name "~/.local/share/venvs"))))

(defun rz/setup-c-mode ()
  (c-set-offset 'innamespace 0)
  (setq c-basic-offset 4))
(add-hook 'c-mode-hook 'rz/setup-c-mode)
(add-hook 'c++-mode-hook 'rz/setup-c-mode)
(add-hook 'glsl-mode-hook 'rz/setup-c-mode)

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package js
  :mode ("\\.js\\'" . js-mode))

(use-package typescript-mode
  :mode "\\.tsx?\\'")

(use-package web-mode
  :mode "\\.vue\\'"
  :mode "\\.html?\\'"
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
  :hook (haskell-mode . interactive-haskell-mode)
  :config
  (setq haskell-interactive-popup-errors nil))

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
  :init
  (add-to-list 'exec-path "/home/moss/.nimble/bin"))

(use-package zig-mode
  :mode "\\.zig\\'"
  :init
  (setq zig-format-on-save nil))

(use-package dockerfile-mode
  :mode "Dockerfile\\'")

(use-package mcf-mode
  :straight nil
  :mode "\\.mcfunction\\'")

(use-package bolt-mode
  :straight nil
  :mode "\\.bolt\\'")

(use-package go-mode
  :mode "\\.go\\'")

(use-package fennel-mode
  :mode "\\.fnl\\'")

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

(use-package odin-mode
  :straight (odin-mode :type git :host github :repo "mattt-b/odin-mode")
  :mode "\\.odin\\'")

(use-package glsl-mode
  :mode "\\.gdshader\\'")

(use-package gdscript-mode
  :mode "\\.gd\\'")

(provide 'rz-lang)
