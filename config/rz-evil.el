;; rz-evil.el --- Evil mode setup

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
   "w s" '(evil-window-split :wk "HSplit window")))

(use-package evil-nerd-commenter
  :general
  (:states '(normal visual)
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

(provide 'rz-evil)
