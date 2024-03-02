(use-package general
  :config
  (general-create-definer leader-def
    :prefix "SPC"
    :non-normal-prefix "M-SPC"
    :keymaps 'override
    :states '(normal visual insert emacs motion))
  (general-create-definer localleader-def
    :prefix ","
    :non-normal-prefix "M-,"
    :major-modes t
    :states '(normal visual motion emacs insert)))

(provide 'rz-general)
