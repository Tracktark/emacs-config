(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-safe-remote-resources
   '("\\`https://fniessen\\.github\\.io/org-html-themes/org/theme-readtheorg\\.setup\\'"))
 '(reb-re-syntax 'string)
 '(safe-local-variable-values
   '((eval add-hook 'before-save-hook
           (lambda nil
             (org-map-entries
              (lambda nil
                (when
                    (org-get-todo-state)
                  (org-id-get-create)))))
           nil t)
     (eval add-hook 'after-save-hook 'org-babel-tangle nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (load buffer-file-name))
           nil t)
     (eval add-hook 'after-save-hook
           (lambda nil
             (load buffer-file-name)))
     (eval add-hook 'after-save-hook
           (lambda nil
             (load
              (expand-file-name "elegance.el" user-emacs-directory))))
     (eval add-hook 'after-save-hook #'org-babel-tangle nil t))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
