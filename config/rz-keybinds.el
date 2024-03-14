;; rz-keybinds.el --- General keybind configuration
(require 'general)

(defvar-local rz/compile-func 'recompile "Function to run when compiling.")
(defun rz/compile () (interactive) (funcall rz/compile-func))
(leader-def
  ";" '(pp-eval-expression :wk "Eval Elisp")
  ":" '(execute-extended-command :wk "M-x")
  "<return>" '(bookmark-jump :wk "Jump to Bookmark")

  "o" '(:ignore t :wk "open")
  "o t" '(eshell :wk "Open eshell")

  "c" '(:ignore t :wk "code")
  "c c" '(rz/compile :wk "Recompile")
  "c C" '(compile :wk "Compile")

  "f" '(:ignore t :wk "file")
  "f s" '(save-buffer :wk "Save file")
  "f f" '(find-file :wk "Find file")
  "f D" `(,(defun rz/delete-file ()
              (interactive)
              (when (y-or-n-p "Are you sure you want to delete this file?")
                 (delete-file buffer-file-name))) :wk "Delete file")
  "f u"  `(,(defun rz/sudo-open-file ()
               "Opens current file with sudo"
               (interactive)
               (unless buffer-file-name
                  (user-error "Buffer is not associated with any file"))
               (find-file (concat "/sudo::" (expand-file-name buffer-file-name)))) :wk "Open current file with sudo")

  "b" '(:ignore t :wk "buffer")
  "b d" '(kill-current-buffer :wk "Kill buffer")
  "b b" '(switch-to-buffer :wk "Switch buffers")
  "b r" `(,(defun rz/revert-buffer () (interactive) (revert-buffer nil (not (buffer-modified-p)))) :wk "Revert buffer")

  "n" '(:ignore t :wk "narrow")
  "n w" '(widen :wk "Widen")
  "n f" '(narrow-to-defun :wk "Function")
  "n r" '(narrow-to-region :wk "Region")

  "t" `(,(defun rz/open-todo-file () (interactive) (find-file (expand-file-name "~/org/todo.org"))) :wk "Open Todo"))

(general-def
  :keymaps 'override
  "ESC" 'keyboard-escape-quit)
(general-def
  :states '(normal visual insert)
  "C-=" 'text-scale-increase
  "C--" 'text-scale-decrease)
(general-def
  :states 'insert
  "C-<backspace>" (defun rz/greedy-delete ()
                    (interactive)
                    (let ((beg-of-whitespace (save-excursion
                                                (skip-chars-backward " \t" (point-at-bol))
                                                (point))))
                       (if (equal (point) beg-of-whitespace)
                          (call-interactively 'backward-kill-word)
                         (delete-region beg-of-whitespace (point))))))

(provide 'rz-keybinds)
