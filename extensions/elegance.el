;;; elegance.el --- A simple and elegant theme
;;; Commentary:
;; This file defines a theme, which uses as little color based syntax highlighting as possible,
;; and instead relies on font weight.  This should improve readability and elegance, especially on light themes.
;;; Code:
(defgroup eleface '()
  "Faces and colors for the Eleface theme."
  :group 'faces)

(defvar elegance-theme-change-hook nil
  "Hook called after a theme change.")
(defvar elegance-current-theme
  (if (string-match-p "true" (shell-command-to-string "awesome-client 'return require\"ui.darkmode\":isActive()'")) 'dark 'light)
  "Currently selected theme.")

(defcustom eleface-font-size 14
  "Font size."
  :type 'integer
  :group 'eleface)

(defcustom eleface-font-family "FiraCode Nerd Font"
  "Font family used for monospace faces."
  :type 'string
  :group 'eleface)

(defcustom eleface-font-family-prop "Roboto"
  "Font family used for proportional faces."
  :type 'string
  :group 'eleface)

(defcustom eleface-foreground-color "#000000" "" :type 'color :group 'eleface)
(defcustom eleface-background-color "#ffffff" "" :type 'color :group 'eleface)
(defcustom eleface-popout-color     "#ffab91" "" :type 'color :group 'eleface)
(defcustom eleface-salient-color    "#673ab7" "" :type 'color :group 'eleface)
(defcustom eleface-faded-color      "#b0bec5" "" :type 'color :group 'eleface)
(defcustom eleface-subtle-color     "#eceff1" "" :type 'color :group 'eleface)


(defface eleface-default nil
  "Used for regular information."
  :group 'eleface)

(defface eleface-strong nil
  "Used for names, and visually important information."
  :group 'eleface)

(defface eleface-popout nil
  "Used for keywords."
  :group 'eleface)

(defface eleface-salient nil
  "Used for keywords."
  :group 'eleface)

(defface eleface-faded nil
  "Used for comments and less important information."
  :group 'eleface)

(defface eleface-subtle nil
  "Used for region and selections."
  :group 'eleface)

(defun set-face (face style &rest args)
  "Reset FACE and make it inherit STYLE.
ARGS are passed to `set-face-attribute'."
  (apply 'set-face-attribute face nil
                      :foreground 'unspecified :background 'unspecified
                      :family     'unspecified :slant      'unspecified
                      :weight     'unspecified :height     'unspecified
                      :underline  'unspecified :overline   'unspecified
                      :box        'unspecified :inherit    style
                      args))

(defun elegance-refresh ()
 (set-frame-font (format "%s %d" eleface-font-family eleface-font-size))
 (set-face-attribute 'eleface-default nil
                     :foreground eleface-foreground-color
                     :background eleface-background-color
                     :family eleface-font-family
                     :height (* eleface-font-size 10))

 (set-face-attribute 'eleface-strong nil
                     :foreground (face-foreground 'eleface-default)
                     :weight 'bold)

 (set-face-attribute 'eleface-salient nil
                     :foreground eleface-salient-color
                     :weight 'regular)

 (set-face-attribute 'eleface-popout nil
                     :foreground eleface-popout-color
                     :weight 'regular)

 (set-face-attribute 'eleface-faded nil
                     :foreground eleface-faded-color
                     :weight 'semi-light)

 (set-face-attribute 'eleface-subtle nil
                     :background eleface-subtle-color)

 (set-foreground-color (face-foreground 'eleface-default))
 (set-background-color (face-background 'eleface-default))

 (set-face 'default 'eleface-default)
 (set-face 'bold 'eleface-strong)
 (set-face 'italic 'eleface-faded)
 (set-face 'bold-italic 'eleface-strong)
 (set-face 'region 'eleface-subtle)
 (set-face 'highlight 'eleface-subtle)
 (set-face 'secondary-selection 'eleface-subtle)
 (set-face 'cursor 'eleface-default
           :background (face-foreground 'eleface-default))

 (set-face 'variable-pitch 'eleface-default
           :family eleface-font-family-prop)

 (set-face-attribute 'warning nil :foreground (if (eq elegance-current-theme 'light) "#df8e1d" "#eed49f"))
 (set-face-attribute 'error nil :foreground (if (eq elegance-current-theme 'light) "#d20f39" "#ed8796"))
 (set-face-attribute 'success nil :foreground (if (eq elegance-current-theme 'light) "#40a02b" "#a6da95"))
 (set-face 'shadow 'eleface-faded)
 (set-face 'link 'eleface-salient)
 (set-face 'fringe 'eleface-faded)
 (set-face 'isearch 'eleface-strong)
 (set-face 'isearch-fail 'eleface-faded)
 (set-face 'lazy-highlight 'eleface-subtle)
 (set-face 'show-paren-match 'eleface-popout)

 (set-face 'font-lock-comment-face 'eleface-faded)
 (set-face 'font-lock-doc-face 'eleface-faded)
 (set-face 'font-lock-string-face 'eleface-popout)
 (set-face 'font-lock-constant-face 'eleface-salient)
 (set-face 'font-lock-warning-face 'eleface-popout)
 (set-face 'font-lock-function-name-face 'eleface-strong)
 (set-face 'font-lock-variable-name-face 'eleface-strong
           :weight 'semi-bold)
 (set-face 'font-lock-builtin-face 'eleface-salient)
 (set-face 'font-lock-type-face 'eleface-salient)
 (set-face 'font-lock-keyword-face 'eleface-salient)

 (set-face 'minibuffer-prompt 'eleface-strong)

 (set-face-attribute 'mode-line nil
                     :foreground (face-foreground 'eleface-default)
                     :background (face-background 'eleface-subtle)
                     :overline nil
                     :underline nil
                     :box nil)
 (set-face-attribute 'mode-line-inactive nil
                     :foreground (face-foreground 'eleface-default)
                     :background (face-background 'eleface-subtle)
                     :overline nil
                     :underline nil
                     :box nil)

 (set-face-attribute 'internal-border nil
                     :background (face-background 'eleface-default))

; Company
 (with-eval-after-load 'company
   (set-face 'company-tooltip-selection '(eleface-strong eleface-subtle))
   (set-face 'company-tooltip 'eleface-subtle)
   (set-face-attribute 'company-scrollbar-fg nil :background (face-foreground 'eleface-default))
   (set-face-attribute 'company-scrollbar-bg nil :background (face-foreground 'eleface-faded))
   (set-face 'company-tooltip-common 'eleface-faded)
   (set-face 'company-tooltip-common-selection '(eleface-strong eleface-subtle)))

; Orderless
 (with-eval-after-load 'orderless
   (set-face 'orderless-match-face-0 'eleface-faded)
   (set-face 'orderless-match-face-1 'eleface-faded)
   (set-face 'orderless-match-face-2 'eleface-faded)
   (set-face 'orderless-match-face-3 'eleface-faded))

; Org
 (with-eval-after-load 'org
   (set-face 'org-archived                            'eleface-faded)
   (set-face 'org-block                             'eleface-default
             :extend t)
   (set-face 'org-block-begin-line                    'eleface-faded
             :extend t)
   (set-face 'org-block-end-line                      'eleface-faded
             :extend t)
   (set-face 'org-checkbox                            'eleface-faded)
   (set-face 'org-checkbox-statistics-done            'eleface-faded)
   (set-face 'org-checkbox-statistics-todo            'eleface-faded)
   (set-face 'org-clock-overlay                       'eleface-faded)
   (set-face 'org-column                              'eleface-faded)
   (set-face 'org-column-title                        'eleface-faded)
   (set-face 'org-date                                'eleface-faded)
   (set-face 'org-date-selected                       'eleface-faded)
   (set-face 'org-default                             'eleface-faded)
   (set-face 'org-document-info                       'eleface-faded)
   (set-face 'org-document-info-keyword               'eleface-faded)
   (set-face 'org-document-title                      'eleface-strong
             :height 1.5)
   (set-face 'org-done                              'eleface-default)
   (set-face 'org-drawer                              'eleface-faded)
   (set-face 'org-ellipsis                            'eleface-faded)
   (set-face 'org-footnote                            'eleface-faded)
   (set-face 'org-formula                             'eleface-faded)
   (set-face 'org-headline-done                       'eleface-faded)
   (set-face 'org-latex-and-related                   'eleface-faded)

   (dolist (f '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6 org-level-7 org-level-8))
     (set-face f 'eleface-strong :height 1.1))
    
   (set-face 'org-link                              'eleface-salient
             :underline t)
   (set-face 'org-list-dt                             'eleface-strong
             :weight 'semi-bold)
   (set-face 'org-macro                               'eleface-faded)
   (set-face 'org-meta-line                           'eleface-faded)
   (set-face 'org-mode-line-clock                     'eleface-faded)
   (set-face 'org-mode-line-clock-overrun             'eleface-faded)
   (set-face 'org-priority                            'eleface-faded)
   (set-face 'org-property-value                      'eleface-faded)
   (set-face 'org-quote                               'eleface-faded)
   (set-face 'org-scheduled                           'eleface-default)
   (set-face 'org-scheduled-previously                'org-warning)
   (set-face 'org-scheduled-today                     'eleface-default)
   (set-face 'org-sexp-date                           'eleface-faded)
   (set-face 'org-special-keyword                     'eleface-faded)
   (set-face 'org-table                               'eleface-faded)
   (set-face 'org-tag                                'eleface-popout)
   (set-face 'org-tag-group                           'eleface-faded)
   (set-face 'org-target                              'eleface-faded)
   (set-face 'org-time-grid                           'eleface-faded)
   (set-face 'org-todo                              'eleface-salient)
   (set-face 'org-upcoming-deadline                 'eleface-default)
   (set-face 'org-verbatim                           'eleface-popout)
   (set-face 'org-verse                               'eleface-faded)
   (set-face 'org-warning                            'eleface-popout
             :foreground (if (eq elegance-current-theme 'light) "#df8e1d" "#eed49f")))


 (with-eval-after-load 'org-agenda
   (set-face 'org-agenda-structure 'eleface-salient
             :weight 'bold)
   (set-face 'org-agenda-date 'eleface-salient)
   (set-face 'org-agenda-date-weekend 'eleface-salient :weight 'semi-bold)
   (set-face 'org-agenda-date-today 'eleface-salient
             :weight 'bold)
   (set-face 'org-agenda-structure-filter 'eleface-popout))

 (with-eval-after-load 'doom-modeline
   (set-face 'doom-modeline-bar 'mode-line))

 (with-eval-after-load 'vertico
   (set-face 'vertico-current 'eleface-subtle))

 (with-eval-after-load 'web-mode
   (set-face 'web-mode-html-tag-face 'eleface-strong)
   (set-face 'web-mode-html-tag-bracket-face 'eleface-default)
   (set-face 'web-mode-html-attr-name-face 'eleface-default))

 (with-eval-after-load 'tree-sitter-hl
   (set-face 'tree-sitter-hl-face:operator 'eleface-default)
   (set-face 'tree-sitter-hl-face:function.call 'eleface-strong)
   (set-face 'tree-sitter-hl-face:property 'eleface-default)
   (set-face 'tree-sitter-hl-face:number 'eleface-default)
   (set-face 'tree-sitter-hl-face:type.builtin 'eleface-salient)
   (set-face 'tree-sitter-hl-face:type 'eleface-salient)))
  


(defun elegance-set-theme (theme)
  "Set a theme to THEME."
  (interactive (list (intern (completing-read "Theme: " '(light dark)))))
  (cond ((eq theme 'light)
         ;; (setq eleface-foreground-color "#000000")
         ;; (setq eleface-background-color "#ffffff")
         ;; (setq eleface-popout-color     "#ffab91")
         ;; (setq eleface-salient-color    "#673ab7")
         ;; (setq eleface-faded-color      "#b0bec5")
         ;; (setq eleface-subtle-color     "#eceff1"))
         (setq eleface-foreground-color "#4c4f69")
         (setq eleface-background-color "#eff1f5")
         (setq eleface-popout-color     "#40a02b")
         (setq eleface-salient-color    "#179299")
         (setq eleface-faded-color      "#8c8fa1")
         (setq eleface-subtle-color     "#dce0e8"))
        ((eq theme 'dark)
         (setq eleface-foreground-color "#cad3f5")
         (setq eleface-background-color "#24273a")
         (setq eleface-popout-color     "#a6da95")
         (setq eleface-salient-color    "#8aadf4")
         (setq eleface-faded-color      "#8087a2")
         (setq eleface-subtle-color     "#363a4f"))
        (t
         (user-error "Invalid theme")))
  (setq elegance-current-theme theme)
  (elegance-refresh)
  (run-hooks 'elegance-theme-change-hook))
(elegance-set-theme elegance-current-theme)
(provide 'elegance)
;;; elegance.el ends here
;; Local Variables:
;; eval: (add-hook 'after-save-hook (lambda () (load buffer-file-name)) nil t)
;; End:
