;; rz-visual.el --- All visual changes to emacs

(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tooltip-mode 0)
(blink-cursor-mode 0)
(setq default-frame-alist
      '((vertical-scroll-bars . nil)
        (internal-border-width . 40)
        (left-fringe . 15)
        (right-fringe . 5)
        (tool-bar-lines . 0)
        (menu-bar-lines . 0)))
(setq-default line-spacing 5)
(pixel-scroll-precision-mode 1)
(setq mouse-wheel-progressive-speed nil
      pixel-scroll-precision-use-momentum t)
(setq frame-title-format "%b - Emacs")

(use-package elegance
  :straight nil
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
                                            "-" "="))))
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

(use-package hl-todo
  :straight nil
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces '(("TODO" warning bold)
                                ("NOTE" success bold))))
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

(use-package popper
  :demand t
  :general
  (leader-def
    "k" '(popper-toggle :wk "Toggle popup")
    "K" '(popper-cycle :wk "Cycle popups"))
  :config
  (setq popper-reference-buffers '((compilation-mode . hide)
                                   helpful-mode
                                   eshell-mode
                                   vterm-mode
                                   inferior-python-mode
                                   "\\*Python\\*"
                                   "\\*vterminal"
                                   "\\*org-roam\\*"
                                   "\\*rg\\*"
                                   "\\*lsp-help\\*")

        popper-group-function 'popper-group-by-perspective
        popper-mode-line nil)
  (popper-mode 1)
  (popper-echo-mode 1))

(provide 'rz-visual)
