;;; bolt-mode.el --- Major mode for editing Bolt files -*- lexical-binding: t -*-

;;; Code:
(require 'font-lock)

(defgroup bolt nil
  "Major mode for editing Bolt files."
  :group 'languages)

(defconst bolt-command-list
  '("advancement"
    "attribute"
    "ban"
    "ban-ip"
    "banlist"
    "bossbar"
    "clear"
    "clone"
    "damage"
    "data"
    "datapack"
    "debug"
    "defaultgamemode"
    "deop"
    "difficulty"
    "effect"
    "enchant"
    "execute"
    "experience"
    "fill"
    "fillbiome"
    "forceload"
    "function"
    "gamemode"
    "gamerule"
    "give"
    "help"
    "item"
    "jfr"
    "kick"
    "kill"
    "list"
    "locate"
    "loot"
    "me"
    "msg"
    "op"
    "pardon"
    "pardon-ip"
    "particle"
    "perf"
    "place"
    "playsound"
    "publish"
    "recipe"
    "reload"
    "random"
    "return"
    "ride"
    "save-all"
    "save-off"
    "save-on"
    "say"
    "schedule"
    "scoreboard"
    "seed"
    "setblock"
    "setidletimeout"
    "setworldspawn"
    "spawnpoint"
    "spectate"
    "spreadplayers"
    "stop"
    "stopsound"
    "summon"
    "tag"
    "team"
    "teammsg"
    "teleport"
    "tell"
    "tellraw"
    "time"
    "title"
    "tm"
    "tp"
    "trigger"
    "w"
    "weather"
    "whitelist"
    "worldborder"
    "xp"))

(defconst bolt-execute-subcommand-list
    '("align" "anchored" "as" "at" "facing" "in" "on" "positioned" "rotated" "store" "summon" "if" "unless" "run"))

(defconst bolt-keywords
    '("macro" "from" "import" "yield" "with" "function_tag"))

(defvar bolt-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n "> " st)
    (dolist (c (string-to-list ".:")) (modify-syntax-entry c "_" st))
    st))

(defvar bolt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'bolt-electric-delete)
    map))

;;;###autoload
(define-derived-mode bolt-mode prog-mode "Bolt"
  "Set major mode for editing Bolt files."
  :group 'bolt
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local indent-line-function 'bolt-indent-line)
  (setq-local syntax-propertize-function
              (syntax-propertize-rules ((rx (group "#") (not space)) (1 "_")))))

(font-lock-add-keywords 'bolt-mode
        ; Keywords, with an optional $ before them
      `(,(rx symbol-start (? (seq bol (* space) "$")) (regexp (regexp-opt (append bolt-execute-subcommand-list bolt-keywords bolt-command-list))) symbol-end)

        ; Entity selectors
        ("@[aeprsn]" . 'font-lock-variable-use-face)

        ; Highlight macro marker on bol and all macro variables
        (,(rx bol (* space) "$")
          (0 font-lock-variable-name-face t)
          (,(rx "$(" (* word) ")") nil nil (0 'font-lock-variable-use-face)))

        ; If macro marker not present on bol, highlight all macro vars with error
        (,(rx bol (* space) (not "$")) ,(rx "$(" (* word) ")") nil nil (0 error)) 

        ; Filters for entity selectors
        (,(rx "[" (group (* (not "]"))) "]") ; Anchor - []
          (,(rx bow (* word) (* space) "=") ; Match all keys within []
            (progn (goto-char (match-beginning 0)) (match-end 0))
            nil
            (0 font-lock-type-face))))
         
      'set)

(defun bolt-compute-indent ()
  "Calculate the indentation for the current line."
  (save-excursion
    (beginning-of-line)
    (if (= (point) (point-min)) 0
      (forward-line -1)
      (while (and (looking-at "^ *$") (> (point) (point-min))) (forward-line -1))
      (+ (current-indentation) (if (looking-at ".*: *$") tab-width 0)))))
    
(defun bolt-indent-line ()
  "Indent the current line."
  (interactive "*")
  (indent-line-to (bolt-compute-indent)))

(defun bolt-electric-delete (arg)
  "Delete characters or unindent."
  (interactive "*p")
  (if (or (/= (current-indentation) (current-column)) (bolp))
      (backward-delete-char-untabify arg)
    (let ((ci (current-column)))
      (beginning-of-line)
      (delete-horizontal-space)
      (indent-to (* (- (/ (1- ci) tab-width) arg -1) tab-width)))))

(provide 'bolt-mode)
;;; bolt-mode.el ends here
