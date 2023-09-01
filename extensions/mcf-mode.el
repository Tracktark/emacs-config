;;; mcf-mode.el --- Major mode for editing Minecraft mcfunction -*- lexical-binding: t -*-

;; Copyright (C) 2019 rasensuihei

;; Author: rasensuihei <rasensuihei@gmail.com>
;; URL: https://github.com/rasensuihei/mcf-mode
;; Version: 0.2.2
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The main features of this mode are Minecraft mcfunction syntax
;; highlighting.

;; Settings example
;; (require 'mcf-mode)

;; See also:
;; https://github.com/rasensuihei/mcf-mode

;;; Code:
(require 'font-lock)

(defgroup mcf nil
  "Major mode for editing minecraft mcfunction."
  :group 'languages)

(defconst mcf-command-list
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

(defconst mcf-execute-subcommand-list
    '("align" "anchored" "as" "at" "facing" "in" "on" "positioned" "rotated" "store" "summon" "if" "unless" "run"))

(defvar mcf-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st))

;;;###autoload
(define-derived-mode mcf-mode prog-mode "Minecraft-Function"
  "Set major mode for editing Minecraft mcfunction file."
  :group 'mcf
  (setq-local comment-start "#")
  (setq-local comment-end ""))

(font-lock-add-keywords 'mcf-mode
      `((,(rx (or (seq bol (? "$")) "run") (* space) (regexp (regexp-opt mcf-command-list t)) (or eol space)) . 1)

        ("execute" ,(regexp-opt mcf-execute-subcommand-list 'words) nil nil (0 font-lock-keyword-face))

        ("@[aeprs]" . 'font-lock-variable-use-face)
        ("\\sw+\\s-*=" . font-lock-type-face)
        ("^\\$" "\\$(\\sw+)" nil nil (0 'font-lock-variable-use-face))
        ("^[^$]" "\\$(\\sw+)" nil nil (0 'error))
        ("^\\$" . font-lock-variable-name-face))
      'set)


(provide 'mcf-mode)
;;; mcf-mode.el ends here
