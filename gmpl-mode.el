;;; gmpl-mode.el --- Major mode for editing GMPL(MathProg) files

;; Copyright (C) 2015  Junpeng Qiu

;; Author: Junpeng Qiu <qjpchmail@gmail.com>
;; Keywords: extensions

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defvar gmpl-mode-hook nil)

(defconst gmpl-font-lock-keywords
  (list
   ;; Reserved keywords
   (cons (concat "\\_<\\("
                 (mapconcat 'identity
                            '("and" "else" "mod" "union"
                              "by" "if" "not" "within"
                              "cross" "in" "or"
                              "diff" "inter" "symdiff"
                              "div" "less" "then")
                            "\\|")
                 "\\)\\_>")
         font-lock-keyword-face)
   ;; Keywords in statement
   (cons (concat "\\_<\\("
                 (mapconcat 'identity
                            '("maximize" "minimize"
                              "dimen" "default" "integer" "binary" "symbolic"
                              "for" "check" "table" "IN" "OUT")
                            "\\|")
                 "\\)\\_>")
         '(1 font-lock-keyword-face))
   ;; One keyword per line
   (cons "^[ \t]*\\(data\\|end\\|solve\\)[ \t]*;"
         '(1 font-lock-keyword-face))
   ;; Subject to, not a word
   (cons "^[ \t]*\\(s\.t\.\\|subject to\\|subj to\\)[ \t\r\n]"
         '(1 font-lock-keyword-face))
   ;; `set', `param', `var' keywords use `font-lock-type-face'
   (cons "^[ \t]*\\_<\\(set\\|param\\|var\\)\\_>" '(1 font-lock-type-face))
   ;; `display' and `printf' keywords use `font-lock-builtin-face'
   (cons "\\_<\\(display\\|printf\\)\\_>" '(1 font-lock-builtin-face))
   ;; Iterated-operator, overriding face for `min' and `max'
   (cons "\\_<\\(sum\\|prod\\|min\\|max\\|setof\\|forall\\|exists\\)\\_>\\([ \t]*{\\)"
         '(1 font-lock-builtin-face))
   ;; Functions
   (cons (concat "\\_<\\("
                 (mapconcat 'identity
                            '(;; Numeric
                              "abs" "atan" "card" "ceil" "cos"
                              "exp" "floor" "gmtime" "length"
                              "log" "log10" "max" "min" "round"
                              "sin" "sqrt" "str2time" "trunc"
                              "Irand224" "Uniform01" "Uniform"
                              "Normal01" "Normal"
                              ;; Symbolic
                              "substr" "time2str")
                            "\\|")
                 "\\)\\_>")
         font-lock-function-name-face)
   ;; Variable name
   (cons (concat "^[ \t]*\\("
                 (mapconcat 'identity
                            '("set" "param" "var" "maximize" "minimize" "table"
                              "s\.t\." "subject to" "subj to")
                            "\\|")
                 "\\)[ \t]+\\([a-zA-Z0-9_]+\\)[ \t,;:{]")
         '(2 font-lock-variable-name-face))
   ;; Variable name can also start with itself and followed by `:'
   (cons "^[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*:" '(1 font-lock-variable-name-face))))


(defvar gmpl-mode-syntax-table
  (let ((syn-tab (make-syntax-table)))
    ;; `_' is part of a symbol
    (modify-syntax-entry ?_ "_" syn-tab)
    ;; `-' is not part of a word
    (modify-syntax-entry ?- "." syn-tab)
    ;; String literals
    (modify-syntax-entry ?' "\"" syn-tab)
    ;; Comments
    (modify-syntax-entry ?# "<" syn-tab)
    (modify-syntax-entry ?/ ". 14" syn-tab)
    (modify-syntax-entry ?* ". 23b" syn-tab)
    (modify-syntax-entry ?\n ">" syn-tab)
    syn-tab)
  "Syntax table for gmpl-mode.")


(defun gmpl-mode ()
  "Major mode for editing GMPL(MathProg) files."
  (interactive)
  (kill-all-local-variables)
  (set (make-local-variable 'font-lock-defaults) '(gmpl-font-lock-keywords))
  (set-syntax-table gmpl-mode-syntax-table)
  (setq major-mode 'gmpl-mode)
  (setq mode-name "gmpl")
  (run-hooks 'gmpl-mode-hook))

(provide 'gmpl-mode)
;;; gmpl-mode.el ends here
