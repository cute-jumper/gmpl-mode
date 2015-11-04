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

(eval-when-compile
  (require 'regexp-opt))

(defconst gmpl-font-lock-keywords
  (eval-when-compile
    `( ;; Reserved keywords
      (,(regexp-opt '("and" "else" "mod" "union"
                      "by" "if" "not" "within"
                      "cross" "in" "or"
                      "diff" "inter" "symdiff"
                      "div" "less" "then")
                    'symbols)
       (1 font-lock-keyword-face))
      ;; Keywords in statement
      (,(regexp-opt
         '("maximize" "minimize"
           "dimen" "default" "integer" "binary" "symbolic"
           "for" "check" "table" "IN" "OUT")
         'symbols)
       (1 font-lock-keyword-face))
      ;; One keyword per line
      (,(concat "^[ \t]*" (regexp-opt '("data" "end" "solve") t) "[ \t]*;")
       (1 font-lock-keyword-face))
      ;; Subject to, not a word
      (,(concat "^[ \t]*" (regexp-opt '("s\.t\." "subject to" "subj to") t) "[ \t\r\n]")
       (1 font-lock-keyword-face))
      ;; `set', `param', `var' keywords use `font-lock-type-face'
      (,(concat "^[ \t]*" (regexp-opt '("set" "param" "var") 'symbols))
       (1 font-lock-type-face))
      ;; `display' and `printf' keywords use `font-lock-builtin-face'
      (,(regexp-opt '("display" "printf") 'symbols)
       (1 font-lock-builtin-face))
      ;; Iterated-operator, overriding face for `min' and `max'
      (,(concat (regexp-opt '("sum" "prod" "min" "max" "setof" "forall" "exists")
                            'symbols)
                "\\([ \t]*{\\)")
       (1 font-lock-builtin-face))
      ;; Functions
      (,(regexp-opt '( ;; Numeric
                      "abs" "atan" "card" "ceil" "cos"
                      "exp" "floor" "gmtime" "length"
                      "log" "log10" "max" "min" "round"
                      "sin" "sqrt" "str2time" "trunc"
                      "Irand224" "Uniform01" "Uniform"
                      "Normal01" "Normal"
                      ;; Symbolic
                      "substr" "time2str")
                    'symbols)
       (1 font-lock-function-name-face))
      ;; Variable name
      (,(concat "^[ \t]*"
                (regexp-opt
                 '("set" "param" "var" "maximize" "minimize" "table"
                   "s\.t\." "subject to" "subj to")
                 t)
                "[ \t]+\\([a-zA-Z0-9_]+\\)[ \t,;:{]")
       (2 font-lock-variable-name-face))
      ;; Variable name can also start with itself and followed by `:'
      ("^[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*:" (1 font-lock-variable-name-face))))
  "Keywords for highlighting.")

(defvar gmpl-indent-width 4
  "Indent width in `gmpl-mode'.")

(defun gmpl-indent-line ()
  "Line indent function of `gmpl-mode'."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent 0))
    (save-excursion
      (beginning-of-line)
      (unless (bobp)
        (forward-line -1)
        (if (looking-at ".*\\(?:[:={]\\|\\_<then\\_>\\)[ \t]*$")
            (setq indent (+ (current-indentation) tab-width))
          (setq indent (current-indentation)))))
    (if savep
        (save-excursion
          (indent-line-to indent))
      (indent-line-to indent))))

(defvar gmpl-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; `_' is part of a symbol
    (modify-syntax-entry ?_ "_" st)
    ;; `-' is not part of a word
    (modify-syntax-entry ?- "." st)
    ;; String literals
    (modify-syntax-entry ?' "\"" st)
    ;; Comments
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?/ ". 14" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for gmpl-mode.")

;;;###autoload
(define-derived-mode gmpl-mode fundamental-mode "gmpl"
  "Major mode for editing GMPL(MathProg) files."
  :syntax-table gmpl-mode-syntax-table
  ;; font-lock
  (set (make-local-variable 'font-lock-defaults) '(gmpl-font-lock-keywords))
  ;; indent
  (set (make-local-variable 'tab-width) gmpl-indent-width)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'gmpl-indent-line))

(provide 'gmpl-mode)
;;; gmpl-mode.el ends here
