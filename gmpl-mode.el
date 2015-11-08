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

;;                              _____________

;;                                GMPL-MODE

;;                               Junpeng Qiu
;;                              _____________


;; Table of Contents
;; _________________

;; 1 Overview
;; 2 Usage
;; 3 *TODO*


;; Major mode for editing GMPL(MathProg) files.

;; If you are writing GMPL and using GLPK and to solve problems, try this out!

;; 1 Overview
;; ==========

;;   GMPL is a modeling language to create mathematical programming models
;;   which can be used by [GLPK].

;;   Although GMPL is a subset of AMPL, the current Emacs major mode for
;;   AMPL, which can be found at [https://github.com/dpo/ampl-mode],
;;   doesn't work well with GMPL files. Here is the list of the reasons why
;;   `gmpl-mode' works better compared to `ampl-mode' when editing GMPL
;;   files:
;;   1. Support single quoted string and C style comments.
;;   2. Some keywords(such as `for', `end') are highlighted properly, and
;;      it provides better hightlighting generally.
;;   3. A usable indent function.


;;   [GLPK] https://www.gnu.org/software/glpk/


;; 2 Usage
;; =======

;;   First, add the `load-path' and load the file:
;;   ,----
;;   | (add-to-list 'load-path "/path/to/gmpl-mode.el")
;;   | (require 'gmpl-mode)
;;   `----

;;   Use `gmpl-mode' when editing files with the `.mod' extension:
;;   ,----
;;   | (add-to-list 'auto-mode-alist '("\\.mod\\'" . gmpl-mode))
;;   `----


;; 3 *TODO*
;; ========

;;   - Add some functions to interact with the `glpsol' command line tool.
;;   - Translate LaTeX equations to GMPL format and solve the problem
;;     directly.
;;   - Add company-keywords support.

;;; Code:

(require 'font-lock)

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

(defun gmpl--compute-indent ()
  "Compute the indentation for current line."
  (save-excursion
    (beginning-of-line)
    (if (bobp)
        0
      (if (cond ((looking-at-p "[ \t]*}")
                 (backward-up-list) t)
                ((looking-at-p "[ \t]*\\_<else\\_>")
                 (re-search-backward "\\_<if\\_>" nil t) t))
          (current-indentation)
        (forward-line -1)
        (cond ((looking-at-p ".*;")
               (unless (or (bobp)
                           (save-excursion (forward-line -1) (looking-at-p ".*[;{}][ \t]*$")))
                 (let ((pos (current-indentation)))
                   (while (and (>= (current-indentation) pos) (not (bobp)))
                     (forward-line -1))))
               (current-indentation))
              ((looking-at-p ".*\\(?:[:={]\\|\\_<then\\_>\\|\\_<else\\_>\\)[ \t]*$")
               (+ (current-indentation) tab-width))
              ((looking-at-p ".*:=")
               (search-forward ":=" nil t)
               (skip-chars-forward " \t")
               (current-column))
              (t (current-indentation)))))))

(defun gmpl-indent-line ()
  "Line indent function of `gmpl-mode'."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (gmpl--compute-indent)))
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

;; ------------------------------------------- ;;
;; Variables and functions related to `gplsol' ;;
;; ------------------------------------------- ;;

(defvar gmpl--glpsol-input-file-name (make-temp-file "gmpl-glpsol-input"))
(defvar gmpl--glpsol-output-file-name (make-temp-file "gmpl-glpsol-output"))
(defvar gmpl--glpsol-ranges-file-name (make-temp-file "gmpl-glpsol-ranges"))
(defvar gmpl--glpsol-temp-buffer-name "*glpsol*")
(defvar gmpl--separator-width 80)
(defvar gmpl--separator-char ?=)

(defvar gmpl-glpsol-program "glpsol"
  "The program name of `glpsol'.
If `glpsol' is not in your PATH, you may need to specify the
exact location of `glpsol'.")

(defvar gmpl-glpsol-extra-args nil
  "Extra arguments passed to `glpsol' command line tool.")

(defvar gmpl--glpsol-buffer-font-lock-keywords
  `((,(concat "^" (regexp-opt '("Problem" "Rows" "Columns"
                                "Non-zeros" "Status" "Objective"
                                "Time used" "Memory used") t)
              ":")
     1 font-lock-type-face)
    (,(format "^%c.*%c+$\\|^End of .*$" gmpl--separator-char gmpl--separator-char)
     0 font-lock-comment-face)
    ("OPTIMAL LP SOLUTION FOUND" 0 font-lock-keyword-face)
    ("PROBLEM HAS NO PRIMAL FEASIBLE SOLUTION" 0 font-lock-warning-face)
    (".*\\<GLPK\\>.*" 0 font-lock-builtin-face)))

(defun gmpl--generate-separator (width fill-char text)
  "Generate separator string specified by WIDTH, FILL-CHAR and TEXT."
  (setq text (format " %s " text))
  (when (> (length text) width)
    (error "The length of the text is larger than the specified width"))
  (let* ((fill-char-width (- width (length text)))
         (left-width (/ fill-char-width  2))
         (right-width (- fill-char-width left-width)))
    (format "%s%s%s"
            (make-string left-width fill-char)
            text
            (make-string right-width fill-char))))

(defun gmpl--maybe-insert-file-with-title (filename title)
  (when (file-exists-p filename)
    (insert (gmpl--generate-separator gmpl--separator-width
                                      gmpl--separator-char
                                      title))
    (newline)
    (insert-file-contents filename)
    (goto-char (point-max))
    (newline)))

(defun gmpl--send-region-to-glpsol (beg end)
  "Send the region from BEG to END to `glpsol'."
  (write-region beg end gmpl--glpsol-input-file-name)
  (let ((extra-args gmpl-glpsol-extra-args))
    (with-current-buffer (get-buffer-create gmpl--glpsol-temp-buffer-name)
      (special-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (gmpl--generate-separator gmpl--separator-width
                                          gmpl--separator-char
                                          "Terminal Output"))
        (newline)
        (insert (shell-command-to-string
                 (concat gmpl-glpsol-program " -m " gmpl--glpsol-input-file-name
                         " -o " gmpl--glpsol-output-file-name
                         " --ranges " gmpl--glpsol-ranges-file-name
                         " " extra-args)))
        (newline)
        (gmpl--maybe-insert-file-with-title gmpl--glpsol-output-file-name "Solution Details")
        (newline)
        (gmpl--maybe-insert-file-with-title gmpl--glpsol-ranges-file-name "Sensitivity Analysis")
        (goto-char (point-min))
        (font-lock-add-keywords nil gmpl--glpsol-buffer-font-lock-keywords)
        (if (fboundp 'font-lock-ensure)
            (font-lock-ensure)
          (with-no-warnings
            (font-lock-fontify-buffer)))))))

(defun gmpl-glpsol-solve-dwim ()
  "Solve the problem using `glpsol'.
If a region is selected, use the region.  Otherwise, the whole
buffer is used."
  (interactive)
  (unless (executable-find gmpl-glpsol-program)
    (error "No `glpsol' program found! Make sure you have `glpsol' available in your system"))
  (if (use-region-p)
      (gmpl--send-region-to-glpsol (region-beginning) (region-end))
    (gmpl--send-region-to-glpsol (point-min) (point-max)))
  (unless (get-buffer-window gmpl--glpsol-temp-buffer-name)
    (display-buffer gmpl--glpsol-temp-buffer-name)))

(provide 'gmpl-mode)
;;; gmpl-mode.el ends here
