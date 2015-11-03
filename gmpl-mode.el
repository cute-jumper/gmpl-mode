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
  (list (cons (concat "\\_<\\("
                      (mapconcat 'identity
                                 '("and" "else" "mod" "union"
                                   "by" "if" "not" "within"
                                   "cross" "in" "or"
                                   "diff" "inter" "symdiff"
                                   "div" "less" "then")
                                 "\\|")
                      "\\)\\_>")
              font-lock-keyword-face)))


(defvar gmpl-mode-syntax-table
  (let ((syn-tab (make-syntax-table)))
    ;; Word
    (modify-syntax-entry ?_ "w" syn-tab)
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
  (set (make-local-variable 'font-lock-defaults) '(gmpl-font-lock-keywords-reserved))
  (set-syntax-table gmpl-mode-syntax-table)
  (setq major-mode 'gmpl-mode)
  (setq mode-name "gmpl")
  (run-hooks 'gmpl-mode-hook))

(provide 'gmpl-mode)
;;; gmpl-mode.el ends here
