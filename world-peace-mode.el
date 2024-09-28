;;; world-peace-mode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 Lukáš Hozda
;;
;; Author: Lukáš Hozda <luk.hozda@gmail.com>
;; Maintainer: Lukáš Hozda <luk.hozda@gmail.com>
;; Created: září 28, 2024
;; Modified: září 28, 2024
;; Version: 0.0.1
;; Keywords: extensions languages
;; Homepage: https://github.com/luciusmagn/world-peace
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar world-peace-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\" "\"" table)
    table))

(defvar world-peace-font-lock-keywords
  `(
    (,(regexp-opt '("dec" "end" "ret" "do" "by" "case" "num" "load" "if" "else") 'words)
     . font-lock-keyword-face)
    (,(regexp-opt '("<--" "<-" "+" "-" "*" "/" "%" "<<" ">>" "&" "^" "|" "==" "!=" "<" ">" "<=" ">=" "&&" "||" "=" "+=" "-=" "*=" "/=" "%=" "&=" "|=" "^=" "<<=" ">>=") t)
     . font-lock-builtin-face)
    ("\\b[0-9]+\\b" . font-lock-constant-face)
    ("\\b0[xX][0-9A-Fa-f_]+\\b" . font-lock-constant-face)
    ("\\b0[bB][01_]+\\b" . font-lock-constant-face)
    ("\\b0[oO][0-7_]+\\b" . font-lock-constant-face)
    ("\\<dec\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-function-name-face)
    ("\\<num\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" 1 font-lock-variable-name-face)
    ("^\\s-*---" . font-lock-comment-delimiter-face)  ; Highlight triple dash
    ))

(defun world-peace-indent-line ()
  "Indent current line as World Peace code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*end\\>")
          (progn
            (save-excursion
              (forward-line -1)
              (setq cur-indent (- (current-indentation) 3)))
            (if (< cur-indent 0)
                (setq cur-indent 0)))
        (save-excursion
          (while not-indented
            (forward-line -1)
            (if (looking-at "^[ \t]*end\\>")
                (progn
                  (setq cur-indent (current-indentation))
                  (setq not-indented nil))
              (if (looking-at "^[ \t]*\\(dec\\|do\\|if\\|else\\|case\\)\\>")
                  (progn
                    (setq cur-indent (+ (current-indentation) 3))
                    (setq not-indented nil))
                (if (bobp)
                    (setq not-indented nil)))))))
      (if cur-indent
          (indent-line-to cur-indent)
        (indent-line-to 0)))))

(defun world-peace-electric-dash ()
  "Insert triple dash and newline if inside a function body."
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at "^\\s-*---"))
      (progn
        (newline-and-indent)
        (insert "---")
        (world-peace-indent-line))
    (insert "-")))

;;;###autoload
(define-derived-mode world-peace-mode prog-mode "World Peace"
  "Major mode for editing World Peace programming language code."
  :syntax-table world-peace-mode-syntax-table
  (setq font-lock-defaults '(world-peace-font-lock-keywords))
  (setq comment-start "// ")
  (setq comment-end "")
  (setq comment-start-skip "\\(//+\\|/\\*+\\)\\s *")
  (setq-local indent-line-function 'world-peace-indent-line)
  (define-key world-peace-mode-map (kbd "-") 'world-peace-electric-dash))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wp\\'" . world-peace-mode))

;; Add support for Org Babel
(add-to-list 'org-src-lang-modes '("world-peace" . world-peace))

(provide 'world-peace-mode)
;;; world-peace-mode.el ends here
