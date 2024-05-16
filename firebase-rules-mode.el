;;; firebase-rules-mode.el --- Editing support for firebase.rules -*- lexical-binding: t; -*-

;; Author: Darrel Herbst <dherbst@gmail.com>
;; URL: https://github.com/dherbst/firebase-rules-mode
;; Version: 1.0
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;; MIT License

;; Copyright (c) 2024 Darrel Herbst

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Provides syntax highlighting for Firebase security rules files https://firebase.google.com/docs/rules/basics

;;; Code:

(defvar firebase-rules-mode-highlights
  (let ((keywords '("allow" "false" "if" "match" "rules_version" "service" "true"))
        (builtins '("create" "delete" "get" "list" "read" "update" "write")))

    `((,(regexp-opt keywords 'words) . font-lock-keyword-face)
      (,(regexp-opt builtins 'words) . font-lock-builtin-face))))

(defvar firebase-rules-mode-syntax-table
  (let ((table (copy-syntax-table
		(standard-syntax-table))))
    (modify-syntax-entry ?/ ". 124" table)
    (modify-syntax-entry ?\r "> " table)
    (modify-syntax-entry ?\n "> " table)
    table))


;;;###autoload
(define-derived-mode firebase-rules-mode fundamental-mode "Firebase-Rules"
  "Major mode for Firebase security rules"
  (set-syntax-table firebase-rules-mode-syntax-table)
  (setq-local comment-start "//")
  (setq-local comment-use-syntax t)
  (setq font-lock-defaults '(firebase-rules-mode-highlights)))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.rules\\'" . firebase-rules-mode))

(provide 'firebase-rules-mode)
;;; firebase-rules-mode.el ends here
