;;; firebase-rules-mode.el --- Editing support for firebase.rules -*- lexical-binding: t; -*-

;; Author: Darrel Herbst <dherbst@gmail.com>
;; URL: https://github.com/dherbst/firebase-rules-mode
;; Version: 1.0
;; Keywords: languages
;; Package-Requires: ((emacs "24.3"))

;; Copyright (C) 2024 Darrel Herbst

;; This program is free software: you can redistribute it and/or modify
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

;; Provides syntax highlighting for Firebase security rules files version 2,
;; cloud firestore and cloud storage only.  The rules language is documented at
;; https://firebase.google.com/docs/rules/rules-language.  Indention and
;; grammar validation is not currently provided, contributions are welcome.

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
