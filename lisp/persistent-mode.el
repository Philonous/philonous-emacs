;;; persistent-mode.el --- major mode for haskell persistent schema declarations  -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'rx))


(defconst persistent-mode-syntax-table
  (let* ((table (copy-syntax-table)))
    (modify-syntax-entry ?\( "()" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "w" table)
    (modify-syntax-entry ?-  "< 12" table)
    (modify-syntax-entry ?\n ">" table)
    table)
  "Peristsnet mode syntax table."
  )


(defconst persistent-mode-keywords
  '("Primary" "Foreign" "deriving"))

(defvar persistent-font-lock-defaults
  `((
     ;; stuff between "
     ("\"\\.\\*\\?" . font-lock-string-face)
     ;; ; : , ; { } =>  @ $ = are all special elements
     ( ,(regexp-opt persistent-mode-keywords) . font-lock-keyword-face)
     ( "^[ ]+[[:upper:]]\\w*" . 'font-lock-constant-face)
     ( "^[[:upper:]]\\w*" . 'haskell-definition-face)
     ( "\\b[[:upper:]]\\w*" . 'haskell-constructor-face)
     )))

(define-derived-mode persistent-mode prog-mode "persistent-mode"
  "Major mode for editing persistent shema definitions"
  :syntax-table persistent-mode-syntax-table
  (setq comment-start "-- ")
  (setq comment-end "")
  (setq font-lock-defaults persistent-font-lock-defaults)
)

(provide 'persistent-mode)
