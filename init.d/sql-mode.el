(require 'custom-functions)

(defun custom-sql-interactive-mode-hook ()
    (define-key sql-interactive-mode-map (kbd "C-c C-k")
      'comint-clear-interactive-buffer )
    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
    (sql-set-product-feature 'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] "))


(add-hook 'sql-mode-hook 'custom-sql-interactive-mode-hook)
