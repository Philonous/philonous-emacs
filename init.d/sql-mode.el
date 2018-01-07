(require 'custom-functions)

(defun custom-sql-interactive-mode-hook ()
    (define-key sql-interactive-mode-map (kbd "C-c C-k")
      'comint-clear-interactive-buffer )
    )


(add-hook 'sql-interactive-mode-hook 'custom-sql-interactive-mode-hook)
