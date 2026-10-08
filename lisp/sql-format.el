;;; sql-format.el --- Convert Persistent SQL logs to executable SQL  -*- lexical-binding: t; -*-

(defcustom sql-format-command "sqlformat -ra -k upper -"
  "Command to run to format SQL strings"
  :type 'string
  :group 'sql
  )

(defun sql-format--region (beg end)
  (shell-command-on-region beg end sql-format-command nil t (get-buffer-create "*sqlformat: error*") t)
  (goto-char (point-min))
  (flush-lines "^[[:space:]]*$"))


(defun sql-format ()
  (interactive)
  (if (use-region-p)
      (sql-format--region (region-beginning) (region-end))
    (sql-format--region (point-min) (point-max))))

(provide 'sql-format)
