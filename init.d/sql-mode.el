(require 'custom-functions)
(require 'sql)
(require 'comint)

(defvar sql--process-old-window nil)

(defun sql--go-to-sql-buffer ()
  (interactive)
  (if sql-buffer
      (let* ((sql-window (get-buffer-window sql-buffer)))
        (if sql-window
            (progn
              (setq sql--process-old-window (selected-window))
              (select-window sql-window))))))

(defun sql--go-to-old-window ()
  (interactive)
  (if sql--process-old-window
      (select-window sql--process-old-window)))


(defun sql--clear-interactive-buffer ()
  "Clear the process buffer that's associated with the current buffer"
  (interactive)
  (if sql-buffer
      (with-current-buffer sql-buffer
        (comint-clear-interactive-buffer)
        )))

(defun sql-mode-init ()
  (define-key sql-mode-map (kbd "M-`") 'sql--go-to-sql-buffer)
  (define-key sql-mode-map (kbd "C-c C-k") 'sql--clear-interactive-buffer)
  )


(defun custom-sql-interactive-mode-hook ()
    (define-key sql-interactive-mode-map (kbd "C-c C-k")
      'comint-clear-interactive-buffer )
    (define-key sql-interactive-mode-map (kbd "M-`")
      'sql--go-to-old-window )
    (sql-set-product-feature 'postgres :prompt-regexp "^[-[:alnum:]_]*=[#>] ")
    (sql-set-product-feature 'postgres :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] "))


(add-hook 'sql-mode-hook 'custom-sql-interactive-mode-hook)
(add-hook 'sql-mode-hook 'sql-mode-init)

(defun sql-comint-postgres-docker (product options &optional buf-name)
  "Create comint buffer and connect to Postgres."
  ;; username and password are ignored.  Mark Stosberg suggests to add
  ;; the database at the end.  Jason Beegan suggests using --pset and
  ;; pager=off instead of \\o|cat.  The later was the solution by
  ;; Gregor Zych.  Jason's suggestion is the default value for
  ;; sql-postgres-options.
  (let ((params
         (append
          (list  "exec" "-it")
          (if (not (string= "" sql-container))
              (list sql-container)
            (error "container name must not be empty"))
          (list "psql")
          (if (not (= 0 sql-port))
              (list "-p" (number-to-string sql-port)))
          (if (not (string= "" sql-user))
              (list "-U" sql-user))
          (if (not (string= "" sql-server))
              (list "-h" sql-server))
          options
          (if (not (string= "" sql-database))
              (list sql-database)))))
    (sql-comint product params buf-name)))

(setq sql-product-alist (assoc-delete-all 'postgres-docker sql-product-alist))

(sql-add-product 'postgres-docker "Postgres-docker"
                 `(:free-software t
                   :font-lock sql-mode-postgres-font-lock-keywords
                   :sqli-program "docker"
                   :sqli-options sql-postgres-options
                   :sqli-login sql-postgres-login-params
                   :sqli-comint-func sql-comint-postgres-docker
                   :list-all '("\\d+" . "\\dS+")
                   :list-table '("\\d+ %s" . "\\dS+ %s")
                   :completion-object sql-postgres-completion-object
                   :prompt-regexp "^[-[:alnum:]_]*=[#>] "
                   ;; :prompt-regexp "^[[:alnum:]_]*=[#>] "  ; Old one from postgres. Doesn't work with hyphens
                   :prompt-length 5
                   ;; :prompt-cont-regexp "^[[:alnum:]_]*[-(][#>] " ; doesn't work with hyphens
                   :prompt-cont-regexp "^[-[:alnum:]_]*[-(][#>] "
                   :input-filter sql-remove-tabs-filter
                   :terminator '("\\(^\\s-*\\\\g$\\|;\\)" . "\\g")))
