
(defun restclient-init ()
  (define-key restclient-mode-map (kbd "C-c C-v") 'restclient-http-send-current)
  (define-key restclient-mode-map (kbd "C-c C-c") (lambda () (interactive)
                                                    (restclient-http-send-current-stay-in-window)
                                                    (restclient-jump-next)
                                                    ))
  )

(defvar restclient-last-response-status nil
  "Store the last response" )

(defvar restclient-last-response-body nil
  "Store the last response" )

(defun restclient-save-last-response ()
  (beginning-of-buffer)
  (unless (looking-at "HTTP[^ ]+[ ]\\([0-9]+\\)[ ]") (error "Could not parse response"))
  (setq restclient-last-response-status (string-to-number (match-string 1)))
  (re-search-forward "^$")
  (forward-line)
  (setq restclient-last-response-body (buffer-substring (point) (point-max)))
  )

(defun restclient-lookup-response (&rest keys)
  (let* ((json-object-type 'alist)
         (json-key-type 'string)
         (j (json-read-from-string restclient-last-response-body))
         (s restclient-last-response-status)
         )
    (unless (and (<= 200 s) (< s 300)) (error "Last response wasn't success"))
    (while keys
      (setq j (cdr (assoc (pop keys) j)))
      (unless j (error "Key lookup failed")))
    j))

(add-hook 'restclient-response-received-hook 'restclient-save-last-response)

(add-hook 'restclient-mode-hook 'restclient-init)
