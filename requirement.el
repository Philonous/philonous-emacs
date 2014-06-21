;; Major mode for working with RFC requirement tables.

(defun requirements-goto-field (n)
  "go to beginning of nth field"
  (beginning-of-line)
  (dotimes (i n)
    (search-forward "|" (line-end-position) t)
    (if (= (point) (line-end-position))
        (return nil))))

(defun requirements-field-end ()
  "find end of field"
  (save-excursion
    (search-forward "|" (line-end-position) t)
    (forward-char -1)
    (point)))

(defun requirements-set-field (n text)
  (save-excursion
    (requirements-goto-field n)
    (delete-region (point) (requirements-field-end))
    (insert text))
  (org-table-align))

(defun requirements-get-field (n)
  (save-excursion
    (requirements-goto-field n)
    (let* ((field (buffer-substring-no-properties
                   (point)
                   (requirements-field-end))))
      (progn
        (string-match "[ ]*\\([^ ].*[^ ]\\)[ ]*" field)
        (or (match-string 1 field) "")))))

(defun requirements-get-rfc-line ()
  "get line in RFC file pointed to by current entry"
    (let* ((field (requirements-get-field 1))
           (line (if (string-match "^[0-9]+$" field)
                     (match-string 0 field)
                   nil)))
      (if line (string-to-number line) nil)))

(defun requirements-read-rfc-file ()
  (let* ((rfc-file (read-file-name "rfc file: " nil nil t))
         (rfc-buffer (find-file-noselect rfc-file)))
    (when rfc-buffer
      (save-selected-window
        (setq requirements-rfc-buffer rfc-buffer)
        (switch-to-buffer-other-window rfc-buffer)
        (with-current-buffer rfc-buffer
          (requirements-highlight-requirements))))
    rfc-buffer))

(defun requirements-get-rfc-buffer ()
  (or requirements-rfc-buffer (requirements-read-rfc-file)))

(defun requirements-go-to-line-at-point-other-buffer ()
  "Got to line at point in other buffer"
  (interactive)
  (let* ((rfc-buffer (requirements-get-rfc-buffer))
         (rfc-window (if rfc-buffer (get-buffer-window rfc-buffer nil) nil)))
    (when rfc-window
        (let ((line-number (requirements-get-rfc-line)))
          (if line-number
              (with-selected-window rfc-window
                (goto-char (point-min))
                (forward-line (1- line-number))
                (recenter))
            (message "Entry doesn't have a line number " ))))))

(defun requirements-find-next-interesting-line (count)
  "find a line that has CHECK status"
  (interactive "p")
  (let* ((direction (signum count))
         (amount (abs count))
         (boundary (if (> direction 0) (point-max) (point-min))))
    (dotimes (i amount)
      (forward-line direction)
      (let ((case-fold-search nil))
        (while (not (or (= (point) boundary)
                        (string-match "CHECK" (requirements-get-field 3))))
          (forward-line direction))))
    (if (= (point) boundary) nil
      (progn
        (requirements-go-to-line-at-point-other-buffer)
        t ))))

(defun requirements-find-previous-interesting-line (count)
  (interactive "p")
  (requirements-find-next-interesting-line (- count)))

(defun requirements-next-relevant-line (count)
  (interactive "p")
  (let* ((direction (signum count))
         (amount (abs count))
         (boundary (if (> direction 0) (point-max) (point-min))))
    (dotimes (i amount)
      (forward-line direction)
      (while (not (or (requirements-get-rfc-line)
                      (= (point) boundary)))
        (forward-line direction)))
    (if (= (point) boundary) nil
      (progn
        (requirements-go-to-line-at-point-other-buffer)
        t ))))

(defun requirements-previous-relevant-line (count)
  (interactive "p")
  (requirements-next-relevant-line (- count)))

(defun requirements-set-task (to)
  (requirements-set-field 3 to)
  (requirements-next-relevant-line 1)
)

(defun requirements-set-done ()
  "set task to done"
  (interactive)
  (requirements-set-task "DONE") )

(defun requirements-set-NA ()
  (interactive)
  (requirements-set-task "NA"))

(defun requirements-set-NI ()
  (interactive)
  (requirements-set-task "NOT IMPLEMENTED"))

(defun requirements-set-check ()
  (interactive)
  (requirements-set-task "CHECK"))

(defun requirements-set-user ()
  (interactive)
  (requirements-set-task "USER"))

(define-derived-mode requirement-mode org-mode "requirement"
  "Mode for RFC requirement tracking"
  (progn
    (set (make-local-variable 'requirements-rfc-buffer) nil)
    (define-key requirement-mode-map (kbd "M-q") 'requirements-go-to-line-at-point-other-buffer)
    (define-key requirement-mode-map (kbd "M-C-d") 'requirements-set-done)
    (define-key requirement-mode-map (kbd "M-C-n") 'requirements-set-NA)
    (define-key requirement-mode-map (kbd "M-C-o") 'requirements-set-NI)
    (define-key requirement-mode-map (kbd "M-C-c") 'requirements-set-check)
    (define-key requirement-mode-map (kbd "M-C-u") 'requirements-set-user)
    (define-key requirement-mode-map (kbd "C-n")   'requirements-next-relevant-line)
    (define-key requirement-mode-map (kbd "C-p")   'requirements-previous-relevant-line)
    (define-key requirement-mode-map (kbd "M-n")   'requirements-find-next-interesting-line)
    (define-key requirement-mode-map (kbd "M-p")   'requirements-find-previous-interesting-line)
    (define-key requirement-mode-map (kbd "C-<Tab>") 'other-window)))


(defun requirements-highlight-requirements ()
  "Highlight RFC requirements in buffer"
  (interactive)
  (font-lock-mode t)
  (highlight-regexp "MUST\\(?: NOT\\)?\\|SHALL\\(?: NOT\\)?\\|REQUIRED" 'hi-red-b)
  (highlight-regexp "SHOULD\\(?: NOT\\)?\\|RECOMMENDED" 'my-yellow-b)
  (highlight-regexp "MAY\\|OPTIONAL" 'hi-green-b)
)


(provide 'requirement)
