;;; -*- lexical-binding: t; -*-

(defvar-local python--source-window nil
  "In an inferior Python buffer, the window we jumped from.")

(defun python--go-to-python-window ()
  (interactive)
  (let* ((python-buffer (process-buffer (python-shell-get-process-or-error "No inferior python process")))
         (python-window (get-buffer-window python-buffer))
         (current-window (selected-window))
         )
    (if python-window
        (progn
          (select-window python-window)
          (setq python--source-window current-window)))))

(defun python--return-to-source-window ()
  (interactive)
  (when (window-live-p python--source-window)
    (select-window python--source-window)))

(use-package python
  :custom
  (python-shell-interpreter "python3")
  :bind (:map python-mode-map
              ("M-`" . python--go-to-python-window)
              :map inferior-python-mode-map
              ("M-`" . python--return-to-source-window)
              ("C-c C-k" . comint-clear-buffer))
  )


(use-package apheleia
  :defer t
  :config
  (setf (alist-get 'python-mode apheleia-mode-alist)
        '(ruff))
  )
