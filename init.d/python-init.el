;;; -*- lexical-binding: t; -*-

(defun python--go-to-python-window ()
  (interactive)
  (let* ((python-buffer (process-buffer (python-shell-get-process-or-error "No inferior python process")))
         (python-window (get-buffer-window python-buffer))
         (current-window (selected-window))
         )
    (if python-window
        (progn
          (select-window python-window)
          (set (make-local-variable 'source-buffer) current-window)))))

(defun python--return-to-source-window ()
  (interactive)
  (if source-buffer
      (select-window source-buffer)))

(use-package python
  :custom
  (python-shell-interpreter "python3")
  :bind (:map python-mode-map
              ("M-`" . python--go-to-python-window)
              ;; ("M-q" . lsp-format-buffer)
              :map inferior-python-mode-map
              ("M-`" . python--return-to-source-window)
              ("C-c C-k" . comint-clear-buffer))
  )


;; (defun elpy-mode-init ()
;;   (py-autopep8-enable-on-save)
;;   (when (require 'flycheck nil t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   )

;; (add-hook 'elpy-mode-hook 'elpy-mode-init)
