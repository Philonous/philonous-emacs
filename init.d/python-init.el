;; (setenv "PYTHONPATH" "/usr/lib/python2.7/site-packages")
;; (setq
;;  python-shell-interpreter "python3"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp ">>> ")


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

(defun python-mode-init ()
  ""
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (setq-default  python-shell-interpreter "python3")
  (define-key python-mode-map (kbd "M-`") 'python--go-to-python-window)
  (define-key inferior-python-mode-map (kbd "M-`") 'python--return-to-source-window)
  (define-key inferior-python-mode-map (kbd "C-c C-k") 'comint-clear-buffer)
  (define-key python-mode-map (kbd "M-q") 'lsp-format-buffer)
  ;; (lsp)
  )

(setq python-mode-hook 'python-mode-init)


(use-package lsp-pyright)

;; (defun elpy-mode-init ()
;;   (py-autopep8-enable-on-save)
;;   (when (require 'flycheck nil t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   )

;; (add-hook 'elpy-mode-hook 'elpy-mode-init)
