(defvar ess-process-old-window nil)

(defun ess-go-to-r-window ()
    (interactive)
  (let* ((r-buffer (get-buffer "*R*"))
         (r-window (get-buffer-window r-buffer)))
    (setq ess-process-old-window (selected-window))
    (select-window r-window)))

(defun ess-go-back-to-source-window ()
    (interactive)
  (select-window ess-process-old-window))

(defun clear-shell ()
   (interactive)
   (let ((old-max comint-buffer-maximum-size))
     (setq comint-buffer-maximum-size 0)
     (comint-truncate-buffer)
     (setq comint-buffer-maximum-size old-max)))


(use-package ess
  :pin melpa-stable
  :config
  (add-hook 'ess-mode-hook 'r-mode-init
   )
  )

(defun r-mode-init ()
  (interactive)
  (require 'smartparens)
  (smartparens-mode t)
  (column-number-mode t)
  (setq inferior-ess-program "R")
  (setq inferior-R-program-name "R")
  (setq ess-local-process-name "R")
  (setq ess-ask-for-ess-directory nil)
  (setq ess-use-ido t)
  (setq inferior-R-args "--no-save --no-restore")

  (define-key ess-mode-map (kbd "M-`") 'ess-go-to-r-window)
  (define-key ess-mode-map (kbd "_") 'self-insert-command)
  (define-key ess-mode-map (kbd "M-RET") 'ess-eval-buffer-from-beg-to-here)

  (define-key inferior-ess-mode-map (kbd "M-`") 'ess-go-back-to-source-window)
  (define-key inferior-ess-mode-map (kbd "C-c C-k") 'clear-shell)
  (define-key inferior-ess-mode-map (kbd "_") 'self-insert-command)
  )
