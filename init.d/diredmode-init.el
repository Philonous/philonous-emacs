
(defun dired-mode-init ()
  (define-key dired-mode-map (kbd "<f7>") 'make-directory)
  (define-key dired-mode-map (kbd "C-c w") 'wdired-change-to-wdired-mode)
)


(add-hook 'dired-mode-hook 'dired-mode-init)
