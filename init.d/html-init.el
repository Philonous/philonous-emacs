(add-to-list 'company-backends 'company-tern)

(defun html-mode-init ()
  (require 'tagedit)
  (tagedit-mode 1)
  (tagedit-add-paredit-like-keybindings)
  )

(add-hook 'html-mode-hook #'html-mode-init )
