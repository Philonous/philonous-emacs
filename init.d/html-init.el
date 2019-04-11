(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package tagedit
  :config
  (tagedit-add-paredit-like-keybindings)
  :hook html-mode)
