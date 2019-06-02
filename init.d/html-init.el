(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package tagedit
  :config
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features)
  :hook (html-mode . tagedit-mode))

(setq sgml-quick-keys 'close)
