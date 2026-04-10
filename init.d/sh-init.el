;;; -*- lexical-binding: t; -*-
(use-package sh-script
  :config
  (add-to-list 'auto-mode-alist '("\\.envrc\\'" . sh-mode))
  :hook
  (sh-mode . eglot-ensure)
  )
