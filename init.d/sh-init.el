;;; -*- lexical-binding: t; -*-
(use-package sh-script
  :mode ("\\.envrc\\'" . sh-mode)
  :custom
  (sh-basic-offset 2)
  :hook
  (sh-mode . eglot-ensure)
  )
