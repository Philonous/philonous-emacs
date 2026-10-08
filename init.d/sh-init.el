;;; -*- lexical-binding: t; -*-
(use-package sh-script
  :mode ("\\.envrc\\'" . sh-mode)
  :hook
  (sh-mode . eglot-ensure)
  )
