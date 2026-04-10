;;; -*- lexical-binding: t; -*-
;; (use-package lsp-nix
;;   :after (lsp-mode)
;;   :demand t
;;   :custom
;;   (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :hook (nix-mode . eglot-ensure) )
