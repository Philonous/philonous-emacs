(defun haskell-mode-init ()
  (paredit-mode 1)
  )

(setq emacs-lisp-mode-hook 'haskell-mode-init)
