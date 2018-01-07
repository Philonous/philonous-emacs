(defun elisp-mode-init ()
  (paredit-mode 1)
  )

(setq emacs-lisp-mode-hook 'elisp-mode-init)
