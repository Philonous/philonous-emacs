;;; -*- lexical-binding: t; -*-
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :diminish)


(defun my/elisp-flymake-setup ()
  "Set up flymake for Emacs Lisp: byte-compile with our load-path, no checkdoc."
  (remove-hook 'flymake-diagnostic-functions #'elisp-flymake-checkdoc t)
  (setq-local elisp-flymake-byte-compile-load-path load-path)
  (flymake-mode 1))

(use-package elisp-mode
  :ensure nil
  :custom (trusted-content
           ( list
             ;; trusted-content-p compares paths after calling
             ;; abbreviated-file-name on the buffer file name. So
             ;; we need to do the same here.
             (abbreviate-file-name (expand-file-name "lisp/" user-emacs-directory))
             (abbreviate-file-name (expand-file-name "init.d/" user-emacs-directory)))
           )
  :hook (emacs-lisp-mode . my/elisp-flymake-setup))
