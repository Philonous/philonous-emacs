;;; -*- lexical-binding: t; -*-
(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode)
  :diminish)


(use-package flymake
  :custom (trusted-content
           ( list
             ;; trusted-content-p compares paths after calling
             ;; abbreviated-file-name on the buffer file name. So
             ;; we need to do the same here.
             (abbreviate-file-name (expand-file-name "lisp/" user-emacs-directory))
             (abbreviate-file-name (expand-file-name "init.d/" user-emacs-directory))))
  :hook ((emacs-lisp-mode . flymake-mode)
         (emacs-lisp-mode . (lambda ()
                              (remove-hook 'flymake-diagnostic-functions
                                           #'elisp-flymake-checkdoc t)))
         (emacs-lisp-mode . (lambda ()
                              (setq-local elisp-flymake-byte-compile-load-path load-path)))
         ))
