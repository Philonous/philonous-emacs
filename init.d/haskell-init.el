;;; -*- lexical-binding: t; -*-
(require 'custom-functions)
(require 'haskell-import)
;;; Major mode for persistent entity definitions
(require 'persistent-mode)
(require 'test-counterpart)

(defun haskell-process-restart-and-load-file ()
  "Restart the haskell process and load the file visited by buffer."
  (interactive)
  (haskell-process-restart)
  (haskell-process-load-file))

(defvar haskell-custom-align-rules
  '((haskell-types
     (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-assignment
     (regexp . "\\(\\s-+\\)=\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-arrows
     (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-left-arrows
     (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-guards
     (regexp . "\\(\\s-+\\)|\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-constraints
     (regexp . "\\(\\s-+\\)\\(=>\\|⇒\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-constituent-group
     (regexp . "\\(\\s-+\\)\\(\\[\\|{\\|(\\|,\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-applicative-group
     (regexp . "\\(\\s-+\\)\\(<\\$>\\|<\\*>\\|<\\*\\|\\*>\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-monadic
     (regexp . "\\(\\s-+\\)\\(>>=\\|>>\\|=<<\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-append
     (regexp . "\\(\\s-+\\)\\(<>\\|<|>\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-comments
     (regexp . "\\(\\s-+\\)--\\s-+")
     (modes quote (haskell-mode literate-haskell-mode)))
    (haskell-imports-as
     (regexp . "\\(\\s-+\\)\\(as\\|hiding\\)\\s-+")
     (modes quote (haskell-mode literate-haskell-mode))))
  "Custom align rules for Haskell.")

(defun haskell-set-align-rules ()
  "Install `haskell-custom-align-rules' as buffer-local align rules."
  (set (make-local-variable 'align-rules-list) nil)
  (dolist (rule haskell-custom-align-rules)
    (add-to-list 'align-rules-list rule)))

(defun my/haskell-clear-and-load ()
  "Clear interactive window and load buffer"
  (interactive)
  (haskell-clear-interactive-window)
  (haskell-process-load-file)
  )

(use-package haskell-mode
  :defer t
  :preface
  (defun my/eldoc-use-buffer ()
    (setq-local eldoc-display-functions '(eldoc-display-in-buffer)))

  :custom
  (haskell-indentation-layout-offset 2)
  (haskell-indentation-left-offset 2)
  (haskell-notify-p t)
  (haskell-process-log t)
  (haskell-process-path-cabal "cabal")
  (haskell-process-path-cabal-ghci "cabal")
  (haskell-process-reload-with-fbytecode nil)
  (haskell-process-show-debug-tips nil)
  (haskell-process-suggest-overloaded-strings t)
  (haskell-process-suggest-remove-import-lines nil)
  (haskell-process-suggest-restart nil)
  (haskell-process-type 'stack-ghci)
  (haskell-process-use-presentation-mode nil)
  (haskell-tags-on-save t)

  :bind (:map haskell-mode-map
              ("C-`"         . haskell-go-to-haskell-window)
              ("M-`"         . haskell-go-to-haskell-window)
              ("<f11>"       . layout-for-haskell-choose-by-hostname)
              ("<f8>"        . haskell-navigate-imports)
              ("<backspace>" . backward-delete-char)
              ("C-c C-k"     . haskell-clear-interactive-window)
              ("C-c r"       . haskell-process-restart-and-load-file)
              ("C-c C-x C-t" . haskell-session-change-target)
              ("C-c i"       . haskell-import-lines)
              ("M-]"         . align)
              ("C-c C-q"     . haskell-qualify-import-line)
              ("C-c k"       . swap-underscore-camelcase)
              ("C-c C-d"     . haskell-cabal-add-dependency)
              ("C-c C-t"     . haskell-mode-show-type-at)
              ("C-c C-i"     . haskell-process-do-info)
              ("C-c h"       . section-heading)
              ("<delete>"    . delete-char)
              ("C-c C-l"     . my/haskell-clear-and-load)
              ("C-x p t"     . test-counterpart-haskell-toggle-test))

  :config
  (haskell-set-align-rules)
  (add-to-list 'display-buffer-alist
               '("\\*.*compilation\\*"
                 (my/display-in-compile-target-window)))
  (add-to-list 'display-buffer-alist
               '("\\*eldoc\\*"
                 (my/display-in-compile-target-window)
                 (inhibit-same-window . t)))

  :hook
  ((haskell-mode . subword-mode)
   (haskell-mode . column-number-mode)
   (haskell-mode . haskell-indentation-mode)
   (haskell-mode . my/eldoc-use-buffer)
   ))

(use-package haskell-interactive-mode
  ;; Part of haskell-mode, don't try to install
  :ensure nil
  :after haskell-mode
  :bind (:map haskell-interactive-mode-map
              ("M-`" . haskell-go-to-old-window)
              ("C-`" . haskell-go-to-old-window)))

(use-package project
  :config
  (add-to-list 'project-vc-extra-root-markers "hie.yaml")
  (add-to-list 'project-vc-extra-root-markers "*.cabal")
  (add-to-list 'project-vc-extra-root-markers "stack.yaml")
  )

(defun apheleia-first-available (&rest commands)
  "Return the first COMMANDS whose executable is in PATH.
Each element of COMMANDS is a list (PROGRAM . ARGS)."
  (or (cl-loop for cmd in commands
               when (executable-find (car cmd))
               return cmd)
      (error "No formatter found among: %s"
             (mapcar #'car commands))))


(use-package apheleia
  :defer t
  :config
  (setf (alist-get 'haskell-mode apheleia-mode-alist)
        '(project-haskell-formatter)
        (alist-get 'project-haskell-formatter apheleia-formatters)
        '((apheleia-first-available
           (list "project-haskell-formatter-stdin" buffer-file-name)
           (list "fourmolu" "--stdin" buffer-file-name))))
  )

(defun haskell-init--eglot-dwim ()
  (interactive)
  (if (locate-dominating-file default-directory "hie.yaml")
      (eglot-ensure)))

(use-package eglot
  :hook
  ((haskell-mode . haskell-init--eglot-dwim)))
