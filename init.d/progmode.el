;;; -*- lexical-binding: t; -*-
;; General configuration for shared development packages lives here.
;; Language-specific parts (hooks, formatters, server settings) stay in
;; the language's init file, in a secondary `:ensure nil' block.

(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :diminish)

(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c C-a C-a" . eglot-code-actions)
        ("C-c C-q" . eglot-format-buffer)
        )
  :config
  ;; LSP hover uses markdown-mode for fontification which is slow when the hower is large
  ;; Emacs can hang for seconds while it is rendering
  ;; This is a hack that skips formatting the markup when it is large
  ;; TODO: Report the hang as bug or check if it is already fixed.
  ;; This might break in the future
  (defun my/eglot-skip-huge-markup (orig markup)
    (let ((str (if (stringp markup) markup (plist-get markup :value))))
      (if (and str (> (length str) 20000))
          str
        (funcall orig markup))))
  (advice-add 'eglot--format-markup :around #'my/eglot-skip-huge-markup)
  )

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("C-c ! p" . my/flymake-goto-prev-error)
        ("C-c ! n" . my/flymake-goto-next-error)
        ("C-c ! e" . flymake-show-project-diagnostics)
        )
  :config

  (defun my/flymake-goto-next-error (&optional n)
    "Go to next flymake diagnostic, preferring errors.
Only fall back to warnings/notes when no errors exist."
    (interactive "p")
    (condition-case nil
        (flymake-goto-next-error n '(:error) t)
      (user-error
       (flymake-goto-next-error n nil t))))
  ;;
  (defun my/flymake-goto-prev-error (&optional n)
    "Go to next flymake diagnostic, preferring errors.
Only fall back to warnings/notes when no errors exist."
    (interactive "p")
    (condition-case nil
        (flymake-goto-prev-error n '(:error) t)
      (user-error
       (flymake-goto-prev-error n nil t))))
  ;;

  (defun my/flymake-set-faces (&rest _)
    "Set flymake faces blended against the current theme background."
    (my/set-error-face 'flymake-error   "red"    0.02)
    (my/set-error-face 'flymake-warning "orange" 0.02)
    (my/set-error-face 'flymake-note    "blue"   0.02))

  (my/flymake-set-faces)
  (advice-add 'load-theme :after #'my/flymake-set-faces))

(defun occur-dwim ()
  "Call occur on word at point (if exists)"
  (interactive)
  (let* ((here (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning)
                    (region-end))
                 (thing-at-point 'word))))
    (if here
        (funcall-interactively #'occur here)
      (call-interactively #'occur))))

(define-key occur-mode-map (kbd "n") #'occur-next)
(define-key occur-mode-map (kbd "p") #'occur-prev)

(define-key prog-mode-map (kbd "C-c o") #'occur-dwim)


(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(setq executable-prefix-env t)

(use-package compile
  :custom
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t)
  ;; (compilation-auto-jump-to-first-error 'first-error)
  :config
  ;; 2026-05-25: compilation-auto-jump-to-first-error seems to have
  ;; trouble with rust mode, adding our own hook fixes the problem
  (defun my/compile-jump-to-first-error (buffer status)
    (with-current-buffer buffer
      (when (and (derived-mode-p 'compilation-mode)
                 (not (string-prefix-p "finished" status)))
        (ignore-errors (first-error)))))
  (add-hook 'compilation-finish-functions #'my/compile-jump-to-first-error)
  (add-to-list 'display-buffer-alist
               '("\\*.*compilation\\*"
                 (my/display-in-compile-target-window)))
  )


(use-package project
  :config
  (defun my/project-compile ()
    (interactive)
    (let ((buf (call-interactively #'project-compile)))
      (when-let* ((win (get-buffer-window buf)))
        (select-window win))))
  :bind
  (:map project-prefix-map
        ("c" . my/project-compile)))

(use-package eldoc
  :custom
  (eldoc-echo-area-use-multiline-p 10)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*eldoc\\*"
                 (my/display-in-compile-target-window)
                 (inhibit-same-window . t))))
