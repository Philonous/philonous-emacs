;;; -*- lexical-binding: t; -*-
;; (use-package lsp-mode
;;   :commands lsp
;;   :hook ((rust-mode . lsp)
;;          ;; haskell-mode handles its own hook
;;          )
;;   :bind
;;   (:map lsp-mode-map
;;         ("M-." . xref-find-definitions)
;;         ("C-c C-a C-a" . lsp-execute-code-action)
;;         ("C-c C-a C-l" . lsp-avy-lens)
;;         ("C-c C-q" . lsp-format-buffer)
;;         )
;;   :config
;;   (setq gc-cons-threshold 10000000)
;;   (setq read-process-output-max (* 1024 1024))
;;   (setq lsp-log-io nil)
;;   (setq lsp-auto-execute-action nil)    ; Always select actions before executing
;;   :custom
;;   (lsp-ui-sideline-enable nil)
;;   :diminish lsp-lens-mode
;;   )

;; (use-package lsp-ui :commands lsp-ui-mode)

(use-package apheleia)

(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c C-a C-a" . eglot-code-actions)
        ("C-c C-q" . eglot-format-buffer)
        )
  )

(use-package flymake
  :bind
  (:map flymake-mode-map
        ("C-c ! p" . flymake-goto-prev-error)
        ("C-c ! n" . flymake-goto-next-error)
        ("C-c ! e" . flymake-show-project-diagnostics)
        )
  :config

  ;;

  (defun my/flymake-set-faces ()
    "Set flymake faces blended against the current theme background."
    (my/set-error-face 'flymake-error   "red"    0.02)
    (my/set-error-face 'flymake-warning "orange" 0.02)
    (my/set-error-face 'flymake-note    "blue"   0.02))

  (my/flymake-set-faces)
  (advice-add 'load-theme :after (lambda (&rest _) (my/flymake-set-faces))))


(add-hook 'prog-mode-hook #'column-number-mode)

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

;; (defun occur-dwim ()
;;   "Call `occur' with a sane default."
;;   (interactive)
;;   (push (if (region-active-p)
;;             (buffer-substring-no-properties
;;              (region-beginning)
;;              (region-end))
;;           (let ((sym (thing-at-point 'symbol)))
;;             (when (stringp sym)
;;               (regexp-quote sym))))
;;         regexp-history)
;;   (call-interactively 'occur))

(define-key occur-mode-map (kbd "n") #'occur-next)
(define-key occur-mode-map (kbd "p") #'occur-prev)

(define-key prog-mode-map (kbd "C-c o") #'occur-dwim)

(define-key prog-mode-map (kbd "M-.") #'xref-find-definitions)


(use-package ansi-color
  :hook (compilation-filter . ansi-color-compilation-filter))

(setq executable-prefix-env t)

(use-package compile
  :custom
  (compilation-auto-jump-to-first-error 'first-error)
  (compilation-scroll-output 'first-error)
  (compilation-always-kill t))

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
