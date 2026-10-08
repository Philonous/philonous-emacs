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
  :config
  (add-to-list 'display-buffer-alist
               '("\\*eldoc\\*"
                 (my/display-in-compile-target-window)
                 (inhibit-same-window . t))))

;; (use-package minuet
;;   :bind
;;   (:map minuet-active-mode-map
;;         ("TAB" . #'minuet-accept-suggestion)
;;         ("M-n" . #'minuet-next-suggestion)
;;         ("M-p" . #'minuet-previous-suggestion)
;;         ("C-g" . #'minuet-dismiss-suggestion))
;;   :init
;;   (setq minuet-provider 'openai-fim-compatible)
;;   :config
;;   (setq minuet-n-completions 1
;;         minuet-context-window 512)
;;   (plist-put minuet-openai-fim-compatible-options
;;              :end-point "http://localhost:11434/v1/completions")
;;   (plist-put minuet-openai-fim-compatible-options :model "https://huggingface.co/unsloth/Qwen3-Coder-30B-A3B-Instruct-GGUF:UD-Q4_K_XL")
;;   (plist-put minuet-openai-fim-compatible-options :api-key "dummy")
;;   ;; as-you-type ghost text; drop this line for manual-trigger only
;;   (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode))


(use-package gptel
  :commands (gptel gptel-send gptel-rewrite gptel-menu)
  :bind (("C-c g" . gptel-menu)        ; transient: pick model, mode, etc.
         ("C-c RET" . gptel-send)       ; send region/buffer up to point
         ("C-c r" . gptel-rewrite))     ; in-place edit of the active region
  :init
  ;; Don't stream token-by-token for local edits — it fights with
  ;; in-buffer rewrite and the latency win is marginal on localhost.
  (setq gptel-default-mode 'org-mode
        gptel-stream t)
  :config
  (setq gptel-backend
        (gptel-make-ollama "ollama-local"
          :host "localhost:11434"
          :stream t
          :models '(huggingface.co/unsloth/Qwen3-Coder-30B-A3B-Instruct-GGUF:UD-Q3_K_XL)))
  ;; Make the 14B the default for prompt-driven work.
  (setq gptel-model 'huggingface.co/unsloth/Qwen3-Coder-30B-A3B-Instruct-GGUF:UD-Q3_K_XL)

  ;; Send the whole buffer as context for rewrites, not just the region.
  ;; This is the "rest of the file as context" behaviour you wanted.
  (setq gptel-use-context 'system))
