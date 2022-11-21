(use-package lsp-mode
  :pin melpa
  :commands lsp
  :hook ((rust-mode . lsp)
         ;; (haskell-mode . lsp)
         )
  :bind
    (("M-." . xref-find-definitions)
     ("C-c C-a C-a" . lsp-execute-code-action)
     ("C-c C-a C-l" . lsp-avy-lens)
     )
  :config
  (setq gc-cons-threshold 10000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-log-io nil)
  )

(use-package lsp-ui :pin melpa :commands lsp-ui-mode)
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)

;;; Unavailable
;; (use-package company-lsp)

(use-package dap-mode)

(use-package smartparens
  :hook ((haskell-mode rust-mode nix-mode)
         . smartparens-mode)
  :diminish
  )

(use-package fill-column-indicator
  :hook
  prog-mode-hook)

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
