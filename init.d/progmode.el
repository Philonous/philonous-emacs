(use-package lsp-mode
  :commands lsp
  :hook ((rust-mode . lsp)
         ;; (haskell-mode . lsp)
         ))

(use-package lsp-ui)
(use-package company-lsp)

(use-package dap-mode)

(use-package smartparens
  :hook ((haskell-mode rust-mode nix-mode)
         . smartparens-mode))

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
