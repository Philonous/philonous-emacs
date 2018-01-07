
;; (defun rust-gdb ()
;;   (require 'cargo-process)
;;   (cargo-process--start "Build" "cargo build")
;;   (let* ((directory (or project-root default-directory) ))

;;   )

(defun rust-mode-init ()
  (require 'rust-mode)
  (require 'flycheck-rust)
  (require 'smartparens)
  (smartparens-mode t)
  (sp-with-modes '(rust-mode)
    (sp-local-pair "'" nil :actions nil))
  (column-number-mode t)
  (flycheck-rust-setup)
  (flycheck-mode t)
  (cargo-minor-mode t))

(add-hook 'rust-mode-hook #'rust-mode-init)
