
;; (defun rust-gdb ()
;;   (require 'cargo-process)
;;   (cargo-process--start "Build" "cargo build")
;;   (let* ((directory (or project-root default-directory) ))

;;   )

(require 's)
(require 'dash)

(use-package rust-mode)
(use-package flycheck-rust)
(use-package company-racer)
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  )
(use-package lsp-mode
  :hook rust-mode
  )
(use-package lsp-ui)


(defun rust-add-sysroot ()
  (-if-let (sysroot (shell-command-to-string "rustc +nightly --print sysroot"))
      (let* ((path (concat (s-trim sysroot) "/lib"))
             (old-ld-library-path (or (getenv "LD_LIBRARY_PATH") ""))
             (old-paths (s-split ":" old-ld-library-path t))
             (paths (s-join ":" (if (-contains? old-paths path)
                                    old-paths
                                  (cons path old-paths)))))
        (setenv "LD_LIBRARY_PATH" paths)
        )
    (error "Could not determine rust nightly sysroot")
    ))

(defun rust-mode-init ()
  (require 'rust-mode)
  (require 'flycheck-rust)
  (rust-add-sysroot)
  (sp-with-modes '(rust-mode)
    (sp-local-pair "'" nil :actions nil))
  (column-number-mode t)
  (flycheck-rust-setup)
  (flycheck-mode t)
  (cargo-minor-mode t)
  (rust-enable-format-on-save))

(add-hook 'rust-mode-hook #'rust-mode-init)
