
;; (defun rust-gdb ()
;;   (require 'cargo-process)
;;   (cargo-process--start "Build" "cargo build")
;;   (let* ((directory (or project-root default-directory) ))

;;   )

(require 's)
(require 'dash)

(use-package rust-mode)
(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  )

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
  (require 'smartparens)
  (sp-with-modes '(rust-mode)
    (sp-local-pair "'" nil :actions nil))
  )

(add-hook 'rust-mode-hook #'rust-mode-init)
