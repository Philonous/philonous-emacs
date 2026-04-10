;;; -*- lexical-binding: t; -*-

(defcustom rust-indent-offset 4
  "Indent Rust code by this number of spaces."
  :type 'integer
  :group 'rust-mode
  :safe #'integerp)

(use-package s :defer t)
(use-package dash :defer t)

(use-package rust-mode
  :defer t
  :custom
  (rust-mode-treesitter-derive t)
  :config
  (defun rust-add-sysroot ()
    "Add the nightly Rust sysroot lib to LD_LIBRARY_PATH."
    (require 's)
    (require 'dash)
    (-if-let (sysroot (shell-command-to-string "rustc +nightly --print sysroot"))
        (let* ((path (concat (s-trim sysroot) "/lib"))
               (old-ld-library-path (or (getenv "LD_LIBRARY_PATH") ""))
               (old-paths (s-split ":" old-ld-library-path t))
               (paths (s-join ":" (if (-contains? old-paths path)
                                      old-paths
                                    (cons path old-paths)))))
          (setenv "LD_LIBRARY_PATH" paths))
      (error "Could not determine rust nightly sysroot")))

  (defun rust--find-doc-dir (doc-dir name)
    "Return full path if documentation directory exists, nil otherwise."
    (let ((path (concat doc-dir "/" name)))
      (when (file-directory-p path) path)))

  (defun rust-open-dependency-docs ()
    "Query for a direct dependency and open its documentation."
    (interactive)
    (let* ((json-object-type 'alist)
           (json-array-type 'list)
           (metadata (json-read-from-string
                      (shell-command-to-string "cargo metadata --format-version 1 --no-deps")))
           (packages (alist-get 'packages metadata))
           (current-pkg (or (cl-find-if
                             (lambda (pkg)
                               (string-prefix-p (file-name-directory (alist-get 'manifest_path pkg))
                                                (file-name-directory (or buffer-file-name default-directory))))
                             packages)
                            (car packages)))
           (deps (mapcar (lambda (dep) (alist-get 'name dep))
                         (alist-get 'dependencies current-pkg)))
           (dep-name (completing-read "Dependency: " deps nil t))
           (target-dir (alist-get 'target_directory metadata))
           (doc-dir (concat target-dir "/doc")))
      (message "Building docs for %s..." dep-name)
      (shell-command (format "cargo doc --no-deps --package %s" dep-name))
      (let ((doc-path (or (rust--find-doc-dir doc-dir dep-name)
                          (rust--find-doc-dir doc-dir (replace-regexp-in-string "-" "_" dep-name))
                          (rust--find-doc-dir doc-dir (replace-regexp-in-string "_" "-" dep-name)))))
        (if doc-path
            (browse-url (concat "file://" doc-path "/index.html"))
          (error "Documentation directory not found for %s" dep-name))))))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode)
  :diminish cargo-minor-mode)

(use-package dap-mode
  :defer t
  :config
  (dap-register-debug-template "Rust::GDB"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "Rust::GDB"
                                     :gdbpath "rust-gdb"
                                     :MIMode "gdb"
                                     :miDebuggerPath "rust-gdb"
                                     :stopAtConnect t
                                     :console "external"
                                     :dap-compilation "cargo build"
                                     :target nil
                                     :cwd "${workspaceFolder}")))

(use-package realgud :defer t)
(use-package realgud-lldb :defer t)
