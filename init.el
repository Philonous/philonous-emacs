;; init.el -*- lexical-binding: t -*-

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ))

(setq use-package-always-ensure t)

;; (unless package-archive-contents
;;   (message "refreshing package contents")
;;   (package-refresh-contents))

(setq use-package-always-pin "melpa")
(setq package-archive-priorities
      '(("melpa" . 20)
        ("melpa-stable" . 10)
        ("gnu" . 1)
        ))

(use-package auto-compile
  :config
  (auto-compile-on-load-mode +1)
  (auto-compile-on-save-mode +1)
  ;; Auto-compile will jump to the first error when we hit save and
  ;; compilation-auto-jump-to-first-error is true - annoying, so we
  ;; disable that behaviour when auto-compile is run
  (advice-add 'auto-compile-byte-compile :around
              (lambda (fn &rest args)
                (let ((compilation-auto-jump-to-first-error nil))
                  (apply fn args))))
  )

(let ((default-directory (expand-file-name "lisp" user-emacs-directory))) (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "init.d" user-emacs-directory))
(require 'custom-functions)

(eval-when-compile (require 'use-package))


(let ((init-files (directory-files (expand-file-name "init.d" user-emacs-directory) t "\\.el\\'")))
  (mapc (lambda (f)
          (let ((start (current-time)))
            (load (file-name-sans-extension f))
            (message "%s: %.2fs"
                     (file-name-nondirectory f)
                     (float-time (time-subtract (current-time) start)))))
        init-files))


(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)
(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
