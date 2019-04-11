;; packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.
(require 'package)


(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ))

(package-initialize)
(setq package-enable-at-startup nil)

(unless package-archive-contents
  (message "refreshing package contents")
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (message "installing use-package")
  (package-install 'use-package))

(setq use-package-always-ping t)
(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))

(use-package auto-package-update)

(setq auto-package-update-delete-old-versions t)

(defun load-inits ()
  (let ((init-files (directory-files (expand-file-name "init.d" user-emacs-directory) t "\.el$") ))
    (mapc 'load init-files)))

(add-hook
 'after-init-hook
 (lambda ()
   ;; load path
   (add-to-list 'load-path "~/.emacs.d/lisp")
   (add-to-list 'load-path "~/.emacs.d/init.d")
   (add-to-list 'load-path "~/.emacs.d/custom-packages")
   ;; (add-to-list 'load-path "~/.emacs.d/other-packages")
   (require 'custom-functions)
   (add-subdirs-to-load-path "~/.emacs.d/custom-packages")
   ;; (add-subdirs-to-load-path "~/.emacs.d/other-packages")

   ;; initialize use-package
   (eval-when-compile (require 'use-package))
   (setq use-package-always-ensure t)

   ;; Load every *.el file in init.d in lexicographical order
   (load-inits)

   )) ;; after-init-hook


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(put 'dired-find-alternate-file 'disabled nil)
