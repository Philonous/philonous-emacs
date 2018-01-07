;; packages

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

(setq package-archive-priorities
      '(("melpa-stable" . 20)
        ("gnu" . 10)
        ("melpa" . 0)))

(defun load-inits ()
  (let ((init-files (directory-files (expand-file-name "init.d" user-emacs-directory) t "\.el$") ))
    (mapc 'load init-files)))

(add-hook
 'after-init-hook
 #'(lambda ()
     (load-theme 'zenburn)

     ;; load path
     (add-to-list 'load-path "~/.emacs.d/lisp")
     (add-to-list 'load-path "~/.emacs.d/init.d")
     (add-to-list 'load-path "~/.emacs.d/custom-packages")
     (add-to-list 'load-path "~/.emacs.d/other-packages")
     (require 'custom-functions)
     (add-subdirs-to-load-path "~/.emacs.d/custom-packages")
     (add-subdirs-to-load-path "~/.emacs.d/other-packages")

     ;; Load every *.el file in init.d in lexicographical order
     (load-inits)

     )) ;; after-init-hook


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
