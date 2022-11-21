(require 'tramp)

(use-package vagrant-tramp)

(require 'mon-rectangle-utils)

;;; Ripgrep
(use-package deadgrep
  :config
  (defalias 'rg 'deadgrep))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(defun select-fontsize ()
  "Select default font size depending on monitor geomtry"
  (let* ((geom (frame-monitor-attribute 'geometry))
         (xpixels (- (caddr geom) (car geom))))
    (cond ((< xpixels 3000) 10)
          (t 12))))


(use-package diminish
  :config
  (diminish 'eldoc-mode)
  (diminish 'visual-line-mode))

(add-to-list 'default-frame-alist
             `(font . ,(format "Inconsolata-%s" (select-fontsize))))
;; (add-to-list 'default-frame-alist '(font . "Fira Code-12"))



;; key bindings
(global-set-key (kbd "C-c <f5>") 'buffer-menu)
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>") '(lambda () "" (interactive) (other-window (- 1))))
(global-set-key (kbd "<f12>") 'undo)
(global-set-key (kbd "C-M-e") 'recursive-edit)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key
 (kbd "C-x r e")
 '(lambda () "reload .emacs" (interactive) (load-file "~/.emacs.d/init.el")))
(global-set-key (kbd "<Scroll_Lock>") '(lambda () "" (interactive) (ibuffer)))
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "C-x C-p") 'gist-region-or-buffer)
(global-set-key (kbd "C-x r #") 'enumerate-rectangle)
(global-set-key (kbd "<C-kp-add>") 'increment-number-at-point)
(global-set-key (kbd "<C-kp-subtract>") 'decrement-number-at-point)
(global-set-key (kbd "<C-S-SPC>") 'set-rectangular-region-anchor)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-c s") 'flysspell-region-or-buffer)
(global-set-key (kbd "C-x C-b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-x C-m") 'make-frame)
(global-set-key (kbd "C-c C-p") 'close-open-paren)
(global-set-key (kbd "<f11>") 'split-windows-threeway)
(global-set-key (kbd "C-c p") 'check-parens)
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)
(global-set-key (kbd "C-c t") 'open-project-todo)
(global-set-key (kbd "C-c q") 'bury-buffer)

(use-package expand-region
  :bind ("M-e" . 'er/expand-region))
;; (global-set-key (kbd "M-e") 'er/expand-region)

;; Automatically sets up magit-file-mode which sets C-x g etc
(use-package magit
  :pin melpa-stable
  :bind ("C-x g" . magit-status)
  )

(use-package git-gutter-fringe
  :config
  (global-git-gutter-mode t)
  :diminish git-gutter-mode
  )

;;; Unavablable
;; (use-package gitignore-mode)


(use-package direnv
  :pin melpa
  :config
  (direnv-mode))

;;;;;;;;;;;;;;;;;;;;;

;; (use-package ivy
;;   :pin melpa-stable
;;   :config
;;   (ivy-mode t)
;;   :diminish
;;   :config
;;   (define-key ivy-mode-map (kbd "C-j") #'ivy-immediate-done)
;;   )

(use-package swiper
  :pin melpa-stable
  :config
  (global-set-key (kbd "C-s") #'swiper)
  (global-set-key (kbd "C-r") #'swiper-backward)
  :diminish
  )

;; Yasnippets

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/yas/custom-snippets/"
                           "~/.emacs.d/yas/yasnippet-snippets/")))

;; company

(use-package company
  :bind ("∈" . 'company-complete)
  :config
  (global-company-mode)
  (setq company-idle-delay nil)
  :diminish company-mode
  )

;; which-keys

(use-package which-key)

; various

(global-visual-line-mode t)
(global-hl-line-mode t)


(setq compilation-scroll-output t)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(transient-mark-mode -1)
(global-linum-mode t)
(show-paren-mode t)
(setq x-select-enable-clipboard t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(mouse-avoidance-mode 'none)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/emacs/" ))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))


(tool-bar-mode -1)
(desktop-save-mode 0)

(use-package multiple-cursors)

(use-package yaml-mode)

(use-package poe-lootfilter-mode)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :diminish
  :hook (haskell-mode . projectile-mode)
  )

(setq require-final-newline 'visit-save)

(use-package writegood-mode)

(use-package treemacs
  :config
  (progn
    (setq treemacs-position 'left)
    (setq treemacs-width 28)
    )
  (treemacs-resize-icons 28)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package treemacs-magit
  :hook treemacs
  )

;; (use-package undo-tree)


(use-package autoinsert
  :config
  (setq auto-insert-query nil)
  (auto-insert-mode 1)
  (add-hook 'find-file-hook 'auto-insert)
  (setq auto-insert-alist nil))

;;; https://github.com/bbatsov/prelude/issues/1225
;; (use-package undo-tree )
