;;; global-init.el --- Global configuration -*- lexical-binding: t; -*-

;;; ---- Theme & Visuals (necessarily eager) -----------------------------------


(setq custom-unlispify-tag-names nil)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

;; Personal face tweaks live in their own theme rather than in
;; custom.el, so Customize never saves them and they stack on top of the
;; main theme.
(deftheme my-overrides "Personal face overrides on top of the main theme.")
(custom-theme-set-faces
 'my-overrides
 '(hl-line ((t (:background "#424242"))))
 '(font-lock-function-name-face ((t (:underline t))))
 '(font-lock-type-face ((t (:weight bold)))))
(enable-theme 'my-overrides)

(defun select-fontsize ()
  "Select default font size depending on monitor geometry."
  (let* ((geom (frame-monitor-attribute 'geometry))
         (xpixels (- (caddr geom) (car geom))))
    (if (< xpixels 3000) 10 12)))

(add-to-list 'default-frame-alist
             `(font . ,(format "Inconsolata-%s" (select-fontsize))))

(fringe-mode '(12 . 8))
(tool-bar-mode -1)
(global-hl-line-mode t)
(global-display-line-numbers-mode t)
(show-paren-mode t)

;;; ---- Mode line -------------------------------------------------------------

(defvar-local my/narrow-mode-line-indicator
  '(:eval (when (buffer-narrowed-p)
            (propertize " [NARROWED] " 'face 'warning))))
(put 'my/narrow-mode-line-indicator 'risky-local-variable t)

(defvar-local my/line-ending-indicator
  '(:eval (pcase (and buffer-file-coding-system
                      (coding-system-eol-type buffer-file-coding-system))
            (1 (propertize " [WINDOWS] "
                           'face '(:background "red" :foreground "white" :weight bold)
                           'help-echo "CRLF line endings — convert with C-x RET f unix"))
            (2 (propertize " [MAC] "
                           'face '(:background "orange" :foreground "black" :weight bold)
                           'help-echo "CR line endings"))
            (_ "")
            )))

(put 'my/line-ending-indicator 'risky-local-variable t)

(setq-default mode-line-format
              '("%e" mode-line-front-space
                (:propertize
                 ("" mode-line-modified)
                 display
                 (min-width
                  (3.0)))
                my/line-ending-indicator
                my/narrow-mode-line-indicator
                mode-line-remote " " mode-line-buffer-identification
                " »" (:eval (symbol-name major-mode)) "« "
                "   "
                mode-line-misc-info mode-line-end-spaces))

;;; ---- General settings (cheap, no requires) ---------------------------------

(setq inhibit-splash-screen t
      ring-bell-function 'ignore
      require-final-newline 'visit-save
      completion-cycle-threshold 10
      backup-by-copying t
      kill-whole-line t
      vc-follow-symlinks nil)

(setq-default indent-tabs-mode nil
              fill-column 80)
(column-number-mode 1)
(setopt create-lockfiles nil)

(transient-mark-mode -1)
(desktop-save-mode 0)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(mouse-avoidance-mode 'none)

(add-hook 'before-save-hook 'whitespace-cleanup)


(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/emacs/"))
(make-directory user-temporary-file-directory t)

(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

(use-package tramp
  :ensure nil  ; built-in
  :defer t
  :config
  (require 'auth-source)
  (require 'secrets)
  (add-to-list 'backup-directory-alist
               `(,tramp-file-name-regexp . nil))

  :custom
  ;; Compare https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  (tramp-copy-size-limit (* 2 1024 1024))
  (tramp-use-scp-direct-remote-copying t)
  (remote-file-name-inhibit-locks t)
  )

(use-package ediff
  :ensure nil
  :defer t
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package flyspell
  :ensure nil
  :defer t
  :custom
  (flyspell-issue-message-flag nil))

(use-package auth-source
  :ensure nil
  :defer t
  :custom
  (auth-sources '("secrets:tramp"))
  (auth-source-do-cache nil)
  (auth-source-cache-expiry 60)
  (auth-source-save-behavior t)
  )


;;; ---- Key bindings (no packages needed) -------------------------------------

(global-set-key (kbd "C-c <f5>") #'buffer-menu)
(global-set-key (kbd "C-<tab>") #'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "<f12>") #'undo)
(global-set-key (kbd "C-M-e") #'recursive-edit)
(global-set-key (kbd "C-x a r") #'align-regexp)
(global-set-key (kbd "<Scroll_Lock>") #'ibuffer)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "C-x r #") #'enumerate-rectangle)
(global-set-key (kbd "<C-kp-add>") #'increment-number-at-point)
(global-set-key (kbd "<C-kp-subtract>") #'decrement-number-at-point)
(global-set-key (kbd "<C-S-SPC>") #'set-rectangular-region-anchor)
(global-set-key (kbd "M-z") #'zap-up-to-char)
(global-set-key (kbd "C-z") #'jump-to-char)
(global-set-key (kbd "C-c s") #'flyspell-region-or-buffer)
(global-set-key (kbd "C-x C-b") #'switch-to-previous-buffer)
(global-set-key (kbd "C-x C-m") #'make-frame)
(global-set-key (kbd "<f11>") #'split-windows-threeway)
(global-set-key (kbd "C-c p") #'check-parens)
(global-set-key (kbd "C-c d") #'duplicate-line-or-region)
(global-set-key (kbd "C-c t") #'open-project-todo)
(global-set-key (kbd "C-c q") #'bury-buffer)
(global-set-key (kbd "C-x 1")
                (lambda () (interactive)
                  (let ((ignore-window-parameters t))
                    (delete-other-windows))))

;;; ---- Packages: completion stack (eager — UX requires it) -------------------

(use-package vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode 1)
  (setq vertico-multiform-commands
        '((consult-line)
          (consult-ripgrep buffer)
          (consult-grep buffer)
          (t reverse))))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :init
  (marginalia-mode t))

(use-package consult
  :bind (("C-s"   . consult-line)
         ("C-x b" . consult-buffer)
         ("M-g g" . consult-goto-line)
         ("M-g i" . consult-imenu)
         ("M-s r" . consult-ripgrep)
         ("M-s f" . consult-find)
         ("M-y"   . consult-yank-pop))
  :config
  (consult-customize consult-buffer :preview-key "M-."))

(use-package embark
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :after (:all embark consult))  ; only load when both are present

(use-package corfu
  :bind ("∈" . completion-at-point)
  :config
  (global-corfu-mode))

;;; ---- Packages: deferred ---------------------------------------------------

(use-package diminish
  :demand t)  ; other packages need :diminish available immediately

(use-package deadgrep
  :commands (deadgrep rg)
  :init
  (defalias 'rg #'deadgrep))

(use-package expand-region
  :bind ("M-e" . er/expand-region))

(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-diff-refine-hunk 'all)
  (magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(use-package majutsu
  :vc (:url "https://github.com/0WD0/majutsu")
  :bind ("C-x j" . majutsu)
  )

(use-package diff-hl
  :hook (magit-post-refresh . diff-hl-magit-post-refresh) ; was -hook (double-hooked)
  :hook (after-init . global-diff-hl-mode))

(use-package envrc
  :hook (after-init . envrc-global-mode)
  :diminish)

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :custom
  (yas-snippet-dirs '("~/.emacs.d/yas/custom-snippets/"
                      "~/.emacs.d/yas/yasnippet-snippets/"))
  :diminish yas-minor-mode)

(use-package which-key
  :defer 2  ; load after 2s idle — you don't need it at frame 0
  :diminish
  :config
  (which-key-mode t))

(use-package multiple-cursors
  :bind (("C-c C-SPC" . mc/edit-lines)
         ("C->"       . mc/mark-next-like-this)
         ("C-<"       . mc/mark-previous-like-this)
         ("C-c C-<"   . mc/mark-all-like-this)))

(use-package markdown-mode
  :defer t
  :custom
  (markdown-command "pandoc"))

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package poe-lootfilter-mode
  :mode "\\.filter\\'")  ; adjust extension if different

(use-package writegood-mode
  :hook ((text-mode markdown-mode org-mode) . writegood-mode))

(use-package autoinsert
  :defer 3  ; built-in, cheap, but no rush
  :config
  (setq auto-insert-query nil
        auto-insert-alist nil)
  (auto-insert-mode 1))

(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :diminish)

;;; ---- Log watch mode (standalone, no package) -------------------------------

(require 'ansi-color)  ; cheap built-in, needed by log-watch

(defun log-watch-apply-ansi-colors (&rest _args)
  "Apply ANSI colors to the entire buffer without marking it as modified."
  (with-silent-modifications
    (ansi-color-apply-on-region (point-min) (point-max))))

(define-minor-mode log-watch-mode
  "Minor mode to apply ANSI colors in a log file and enable auto-revert."
  :lighter " LogWatch"
  :global nil
  (if log-watch-mode
      (progn
        (auto-revert-mode 1)
        (add-hook 'after-revert-hook #'log-watch-apply-ansi-colors nil t)
        (add-hook 'after-change-functions #'log-watch-apply-ansi-colors nil t)
        (log-watch-apply-ansi-colors))
    (remove-hook 'after-revert-hook #'log-watch-apply-ansi-colors t)
    (remove-hook 'after-change-functions #'log-watch-apply-ansi-colors t)
    (auto-revert-mode -1)))

(defun my/breadcrumb ()
  (let* ((f (or buffer-file-name default-directory))
         (host (file-remote-p f 'host))
         (path (or (file-remote-p f 'localname) f)))
    (concat (when host
              (propertize (concat " @ " host ":") 'face 'warning))
            (if host path (abbreviate-file-name path)))))

(setq-default header-line-format '(:eval (my/breadcrumb)))

(use-package eat
  :pin "nongnu"
  )

(use-package sops)

(use-package isend-mode)

;;; global-init.el ends here
