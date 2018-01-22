(require 'tramp)
(require 'mon-rectangle-utils)

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
(global-set-key (kbd "M-e") 'er/expand-region)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-c s") 'flysspell-region-or-buffer)
(global-set-key (kbd "C-x C-b") 'switch-to-previous-buffer)
(global-set-key (kbd "C-x C-m") 'make-frame)
(global-set-key (kbd "C-c C-p") 'close-open-paren)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c b") 'magit-blame-mode)
(global-set-key (kbd "<f11>") 'split-windows-threeway)
(global-set-key (kbd "C-c p") 'check-parens)
(global-set-key (kbd "C-c d") 'duplicate-line-or-region)

(add-to-list 'default-frame-alist '(font . "Inconsolata-11"))

;; IDO mode
(ido-mode t)
(ido-vertical-mode t)
(flx-ido-mode t)
(ido-ubiquitous-mode t)
(setq ido-use-faces nil)
(setq ido-show-dot-for-dired t)
(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)
(setq ido-default-buffer-method 'samewindow)
(setq ido-default-file-method 'selected-window)
(setq ido-auto-merge-work-directories-length -1)


;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Yasnippets

(setq yas-snippet-dirs '("~/.emacs.d/yas/custom-snippets/"
                         "~/.emacs.d/yas/yasnippet-snippets/"))
(yas-global-mode)

;; company
(global-company-mode)
(define-key company-mode-map (kbd "∈") 'company-complete)

; various
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
(mouse-avoidance-mode 'banish)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))
(tool-bar-mode -1)
(desktop-save-mode 0)
