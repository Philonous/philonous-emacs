(require 'tramp)
(require 'mon-rectangle-utils)

;; Get rid of stupid key translations

;; Now that <tab> isn't C-i any more, we have to adjust some key maps
;; (define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)
;; (global-set-key (kbd "<tab>") 'indent-for-tab-command)

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
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-m") 'make-frame)
(global-set-key (kbd "C-c C-p") 'close-open-paren)
(global-set-key (kbd "C-x g") 'magit-status)

(setq compilation-scroll-output t)
(setq inhibit-splash-screen t)
(setq-default indent-tabs-mode nil)
(transient-mark-mode -1)
(global-linum-mode t)
(show-paren-mode t)
(setq show-paren-style 'mixed)
(setq x-select-enable-clipboard t)
(put 'upcase-region 'disabled nil)
(mouse-avoidance-mode 'banish)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq backup-directory-alist
      (list (cons "." (expand-file-name "backup" user-emacs-directory))))
(tool-bar-mode -1)
(desktop-save-mode 0)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
