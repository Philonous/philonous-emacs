(require 'custom-functions)
(require 'haskell-import)
;; (require 'haskell-cabal)

;; (require 'intero-debug)

;;; intero dante etc.

;; (use-package intero
;;   :pin melpa)

;; (use-package dante
;;   :config
;;   (add-to-list 'flycheck-checkers 'haskell-dante)
;;   (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))
;;   :hook ((haskell-mode . flycheck-mode)
;;          (haskell-mode . dante-mode)))

(use-package intero)

(use-package flycheck)
(setq flycheck-check-syntax-automatically '(save mode-enabled))

(use-package haskell-mode
  :pin melpa
  )

(use-package lsp-haskell
 :config
 (setq lsp-haskell-process-args-hie '())
 ;; Comment/uncomment this line to see interactions between lsp client/server.
 ;;(setq lsp-log-io t)
)

(use-package hindent
  :hook (haskell-mode . hindent-mode))

(add-to-list 'auto-mode-alist
             '("\\.hs\\'" . haskell-mode))
(add-to-list 'interpreter-mode-alist
             '("stack" . haskell-mode))

(defun haskell-process-restart-and-load-file ()
  "Restart the haskell process and load the file visited by buffer."
  (interactive)
  (progn (haskell-process-restart)
         (haskell-process-load-file)))

;; Load mode. This

(defun intero-run-command (command)
  (let* ((repl-buffer (intero-repl-buffer nil)))
    (with-current-buffer repl-buffer
      (comint-simple-send
       (get-buffer-process (current-buffer))
       command
       ))))

(defun haskell-load-file ()
  (cond
   ((boundp 'intero-mode) (save-window-excursion (intero-repl-load)))
   ((boundp 'haskell-interactive-mode) (haskell-process-load-file))))

(defun haskell-run-command (command)
  (cond
   ((boundp 'intero-mode)  (intero-run-command command))
   ((boundp  'haskell-interactive-mode) (haskell-process-show-repl-response command))
   ))


(defun haskell-process-set-load-mode (mode)
  "Set the ghci load mode to MODE (bytecode, object code, no code)."
  (haskell-run-command (concat ":set -f" mode "-code")))

(defun haskell-process-set-load-byte-code ()
  "Set the haskell process to compile to byte code."
  (interactive)
  (haskell-process-set-load-mode "byte"))

(defun haskell-process-set-load-no-code ()
  "Set the haskell process to compile to byte code."
  (interactive)
  (haskell-process-set-load-mode "no"))

(defun haskell-process-set-load-object-code ()
  "Set the haskell process to compile to byte code."
  (interactive)
  (haskell-process-set-load-mode "object"))

(defun haskell-process-reload-with-object-code ()
  "Reload current file with -f-object-code and reset to byte code."
  (interactive)
  (haskell-process-set-load-object-code)
  (haskell-load-file)
  (haskell-process-set-load-byte-code)
  ;; (haskell-clear-interactive-window)
  (haskell-load-file))

(defun haskell-process-for-tests ()
  "Load packages and set source path for tests."
  (interactive)
  (haskell-process-load-buffer-packages)
  (haskell-process-show-repl-response ":set -isrc:tests")
  (haskell-process-load-file)
)

(defun haskell-run-make-test ()
  "Run make test for the current project"
  (interactive)
  (if-let ((makefile (locate-dominating-file default-directory "Makefile"))
           (makedir (file-name-directory makefile))
           (default-directory makedir))
      (compilation-start "make test" 'haskell-compilation-mode)
    (error "No Makefile found")))

(defun haskell-run-make ()
  "Run make for the current project"
  (interactive)
  (if-let ((makefile (locate-dominating-file default-directory "Makefile"))
           (makedir (file-name-directory makefile))
           (default-directory makedir))
      (compilation-start "make" 'haskell-compilation-mode)
    (error "No Makefile found")))

(defun haskell-stack-test ()
  "Run stack test for the current project"
  (interactive)
  (if-let ((stack-file (locate-dominating-file default-directory "stack.yaml"))
           (stack-dir (file-name-directory stack-file))
           (default-directory stack-dir)
           )
      (compilation-start "stack test" 'haskell-compilation-mode)
    (error "No stack.yaml found")))



;; (setf (flycheck-checker-get 'intero 'next-checkers) nil)

;;; Run hlint after intero, but only if intero didn't report anything more
;;; severe than warnings
;; (flycheck-add-next-checker 'intero '(warning . haskell-hlint))

(defvar-local haskell-init--ide-mode nil
  "Which haskell IDE engine to use (e.g. intero, haskell-ide-engine)")

(add-to-list 'safe-local-variable-values '(haskell-init--ide-mode . intero))
(add-to-list 'safe-local-variable-values '(haskell-init--ide-mode . ghcide))
(add-to-list 'safe-local-variable-values '(haskell-init--ide-mode . lsp))
(add-to-list 'safe-local-variable-values '(haskell-init--ide-mode . hie))
(add-to-list 'safe-local-variable-values '(haskell-init--ide-mode . none))

(defun haskell-init--choose-ide-mode ()
  "Choose an IDE mode (i.e. intero, lsp) for this file"
  (interactive)
  (if (string= major-mode "haskell-mode")
      (progn
        (message "Got ide mode %s" haskell-init--ide-mode)
        (cond ((equal haskell-init--ide-mode 'intero) (intero-mode t))
              ((equal haskell-init--ide-mode 'ghcide)
               (setq lsp-haskell-process-path-hie "ghcide")
               (lsp))
              ((equal haskell-init--ide-mode 'lsp) (lsp))
              ((equal haskell-init--ide-mode 'hie)
               (lambda ()
                 (make-local-variable 'lsp-haskell-process-path-hie)
                 (setq lsp-haskell-process-path-hie "hie")
                 (make-local-variable lsp-haskell-process-args-hie)
                 (setq lsp-haskell-process-args-hie '())
                 (lsp)
                 ))
              ((eq haskell-init--ide-mode 'none) t)
              ;; If no mode is explicitly set, we try to guess one
              ((locate-dominating-file default-directory "stack.yaml")
               (intero-mode t))
              ))))


(add-hook 'hack-local-variables-hook #'haskell-init--choose-ide-mode t)

(defun intero-mode-init ()
  "Initialization for Intero mode."
  (require 'intero)
  ;; (set (make-local-variable 'intero-mode) t) ;; Hack to make flycheck
  ;; happy
  (intero-mode t)
  (require 'company)
  (set (make-local-variable 'company-backends)
       '(( company-intero
           company-capf
           company-dabbrev-code
           )
         company-files
         ))
  (company-mode nil)
  (setq flycheck-checker nil)
  (define-key intero-mode-map (kbd "C-c C-t") 'intero-type-at)
  (define-key intero-mode-map (kbd "C-c t") 'intero-type-at)
  (define-key intero-mode-map (kbd "C-c C-i") 'intero-info)
  (define-key intero-mode-map (kbd "M-.") 'intero-goto-definition)
  (define-key intero-mode-map (kbd "C-c C-l") (lambda () (interactive) (save-window-excursion
                                                                         (haskell-process-save-current-window)
                                                                         (haskell-clear-interactive-window)
                                                                         (haskell-load-file))))
  (define-key intero-mode-map (kbd "C-c C-c") nil)
  (define-key intero-mode-map (kbd "C-c C-c C-f") '(lambda () (interactive (flycheck-buffer)
                                                                           (flycheck-list-errors)
                                                                           )))
  (define-key intero-mode-map (kbd "C-c C-r") 'intero-apply-suggestions)
  (define-key intero-mode-map (kbd "C-c C-e") 'intero-expand-splice-at-point)
  (define-key intero-mode-map (kbd "C-c M-e") 'mc/mark-next-like-this)
  (define-key intero-mode-map (kbd "C-c r") 'intero-restart)
  )

(defun haskell-mode-init ()
  "Custom initialization function for Haskell mode."
  ;; (require 'ac-haskell-etags)
  ;; (require 'haskell-debug)
  ;; (require 'paredit)
  ;; (require 'intero)
  ;; (turn-on-font-lock)
  (column-number-mode t)

  (setq mode-line-format (list "%e"
                          mode-line-front-space
                          "%*"
                          mode-line-remote
                          " "
                          mode-line-buffer-identification
                          mode-line-position
                          mode-line-modes
                          mode-line-misc-info
                          mode-line-end-spaces)
        )

  (diminish 'auto-revert-mode)
  (diminish 'hindent-mode)
  (diminish 'yas-minor-mode)
  (diminish 'subword-mode)

  (subword-mode t)
  ;; (setq flymake-mode nil)
  ;; (flyspell-prog-mode)
  ;; (intero-mode-init)
  ;; (add-to-list 'flycheck-disabled-checkers 'haskell-ghc)
  ;; (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc)
  (sp-with-modes '(haskell-mode haskell-interactive-mode)
    (sp-local-pair "{-#" "#-}")
    (sp-local-pair "'" nil :actions nil)
    (sp-local-pair "\\(" nil :actions nil))

  ;; (yas-minor-mode t)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "M-<tab>") 'yas-expand)

  ;; (interactive-haskell-mode)

  (hindent-mode t)

  (set (make-local-variable 'align-rules-list) nil)
  (add-to-list 'align-rules-list
               '(haskell-types
                 (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-assignment
                 (regexp . "\\(\\s-+\\)=\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-arrows
                 (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-left-arrows
                 (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-constituent-group
                 (regexp . "\\(\\s-+\\)\\(\\[\\|{\\|(\\|,\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (add-to-list 'align-rules-list
               '(haskell-applicative-group
                 (regexp . "\\(\\s-+\\)\\(<\\$>\\|<\\*>\\)\\s-+")
                 (modes quote (haskell-mode literate-haskell-mode))))
  (haskell-indentation-mode t)

  (define-key haskell-mode-map (kbd "C-c C-f") 'inferior-haskell-find-definition)
  (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c t") 'haskell-process-do-type )
  (define-key haskell-mode-map (kbd "C-c C-l") (lambda () (interactive)
                                                 (haskell-clear-interactive-window)
                                                 (haskell-process-load-file)))

  (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-mode-save-buffer-and-tags)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-go-to-haskell-window)
  (define-key haskell-mode-map (kbd "M-`") 'haskell-go-to-haskell-window)
  (define-key haskell-mode-map (kbd "<f11>") 'layout-for-haskell-choose-by-hostname)
  (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "<backspace>") 'backward-delete-char)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-clear-interactive-window)
  (define-key haskell-mode-map (kbd "C-c r") 'haskell-process-restart-and-load-file)
  (define-key haskell-mode-map (kbd "C-c C-r") 'haskell-process-for-tests)
  (define-key haskell-mode-map (kbd "C-c C-c") nil)
  ;; (define-key haskell-mode-map (kbd "C-c C-c C-d") 'intero-debug-mode)
  (define-key haskell-mode-map (kbd "C-c C-c C-c") 'haskell-compile)
  (define-key haskell-mode-map (kbd "C-c C-c C-t") 'haskell-stack-test)
  (define-key haskell-mode-map (kbd "C-c C-c C-m C-m") 'haskell-run-make)
  (define-key haskell-mode-map (kbd "C-c C-c C-m C-t") 'haskell-run-make-test)
  (define-key haskell-mode-map (kbd "C-c l") 'send-text-to-haskell-process)
  (define-key haskell-mode-map (kbd "C-c C-x C-t") 'haskell-session-change-target)
  (define-key haskell-mode-map (kbd "C-c i") 'haskell-import-lines)
  (define-key haskell-mode-map (kbd "C-c C-.") (lambda () (interactive)
                                                 (haskell-align-imports)
                                                 (haskell-sort-imports)
                                                 ))
  (define-key haskell-mode-map (kbd "M-.")  'haskell-mode-jump-to-def-or-tag)
  (define-key haskell-mode-map (kbd "C-M-.")   'haskell-tag-find)
  ;; (define-key haskell-mode-map (kbd "C-M-.") 'haskell-mode-jump-to-def-or-tag)
  (define-key haskell-mode-map (kbd "M-]")   'align)
  (define-key haskell-mode-map (kbd "C-c g") 'haskell-rgrep)
  (define-key haskell-mode-map (kbd "C-c C-q") 'haskell-qualify-import-line)
  (define-key haskell-mode-map (kbd "C-c k") 'swap-underscore-camelcase)
  (define-key haskell-mode-map (kbd "C-c d") 'haskell-cabal-add-dependency)
  (define-key haskell-mode-map (kbd "M-n") 'forward-paragraph)
  (define-key haskell-mode-map (kbd "M-p") 'backward-paragraph)
  ;; (define-key haskell-mode-map (kbd "M-`") 'haskell-go-to-haskell-window)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c h") 'section-heading)
  (define-key haskell-mode-map (kbd "C-c C-x o") 'haskell-process-set-load-object-code)
  (define-key haskell-mode-map (kbd "C-c C-x b") 'haskell-process-set-load-byte-code)
  (define-key haskell-mode-map (kbd "C-c C-x n") 'haskell-process-set-load-no-code)
  (define-key haskell-mode-map (kbd "C-c C-s") 'haskell-process-reload-with-object-code)
  (define-key haskell-mode-map (kbd "C-c n") 'flycheck-next-error)
  (define-key haskell-mode-map (kbd "C-c C-d") 'duplicate-line-or-region)
  (define-key haskell-mode-map (kbd "<delete>") 'delete-char)


  (define-key haskell-mode-map (kbd "C-c e") (lambda () (interactive)
                                               (if flycheck-current-errors
                                                   (flycheck-list-errors))
                                               (message "No errors.")
                                               ))
  (define-key intero-mode-map (kbd "C-c C-e") 'mc/mark-next-like-this)
  )

(add-hook 'haskell-mode-hook #'haskell-mode-init)

(setq haskell-interactive-mode-hook
           (lambda ()
             (define-key haskell-interactive-mode-map (kbd "M-`") 'haskell-go-to-old-window)
             (define-key haskell-interactive-mode-map (kbd "C-`") 'haskell-go-to-old-window)
             ))

(defun intero-repl-mode-init ()
  (interactive)
  (define-key intero-repl-mode-map (kbd "M-`") 'haskell-go-to-old-window))

(add-hook 'intero-repl-mode-hook #'intero-repl-mode-init)

(use-package origami
  :hook (haskell-mode . origami-mode)
  :config
  (define-key origami-mode-map (kbd "C-M-i") 'origami-recursively-toggle-node)
  (define-key origami-mode-map (kbd "C-c @ a") 'origami-open-all-nodes)
  (define-key origami-mode-map (kbd "C-c @ d") 'origami-close-all-nodes)
  )
