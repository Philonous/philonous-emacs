(require 'haskell-mode-autoloads)
(require 'hare)
(require 'custom-functions)
(require 'haskell-importer)

(setq auto-mode-alist (cons '("\\.hs$" . haskell-mode) auto-mode-alist))

(defun haskell-process-restart-and-load-file ()
  "Restart the haskell process and load the file visited by buffer"
  (interactive)
  (progn (haskell-process-restart)
         (haskell-process-load-file)))

(defun shm ()
  "start structures haskell mode"
  (interactive)
  (require 'shm)
  (structured-haskell-mode t)
  (require 'shm-case-split)
  (define-key haskell-mode-map (kbd "C-c C-s") 'shm-case-split))


(setq haskell-mode-hook
      (lambda ()
        (require 'ac-haskell-etags)
        (require 'haskell-debug)
        (require 'paredit)
        (haskell-indentation-mode t)
        (turn-on-font-lock)
        (column-number-mode t)
        (setq flymake-mode nil)
        (flyspell-prog-mode)
        (auto-complete-mode t)
        (setq ac-sources '(ac-source-haskell-etags
                           ac-source-words-in-same-mode-buffers))
        (ac-linum-workaround)
        (setq ac-auto-start nil)
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

        ;; (shm)
        (define-key ac-mode-map (kbd "<tab>") 'indent-according-to-mode)
        (define-key haskell-mode-map (kbd "M-<tab>") 'ac-trigger-key-command)
        (define-key haskell-mode-map (kbd "C-c C-f") 'inferior-haskell-find-definition)
        (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
        (define-key haskell-mode-map (kbd "C-c t") 'haskell-process-do-type )
        (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
        (define-key haskell-mode-map (kbd "C-c C-d") 'haskell-mode-save-buffer-and-tags)
        (define-key haskell-mode-map (kbd "C-`") 'haskell-go-to-haskell-window)
        (define-key haskell-mode-map (kbd "M-`") 'haskell-go-to-haskell-window)
        (define-key haskell-mode-map (kbd "<f11>") 'layout-for-haskell)
        (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
        (define-key haskell-mode-map (kbd "<backspace>") 'backward-delete-char)
        (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-clear-interactive-window)
        (define-key haskell-mode-map (kbd "C-c r") 'haskell-process-restart-and-load-file)
        (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)
        (define-key haskell-mode-map (kbd "C-c p") 'check-parens)
        (define-key haskell-mode-map (kbd "C-c l") 'send-text-to-haskell-process)
        (define-key haskell-mode-map (kbd "C-c C-x C-t") 'haskell-session-change-target)
        (define-key haskell-mode-map (kbd "C-c i") 'haskell-import-lines)
        (define-key haskell-mode-map (kbd "M-.")   'haskell-mode-tag-find)
        (define-key haskell-mode-map (kbd "C-M-.") 'haskell-mode-jump-to-def-or-tag)
        (define-key haskell-mode-map (kbd "M-]")   'align)
        (define-key haskell-mode-map (kbd "C-c g") 'haskell-rgrep)
        (define-key haskell-mode-map (kbd "C-c C-q") 'haskell-qualify-import-line)
        (define-key haskell-mode-map (kbd "C-c k") 'swap-underscore-camelcase)
        (define-key haskell-mode-map (kbd "C-<right>") 'paredit-forward-slurp-sexp)
        (define-key haskell-mode-map (kbd "C-c d") 'haskell-cabal-add-dependency)
        (define-key haskell-mode-map (kbd "M-n") 'forward-paragraph)
        (define-key haskell-mode-map (kbd "M-p") 'backward-paragraph)
        (define-key haskell-debug-mode-map (kbd "M-`") 'haskell-go-to-haskell-window)
        ))

(setq haskell-interactive-mode-hook
           (lambda ()
             (define-key haskell-interactive-mode-map (kbd "M-`") 'haskell-go-to-old-window)
             (define-key haskell-interactive-mode-map (kbd "C-`") 'haskell-go-to-old-window)
             ))

(custom-set-variables
 '(haskell-compile-cabal-build-command
    "cd %s; cabal install --ghc-option=-ferror-spans --enable-tests && cabal configure --enable-tests")
 '(haskell-indentation-layout-offset 4)
 '(haskell-indentation-left-offset 4)
 '(haskell-process-args-cabal-repl nil)
 '(haskell-process-log t)
 '(haskell-process-path-cabal "/home/uart14/.cabal/bin/cabal")
 '(haskell-process-path-cabal-ghci "/home/uart14/.cabal/bin/cabal")
 '(haskell-process-suggest-overloaded-strings nil)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-process-suggest-restart nil)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t))
