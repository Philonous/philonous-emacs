(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-solve-tactics-face ((((type x) (class color) (background dark)) (:foreground "#f0f"))) t)
 '(font-lock-comment-face ((t (:inherit font-lock-comment))))
 '(font-lock-function-name-face ((t (:inherit font-lock-string :underline t))))
 '(font-lock-keyword-face ((t (:inherit font-lock-keyword :underline t))))
 '(font-lock-string-face ((t (:inherit font-lock-doc))))
 '(font-lock-type-face ((t (:inherit font-lock-type :weight bold))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "green"))))
 '(magit-diff-file-header ((t (:inherit magit-header :underline nil))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :underline nil :slant italic))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "gray20"))))
 '(proof-locked-face ((((type x) (class color) (background dark)) (:background "#2b2b2b"))) t)
 '(proof-queue-face ((((type x) (class color) (background dark)) (:background "#4f4f4f"))) t)
 '(visible-mark-active ((t (:inverse-video t)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agda2-highlight-face-groups (quote conor))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(bbdb-check-postcode nil)
 '(bbdb-default-country "Germany")
 '(custom-safe-themes
   (quote
    ("40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "31d3463ee893541ad572c590eb46dcf87103117504099d362eeed1f3347ab18f" "bcb5c86c0e6576d1d6bba9bd2a55cd5c20a57d307ed13bf4ed0e86ed944e33df" "5ad69aca83efd8658fb11019c476d13b8b62422c6446220b1e90d6ef9dfd4498" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "81805c86e126018f339211bb3f03e1c9eae30adfbe72832bd02f89ca0cbe5885" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(ecb-options-version "2.40")
 '(ecb-source-path
   (quote
    ("/home/uart14/projects/pontarius" "/home/uart14/src"
     ("/home/uart14/projects" "filesender"))))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fci-rule-color "dark gray")
 '(fill-column 80)
 '(flyspell-issue-message-flag nil)
 '(global-visible-mark-mode t)
 '(ido-confirm-unique-completion t)
 '(safe-local-variable-values
   (quote
    ((haskell-compile-cabal-build-command . "cd %s; stack build --ghc-options=-ferror-spans"))))
 '(sh-indentation 2)
 '(show-paren-style (quote parenthesis))
 '(sp-base-key-bindings (quote paredit))
 '(sp-ignore-modes-list (quote (minibuffer-inactive-mode emacs-lisp-mode)))
 '(sql-connection-alist
   (quote
    (("ltest"
      (sql-product
       (quote postgres))
      (sql-user "uart14")
      (sql-database "lambdatradetest")
      (sql-server "localhost")))))
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-compile-cabal-build-alt-command "cd %s && cabal install --only-dep --enable-tests && cabal configure --enable-tests")
 '(haskell-compile-cabal-build-command "cd %s; cabal install --only-dep --enable-tests && cabal configure --enable-tests && cabal build --ghc-option=-ferror-spans")
 '(haskell-indentation-layout-offset 2)
 '(haskell-indentation-left-offset 2)
 '(haskell-notify-p t)
 '(haskell-process-args-cabal-repl nil ) ;; (quote ("--with-ghc=/home/uart14/projects/ghci-ng/.cabal-sandbox/bin/ghci-ng")))
 '(haskell-process-log t)
 '(haskell-process-path-cabal "cabal")
 '(haskell-process-path-cabal-ghci "cabal")
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-overloaded-strings t)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-process-suggest-restart nil)
 '(haskell-process-type 'auto)
 '(haskell-process-use-presentation-mode nil)
 '(haskell-tags-on-save t)
 )
