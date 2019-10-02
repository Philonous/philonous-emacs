(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coq-solve-tactics-face ((((type x) (class color) (background dark)) (:foreground "#f0f"))))
 '(flycheck-error ((t (:background "#502020" :underline nil))))
 '(flycheck-fringe-error ((t (:background "#502020" :weight bold))))
 '(flycheck-info ((t (:background "#202050" :underline nil))))
 '(flycheck-warning ((t (:background "#303010" :underline nil))))
 '(font-lock-comment-face ((t (:inherit font-lock-comment))))
 '(font-lock-function-name-face ((t (:inherit font-lock-string :underline t))))
 '(font-lock-keyword-face ((t (:inherit font-lock-keyword :underline t))))
 '(font-lock-string-face ((t (:inherit font-lock-doc))))
 '(font-lock-type-face ((t (:inherit font-lock-type :weight bold))))
 '(git-gutter-fr:added ((t (:background "#7FC07F" :foreground "black" :weight bold))))
 '(git-gutter-fr:deleted ((t (:background "#CC4343" :foreground "black" :weight bold))))
 '(git-gutter-fr:modified ((t (:background "#DC4CC3" :foreground "black" :weight bold))))
 '(hl-line ((t (:background "#424242"))))
 '(intero-debug-current-context ((t (:background "#301810"))))
 '(magit-diff-add ((((class color) (background dark)) (:foreground "green"))))
 '(magit-diff-file-header ((t (:inherit magit-header :underline nil))))
 '(magit-diff-hunk-header ((t (:inherit magit-header :underline nil :slant italic))))
 '(magit-item-highlight ((((class color) (background dark)) (:background "gray20"))))
 '(proof-locked-face ((((type x) (class color) (background dark)) (:background "#2b2b2b"))))
 '(proof-queue-face ((((type x) (class color) (background dark)) (:background "#4f4f4f"))))
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
   ["#3F3F3F" "#CC9393" "#7F9F7F" "#F0DFAF" "#8CD0D3" "#DC8CC3" "#93E0E3" "#DCDCCC"])
 '(bbdb-check-postcode nil)
 '(bbdb-default-country "Germany")
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("ec5f697561eaf87b1d3b087dd28e61a2fc9860e4c862ea8e6b0b77bd4967d0ba" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "2022c5a92bbc261e045ec053aa466705999863f14b84c012a43f55a95bf9feb8" "67e998c3c23fe24ed0fb92b9de75011b92f35d3e89344157ae0d544d50a63a72" "f5512c02e0a6887e987a816918b7a684d558716262ac7ee2dd0437ab913eaec6" "6de7c03d614033c0403657409313d5f01202361e35490a3404e33e46663c2596" "1011be33e9843afd22d8d26b031fbbb59036b1ce537d0b250347c19e1bd959d0" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "ed317c0a3387be628a48c4bbdb316b4fa645a414838149069210b66dd521733f" "938d8c186c4cb9ec4a8d8bc159285e0d0f07bad46edf20aa469a89d0d2a586ea" "69831e572dc46ced47c5309bff8fc2f4a9e237e2bad2c76f313da814a4628694" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "bb4733b81d2c2b5cdec9d89c111ef28a0a8462a167d411ced00a77cfd858def1" "b747fb36e99bc7f497248eafd6e32b45613ee086da74d1d92a8da59d37b9a829" "40f6a7af0dfad67c0d4df2a1dd86175436d79fc69ea61614d668a635c2cd94ab" "11636897679ca534f0dec6f5e3cb12f28bf217a527755f6b9e744bd240ed47e1" "19352d62ea0395879be564fc36bc0b4780d9768a964d26dfae8aad218062858d" "146d24de1bb61ddfa64062c29b5ff57065552a7c4019bee5d869e938782dfc2a" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "31d3463ee893541ad572c590eb46dcf87103117504099d362eeed1f3347ab18f" "bcb5c86c0e6576d1d6bba9bd2a55cd5c20a57d307ed13bf4ed0e86ed944e33df" "5ad69aca83efd8658fb11019c476d13b8b62422c6446220b1e90d6ef9dfd4498" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "0bac11bd6a3866c6dee5204f76908ec3bdef1e52f3c247d5ceca82860cccfa9d" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "81805c86e126018f339211bb3f03e1c9eae30adfbe72832bd02f89ca0cbe5885" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" default)))
 '(dante-methods
   (quote
    (stack styx new-impure-nix new-nix nix impure-nix new-build nix-ghci mafia bare-cabal bare-ghci)))
 '(dante-methods-alist
   (quote
    ((styx "styx.yaml"
           ("styx" "repl" dante-target))
     (new-impure-nix dante-cabal-new-nix
                     ("nix-shell" "--run"
                      (concat "cabal new-repl "
                              (or dante-target
                                  (dante-package-name)
                                  "")
                              " --builddir=dist/dante")))
     (new-nix dante-cabal-new-nix
              ("nix-shell" "--pure" "--run"
               (concat "cabal new-repl "
                       (or dante-target
                           (dante-package-name)
                           "")
                       " --builddir=dist/dante")))
     (nix dante-cabal-nix
          ("nix-shell" "--pure" "--run"
           (concat "cabal repl "
                   (or dante-target "")
                   " --builddir=dist/dante")))
     (impure-nix dante-cabal-nix
                 ("nix-shell" "--run"
                  (concat "cabal repl "
                          (or dante-target "")
                          " --builddir=dist/dante")))
     (new-build "cabal.project"
                ("cabal" "new-repl"
                 (or dante-target
                     (dante-package-name)
                     "")
                 "--builddir=dist/dante"))
     (nix-ghci
      #[257 "\300\301\302#\207"
            [directory-files t "shell.nix\\|default.nix"]
            5 "

(fn D)"]
      ("nix-shell" "--pure" "--run" "ghci"))
     (stack "stack.yaml"
            ("stack" "repl" "--ghci-options=-ferror-spans -fdiagnostics-color=never" dante-target))
     (mafia "mafia"
            ("mafia" "repl" dante-target))
     (bare-cabal
      #[257 "\300\301\302#\207"
            [directory-files t ".cabal$"]
            5 "

(fn D)"]
      ("cabal" "repl" dante-target "--builddir=dist/dante"))
     (bare-ghci
      #[257 "\300\207"
            [t]
            2 "

(fn _)"]
      ("ghci")))))
 '(ecb-options-version "2.40")
 '(ecb-source-path
   (quote
    ("/home/uart14/projects/pontarius" "/home/uart14/src"
     ("/home/uart14/projects" "filesender"))))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(fci-rule-color "#383838")
 '(fill-column 80)
 '(flycheck-hlintrc ".hlint.yaml")
 '(flyspell-issue-message-flag nil)
 '(global-visible-mark-mode t)
 '(haskell-align-imports-pad-after-name t)
 '(haskell-ask-also-kill-buffers nil)
 '(haskell-compile-cabal-build-alt-command
   "cd %s && cabal install --only-dep --enable-tests && cabal configure --enable-tests")
 '(haskell-compile-cabal-build-command
   "cd %s; stack build --test --no-run-tests --ghc-options -ferror-spans --color never")
 '(haskell-indentation-layout-offset 2)
 '(haskell-indentation-left-offset 2)
 '(haskell-notify-p t)
 '(haskell-process-args-cabal-repl nil)
 '(haskell-process-args-stack-ghci
   (quote
    ("--ghci-options=-ferror-spans -fdiagnostics-color=never" "--color" "never")))
 '(haskell-process-log t)
 '(haskell-process-path-cabal "cabal")
 '(haskell-process-path-cabal-ghci "cabal")
 '(haskell-process-reload-with-fbytecode nil)
 '(haskell-process-show-debug-tips nil)
 '(haskell-process-suggest-overloaded-strings t)
 '(haskell-process-suggest-remove-import-lines nil)
 '(haskell-process-suggest-restart nil)
 '(haskell-process-type (quote auto))
 '(haskell-process-use-presentation-mode nil)
 '(haskell-tags-on-save t)
 '(ido-confirm-unique-completion t)
 '(js-indent-level 2)
 '(lsp-prefer-flymake nil)
 '(package-selected-packages
   (quote
    (lsp-ui lsp-mode lsp cargo-mode company-racer flycheck-rust-mode rust-mode auto-package-update dante direnv gitignore-mode which-key use-package php-mode elm-mode logstash-conf camcorder dashboard savekill async company dash dash-functional find-file-in-project git-commit haskell-mode helm-core highlight-indentation ht ido-completing-read+ ivy js2-mode julia-mode know-your-http-well magit marshal mmm-mode pyvenv s simple-httpd ssass-mode tern typescript-mode with-editor magit-find-file git-gutter+ git-timemachine company-nixos-options nix-mode nixos-options racer cargo flycheck-rust intero "hindent" "hindent-mode" ag rjsx-mode request js2-highlight-vars jsx-mode vue-html-mode vue-mode company-restclient restclient js-doc web-mode company-tern stan-mode stan-snippets company-auctex auctex ample-theme ample-zen-theme monokai-theme anti-zenburn-theme solarized-theme silkworm-theme ess gist py-autopep8 elpy zenburn-theme yasnippet yaml-mode yagist visible-mark ujelly-theme toml-mode tidy tide tern-auto-complete tagedit soft-stone-theme smex smartparens skewer-mode register-list rainbow-delimiters pov-mode peg paredit nodejs-repl multiple-cursors markdown-mode magit-tramp magit-filenotify lua-mode less-css-mode jist jdee interaction-log ido-vertical-mode ido-ubiquitous hindent highlight helm guru-mode groovy-mode git-gutter-fringe gh flymake-easy flx-ido fill-column-indicator f expand-region ecb-snapshot dockerfile-mode docker csv-mode cryptol-mode color-theme-tango color-theme-solarized color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized anything angular-snippets angular-mode ac-etags)))
 '(python-shell-interpreter "python2")
 '(racer-rust-src-path
   "/home/uart14/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/lib/rustlib/src/rust/src")
 '(rust-rustfmt-bin "rustfmt")
 '(safe-local-variable-values
   (quote
    ((intero-targets "auth-service:lib" "auth-service:exe:auth-service" "auth-service:test:api-tests" "auth-service:test:unit-tests")
     (intero-targets "auth-service:lib" "auth-service:test:unit-tests")
     (intero-targets "servant-example:lib" "servant-example:test:servant-example-test")
     (intero-targets "testing:lib" "testing:test:testing-test")
     (intero-targets "jku-slides:lib" "jku-slides:exe:slides-server" "jku-slides:test:slides-server-test")
     (haskell-process-type quote stack-ghci)
     (js3-additional-externs quote
                             ("angular"))
     (haskell-compile-cabal-build-command . "cd %s; stack build --ghc-options -ferror-spans")
     (haskell-compile-cabal-build-command . "stack test --ghc-options -ferror-spans"))))
 '(sh-basic-offset 2)
 '(sh-indentation 2)
 '(show-paren-style (quote parenthesis))
 '(skewer-repl-strict-p t)
 '(sp-base-key-bindings (quote paredit))
 '(sp-ignore-modes-list (quote (minibuffer-inactive-mode emacs-lisp-mode)))
 '(sql-connection-alist
   (quote
    (("local-devel"
      (sql-product
       (quote postgres))
      (sql-user "postgres")
      (sql-server "localhost")
      (sql-database "postgres")
      (sql-port 5432))))))
