;;; custom.el --- Customize settings -*- lexical-binding: t; -*-
;;
;; Only for settings that Emacs itself saves here (package lists, safe
;; local variables, ad-hoc Customize experiments).  It is loaded at the
;; start of init.el, so anything set in init.d/ takes precedence.  Move
;; anything worth keeping into the relevant init.d/ file.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(apheleia auto-compile cargo claude-code corfu cquery dap-mode
              deadgrep diff-hl diminish diredfl docker-compose-mode
              dockerfile-mode eat ellama embark-consult envrc ess esup
              expand-region flymake git-gutter-fringe gptel
              haskell-mode isend-mode magit majutsu marginalia minuet
              multiple-cursors nix-mode ollama-buddy orderless paredit
              poe-lootfilter-mode realgud-lldb rust-mode sops tagedit
              treemacs vagrant-tramp vertico vterm writegood-mode yaml
              yasnippet zenburn-theme))
 '(package-vc-selected-packages '((majutsu :url "https://github.com/0WD0/majutsu")))
 '(safe-local-variable-values
   '((haskell-process-type 'cabal-repl)
     (haskell-process-type :cabal-repl)
     (haskell-process-type "cabal-repl")
     (sql-connection-alist
      ("ukaa" (sql-product 'postgres) (sql-user "postgres")
       (sql-server "localhost") (sql-database "postgres")
       (sql-port 5432)
       (sql-default-directory "/docker:ukaa-database-test-1:"))
      ("ukaa-testing" (sql-product 'postgres) (sql-user "postgres")
       (sql-server "localhost") (sql-database "postgres")
       (sql-port 5432)
       (sql-default-directory "/docker:ukaa-testing-database:")))
     (sql-connection-alist
      ("ukaa" (sql-product 'postgres) (sql-user "postgres")
       (sql-server "localhost") (sql-database "postgres")
       (sql-port 5432)
       (sql-default-directory "/docker:ukaa-database-test-1:"))
      ("ukaa-testing" (sql-product 'postgres) (sql-user "postgres")
       (sql-server "localhost") (sql-database "postgres")
       (sql-port 5432)
       (sql-default-directory "/docker:ukaa-test-database:")))
     (sql-connection-alist
      ("ukaa" (sql-product 'postgres) (sql-user "postgres")
       (sql-server "localhost") (sql-database "postgres")
       (sql-port 5432)
       (sql-default-directory "/docker:ukaa-database-test-1:"))
      ("ukaa-testing" (sql-product 'postgres) (sql-user "postgres")
       (sql-server "localhost") (sql-database "postgres")
       (sql-port 5432)
       (sql-default-directory "/docker:ukaa-testing-database:"))
      ("uppsala" (sql-product 'postgres) (sql-user "postgres")
       (sql-server "localhost") (sql-database "postgres")
       (sql-port 5432)
       (sql-default-directory "/docker:ukaa-testing-database:")))
     (haskell-process-type . cabal-repl)
     (org-default-notes-file . "~/projects/nejla/app/notes.org")
     (org-default-notes-file
      . "~/projects/nejla/sambruk/ukaa/notes.org"))))
