;;; JS

(require 'compile)
(require 'subr-x)

(use-package js2-mode)

(add-to-list 'compilation-error-regexp-alist-alist
             '(eslint "^\\(\w+\\):\\([0-9]+\\):\\([0-9]+\\):.*$" 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'eslint)

(add-to-list 'compilation-error-regexp-alist-alist
             '(jshint "^\\(.*\\): line \\([0-9]+\\), col \\([0-9]+\\), " 1 2 3))
(add-to-list 'compilation-error-regexp-alist 'jshint)

(defun find-root-git ()
  (locate-dominating-file default-directory ".git"))

(defun file-at-git-root (f)
  (concat (find-root-git) f))

(defun jshint-compile-command ()
  (concat
   "jshint " (file-at-git-root "")))

(defun eslint-compile-command ()
  (let* ((ignore (if (file-exists-p (file-at-git-root "/.eslintignore"))
                     "--ignore-path ./.eslintignore"
                   "" )))
    (concat
     "cd " (file-at-git-root "")
     " && eslint . --format unix "
     ignore
     )))

(defun compile-javascript ()
  (cond
   ((file-exists-p (file-at-git-root ".jshintrc")) (jshint-compile-command))
   ((or (file-exists-p (file-at-git-root ".eslintrc.yml"))
        (file-exists-p (file-at-git-root ".eslintrc.js"))
        (file-exists-p (file-at-git-root ".eslintrc.json")))
    (eslint-compile-command))))

(defvar npm-run-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map))

(define-derived-mode npm-run-mode special-mode "Npm-Run"
  "Major mode for npm rum"
  :group 'npm
  )


(defun npm-run ()
  (interactive)
  (let ((command (concat "cd " (file-at-git-root "") " && npm --color=false start"))
        (buf (get-buffer-create "*npm run*")))
    (pop-to-buffer buf nil t)
    (with-current-buffer buf
      (npm-run-mode)
      (read-only-mode t))
      (start-process "npm-run-process" buf "sh" "-c" command)
    ))

  ;; (async-shell-command (concat "cd " (file-at-git-root "") " && npm --color=false start")
  ;;                      "*npm start*"
  ;;                      ))


(defun node-load ()
  (interactive)
  (let* ((file buffer-file-name))
       (unless buffer-file-name (error "Buffer is not visiting a file"))
       (nodejs-repl-load-file file)))

(defun get-npm-bin-dir ()
  "Get project node bin directory"
  (string-trim (shell-command-to-string "npm bin"))
  )

(defun get-tern-command ()
  (let* ((node-bin (get-npm-bin-dir))
         (tern-bin (concat (file-name-as-directory node-bin) "tern")))
    tern-bin))

(setq js2-mode-hook
          (lambda ()
            ;; (smartparens-mode t)
            (setq compilation-read-command nil)
            (set (make-local-variable 'compile-command) "npm run build")
            (define-key js2-mode-map (kbd "C-c C-c") 'compile)
            (define-key js2-mode-map (kbd "C-C C-n") 'next-error)
            (define-key js2-mode-map (kbd "C-c C-p") 'previous-error)
            (define-key js2-mode-map (kbd "C-c C-r") 'npm-start)
            (define-key js2-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            ;; (set (make-local-variable 'tern-command) (list (get-tern-command)))
            ;; (setq 'tern-command )
            (flycheck-mode 1)
            (smartparens-strict-mode 1)
            ))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

(defun rjsx-mode-init ()
  (define-key rjsx-mode-map "<" #'self-insert-command)
  )

(add-hook 'rjsx-mode-hook #'rjsx-mode-init)


;;; HTML

(use-package company-tern
  :config
  (add-to-list 'company-backends 'company-tern))

(use-package tagedit
  :config
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features)
  :hook (html-mode . tagedit-mode))

(setq sgml-quick-keys 'close)

;;; Web mode
(use-package web-mode
  )
