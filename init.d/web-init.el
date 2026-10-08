;;; -*- lexical-binding: t; -*-
;;; JS

(require 'compile)
(require 'subr-x)

(add-to-list 'compilation-error-regexp-alist-alist
             '(eslint "^\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\):.*$" 1 2 3))
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


;;; HTML

(use-package tagedit
  :config
  (tagedit-add-paredit-like-keybindings)
  (tagedit-add-experimental-features)
  :hook (html-mode . tagedit-mode))

(setq sgml-quick-keys 'close)
