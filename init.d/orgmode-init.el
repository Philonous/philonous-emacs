;;; -*- lexical-binding: t; -*-
(use-package org
  :hook (org-mode . my/org-mode-init)
  :custom
  (org-todo-keywords
   '((sequence "TODO" "BLOCKED" "WORKING" "|" "DONE" "DELEGATED")))
  :bind
  (("C-c n t" . org-capture)
   ("C-c n n" . my/org-open-notes))
  :init
  (defun my/org-open-notes ()
    "Open the default org notes file."
    (interactive)
    (require 'org)
    (find-file org-default-notes-file))
  :config
  (defun my/org-mode-init ()
    (define-key org-mode-map (kbd "C-<tab>") nil)
    (define-key org-mode-map (kbd "M-e") nil)))
