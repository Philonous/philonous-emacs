;;; -*- lexical-binding: t; -*-
(use-package org
  :custom
  (org-todo-keywords
   '((sequence "TODO" "|" "DONE")))
  :preface
  (defun my/org-todo-previous ()
    "Cycle TODO state backwards."
    (interactive)
    (org-todo 'left))
  :bind
  (("C-c n t" . org-capture)
   ("C-c n n" . my/org-open-notes)
   :map org-mode-map
   ;; Let global movement through
   ("S-<up>"    . nil)
   ("S-<down>"  . nil)
   ("S-<left>"  . nil)
   ("S-<right>" . nil)
   ("C-<tab>"   . nil)
   ("M-e"       . nil)
   ;; TODO state cycling
   ("M-<right>" . org-todo)
   ("M-<left>"  . my/org-todo-previous)
   )
  :init
  (defun my/org-open-notes ()
    "Open the default org notes file."
    (interactive)
    (require 'org)
    (find-file org-default-notes-file))
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((emacs-lisp . t)
                                 (shell . t)
                                 (eshell . t)
                                 ))
  )
