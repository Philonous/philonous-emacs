(require 'org)
(setq org-todo-keywords
  '((sequence "TODO" "BLOCKED" "WORKING" "|" "DONE" "DELEGATED")))

(defun org-mode-init ()
  (define-key org-mode-map (kbd "C-<tab>") nil)
  (define-key org-mode-map (kbd "M-e") nil)
  )

(add-hook 'org-mode-hook 'org-mode-init)


(global-set-key (kbd "C-c C-n C-t") #'org-capture)
(global-set-key (kbd "C-c C-n C-n") (lambda () (interactive) (find-file org-default-notes-file)))
