;;; -*- lexical-binding: t; -*-
(defun my/dired-two-pane ()
  "Orthodox file manager mode: two dired buffers, side by side."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (unless (derived-mode-p 'dired-mode)
    (dired default-directory))
  (other-window 1)
  (unless (derived-mode-p 'dired-mode)
    (dired default-directory)))

(defun my/dired-next-pane ()
  "Cycle to the next dired window, if any."
  (interactive)
  (let* ((dired-wins (cl-remove-if-not
                      (lambda (w)
                        (with-current-buffer (window-buffer w)
                          (derived-mode-p 'dired-mode)))
                      (window-list)))
         (rest (cdr (memq (selected-window) dired-wins))))
    (when-let ((next (or (car rest) (car dired-wins))))
      (unless (eq next (selected-window))
        (select-window next)))))

(use-package dired
  :ensure nil ; Don't try to install from melpa
  :bind
  (:map dired-mode-map
        ("<f7>" . make-directory)
        ("<tab>" . my/dired-next-pane)
        ("<f11>" . my/dired-two-pane)
        )
  :config
  (setopt dired-dwim-target t)
  )
