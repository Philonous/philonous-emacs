(require 'sh-script)

(defun sh-mode-init ()
    "initialize sh mode"
    (interactive)
    (flycheck-mode t)
    )

(add-hook 'sh-mode-hook #'sh-mode-init)
