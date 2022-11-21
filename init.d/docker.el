(use-package dockerfile-mode)

(add-hook 'dockerfile-mode-hook #'flycheck-mode)

(use-package docker-compose-mode)
