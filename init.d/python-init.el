;; (setenv "PYTHONPATH" "/usr/lib/python2.7/site-packages")
;; (setq
;;  python-shell-interpreter "python3"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp ">>> ")

(defun python-mode-init ()
  ""
  (setenv "IPY_TEST_SIMPLE_PROMPT" "1")
  (elpy-mode 1)
  (elpy-use-ipython)

  )

(setq python-mode-hook 'python-mode-init)

(defun elpy-mode-init ()
  (py-autopep8-enable-on-save)
  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  )

(add-hook 'elpy-mode-hook 'elpy-mode-init)
