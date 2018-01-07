(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-PDF-mode t)

(defun pdfevince ()
   (add-to-list 'TeX-output-view-style
                 '("^pdf$" "." "evince %o %(outpage)")))

(defun pdfokular ()
   (add-to-list 'TeX-output-view-style
                 '("^pdf$" "." "okular %o %(outpage)")))

(add-hook  'LaTeX-mode-hook  'pdfevince  t) ; AUCTeX LaTeX mode
;; (add-hook  'LaTeX-mode-hook  'pdfokular  t) ; AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
