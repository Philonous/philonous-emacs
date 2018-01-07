(let ((pg-site-file
       (substitute-in-file-name
        "$HOME/.emacs.d/other-packages/ProofGeneral/generic/proof-site.el")))
  (if (file-exists-p pg-site-file)
      (load-file pg-site-file)
    (message "Proof General is not available")))
