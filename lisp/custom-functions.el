(defun add-subdirs-to-load-path (dir)
  "Read subdirectories and prepend them to load-path"
  (let* ((base-dir (file-name-as-directory dir))
         (candidate-files (directory-files base-dir))
         (dirs (delq nil (mapcar (lambda (file)
                                   (let ((filename (concat base-dir file)))
                                     (if (and (file-directory-p filename)
                                              (not (string= file "."))
                                              (not (string= file "..")) )
                                         filename
                                       nil)))
                                 candidate-files))))
    (mapc (lambda (file) (add-to-list 'load-path file)) dirs)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  ((let ((name (buffer-name))
    (filename (buffer-file-name)))
    (if (not filename)
    (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
      (message "A buffer named '%s' already exists!" new-name)
    (progn
      (rename-file name new-name 1)
      (rename-buffer new-name)
      (set-visited-file-name new-name)
      (set-buffer-modified-p nil)))))))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "0123456789")
  (or (looking-at "[0123456789]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(require 'rect)
(defvar enumerate-line-num)
(defun enumerate-line (start end fmt)
  (string-rectangle-line start end (format fmt enumerate-line-num) t)
  (incf enumerate-line-num))
(defun enumerate-rectangle (start end &optional first-number)
  "Replace the region-rectangle with numbers beginning at 1 and incrementing for each line.

You can use the universal argument to change the initial value.
For example, to start counting lines at zero:

C-u 0 M-x enumerate-rectangle"
  (interactive "*r\np")
  (setq enumerate-line-num first-number)
  (let (line0 lineN fmt)
    (save-excursion
      (goto-char start)
      (setq line0 (line-number-at-pos))
      (goto-char end)
      (setq lineN (line-number-at-pos)))
    (setq fmt (concatenate 'string
                           "%"
                           (format "%0d" (string-width (format "%0d" (+ enumerate-line-num (- lineN line0)))))
                           ".1d"))
    (apply-on-rectangle 'enumerate-line start end fmt)))


(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurrence of CHAR.
 Case is ignored if `case-fold-search' is non-nil in the current buffer.
 Goes backward if ARG is negative; error if CHAR not found.
 Ignores CHAR at point."
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))

(defun get-haskell-buffer ()
  (save-window-excursion
    (let ((sess (haskell-session)))
       (if sess (haskell-session-interactive-buffer sess)
                nil))))

(defun layout-for-haskell ()
  (interactive)
  (save-selected-window
  (let ((haskell-buffer (get-haskell-buffer)))
  (progn
  (mapc 'delete-window (get-buffer-window-list haskell-buffer))
  (let ((buffer-next-window (window-buffer (next-window))))
  (progn
    (delete-other-windows)
    (let* ((right-window (split-window-right (- (/ (window-total-width) 3))))
           (haskell-window (split-window-below -20))
           (middle-window (split-window-right)))
      (progn
        (set-window-buffer haskell-window haskell-buffer )
        (set-window-dedicated-p haskell-window t )
        (set-window-parameter haskell-window 'no-other-window t)
        (set-window-buffer right-window buffer-next-window )))))))))

(defun layout-for-haskell2 ()
  (interactive)
  (save-selected-window
  (let ((haskell-buffer (get-haskell-buffer)))
  (progn
  (mapc 'delete-window (get-buffer-window-list haskell-buffer))
  (let ((buffer-next-window (window-buffer (next-window))))
  (progn
    (delete-other-windows)
    (let* ((right-window (split-window-right (- (/ (window-total-width) 3))))
           (middle-window (split-window-right))
           (haskell-window (split-window-below -20))
           (compile-window (with-selected-window right-window
                             (split-window-below -20))))
      (progn
        (set-window-buffer haskell-window haskell-buffer )
        (set-window-dedicated-p haskell-window t )
        (with-selected-window compile-window
          (switch-to-buffer "*haskell-compilation*")
          (set-window-dedicated-p compile-window t ))
        (set-window-parameter haskell-window 'no-other-window t)
        (set-window-buffer right-window buffer-next-window )))))))))

;; (defun layout-for-haskell ()
;;   (interactive)
;;   (save-selected-window
;;   (let ((haskell-buffer (get-haskell-buffer)))
;;   (progn
;;   (mapc 'delete-window (get-buffer-window-list haskell-buffer))
;;   (let ((buffer-next-window (window-buffer (next-window))))
;;   (progn
;;     (delete-other-windows)
;;     (let ((haskell-window (split-window-below -13))
;;           (right-window   (split-window-right)))
;;       (progn
;;         (set-window-buffer haskell-window haskell-buffer )
;;         (set-window-dedicated-p haskell-window t )
;;         (set-window-parameter haskell-window 'no-other-window t)
;;         (set-window-buffer right-window buffer-next-window )))))))))

(defvar haskell-process-old-window nil)

(defun haskell-go-to-haskell-window ()
  (interactive)
  (let ((sess (haskell-session)))
    (if sess
       (let* ((haskell-buffer (haskell-session-interactive-buffer sess))
              (haskell-window (get-buffer-window haskell-buffer)))
          (if haskell-window (progn (setq haskell-process-old-window (selected-window))
                                    (select-window haskell-window)

                                    ))))))

(defun haskell-go-to-old-window ()
  (interactive)
  (if haskell-process-old-window
      (select-window haskell-process-old-window)))

(defun haskell-clear-interactive-window ()
  (interactive)
  (let ((sess (haskell-session)))
    (if sess
       (let ((haskell-buffer (haskell-session-interactive-buffer sess)))
         (if haskell-buffer (with-current-buffer haskell-buffer
                              (haskell-interactive-mode-clear)))))))

(defun haskell-clear-process-log ()
  (interactive)
  (let ((log-buffer (get-buffer "*haskell-process-log*")))
    (when log-buffer (with-current-buffer log-buffer
                       (let ((inhibit-read-only t))
                         (erase-buffer))))))

(defadvice haskell-interactive-mode-reset-error (after clear-process-log activate)
  "clear the haskell process log when resetting errors"
  (haskell-clear-process-log))

(defun send-text-to-haskell-process ()
  "Send the current line to the haskell process."
  (interactive)
  (let (pos1 pos2 bds)
    (if (region-active-p)
        (setq pos1 (region-beginning) pos2 (region-end))
        (progn
          (let ((bds (bounds-of-thing-at-point 'line)))
            (setq pos1 (car bds) pos2 (cdr bds)))))
    (haskell-process-do-simple-echo (buffer-substring-no-properties pos1 pos2)
                                    'haskell-mode)))

(defun flysspell-region-or-buffer ()
  "run flyspell-region when region is active and flyspell-buffer otherwise"
  (interactive)
  (if (region-active-p)
      (let ((pos1 (region-beginning)) (pos2 (region-end)))
            (flyspell-region pos1 pos2))
      (flyspell-buffer)))

(defun downcase-word-or-region ()
  "Downcase current word or region."
(interactive)
(let (pos1 pos2 bds)
  (if (region-active-p)
     (setq pos1 (region-beginning) pos2 (region-end))
    (progn
      (setq bds (bounds-of-thing-at-point 'symbol))
      (setq pos1 (car bds) pos2 (cdr bds))))

  ;; now, pos1 and pos2 are the starting and ending positions of the
  ;; current word, or current text selection if exist.
  (downcase-region pos1 pos2)
  ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun upcase-rectangle (b e)
  "change chars in rectangle to uppercase"
  (interactive "r")
  (apply-on-rectangle 'upcase-rectangle-line b e))

(defun downcase-rectangle (b e)
  "change chars in rectangle to uppercase"
  (interactive "r")
  (apply-on-rectangle 'downcase-rectangle-line b e))

(defun upcase-rectangle-line (startcol endcol)
  (when (= (move-to-column startcol) startcol)
    (upcase-region (point)
                   (progn (move-to-column endcol 'coerce)
                          (point)))))

(defun downcase-rectangle-line (startcol endcol)
  (when (= (move-to-column startcol) startcol)
    (downcase-region (point)
                   (progn (move-to-column endcol 'coerce)
                          (point)))))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (not (buffer-modified-p)))
        (revert-buffer t t t) )))
  (message "Refreshed open files.") )


(defun abbreviate-module (module-name)
  (let ((parts (split-string module-name "[.]")))
  (mapconcat (lambda (str) (substring str 0 1)) parts ""))):

(defun underscore-to-camelcase ()
  "Convert underscores to camelCase"
  (interactive)
  (destructuring-bind (beg . end) (bounds-of-thing-at-point 'word)
    (goto-char beg)
    (while (re-search-forward "_\\([a-z]\\)" end t)
      (replace-match (upcase (match-string 1))))))

(defun camelcase-to-underscore ()
  "Convert underscores to camelCase"
  (interactive)
  (let ((case-fold-search nil))
    (destructuring-bind (beg . end) (bounds-of-thing-at-point 'word)
      (goto-char beg)
      (while (re-search-forward "\\([a-z0-9]\\)\\([A-Z]\\)" end t)
        (replace-match (concat (match-string-no-properties 1)
                             "_"
                             (downcase (match-string-no-properties 2))))))))

(defun swap-underscore-camelcase ()
  "convert udnerscores to camelCase if word contains underscores
and the other way around otherwise"
  (interactive)
  (save-excursion
    (destructuring-bind (beg . end) (bounds-of-thing-at-point 'word)
      (goto-char beg)
      (cond
       ((save-excursion (re-search-forward "_" end t))
        (underscore-to-camelcase))
       (t (camelcase-to-underscore))))))

(defun smart-hyphen (n)
  "Capitalize the next word, or behave as the usual '-'."
  (interactive "p")
  (if (memq (get-text-property (point) 'face)
            '(font-lock-doc-face
              font-lock-comment-face
              font-lock-string-face))
      (self-insert-command n)
    (progn (insert ?-)
           (let ((next (read-char)))
             (insert (if (eq ?w (char-syntax next))
                         (progn
                           (delete-char -1)
                           (upcase next))
                       next))))))


(defun byte-compile-reload-dir ()
  "Byte-compile and reload everything."
  (interactive)
  (let ((byte-compile-warnings '(free-vars unresolved callargs redefine make-local mapcar constants suspicious)))
    (loop for file in (directory-files (file-name-directory (or load-file-name
                                                                (buffer-file-name)))
                                       nil
                                       "^[a-z0-9-]+\\.el$")
          do (byte-recompile-file file t 0 t))))

(defun fill-hyphens (n)
  (dotimes (i n) (insert "-")))

(defun section-heading ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (looking-at "^[ ]*\\\(--\\\)?[ ]*")
    (replace-match "-- ")
    (re-search-forward "[ ]*[-]*[ ]*$" (line-end-position))
    (replace-match " ")
    (end-of-line)
    (let* ((hyphens (- 80 (current-column))))
      (fill-hyphens hyphens))
    (previous-line)
    (beginning-of-line)
    (unless (looking-at "--[-]*$")
      (end-of-line)
      (newline)
      (fill-hyphens 80))
    (forward-line 2)
    (when (and (eobp) (not (= (current-column) 0)) (newline)))
    (beginning-of-line)
    (unless (looking-at "--[-]*$")
      (open-line 1)
      (fill-hyphens 80))
    (previous-line)
    ))

(provide 'custom-functions)
