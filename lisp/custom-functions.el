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
;; Haskell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-haskell-buffer ()
  (save-window-excursion
    (if (bound-and-true-p intero-mode)
        (intero-repl-buffer nil t)
      (let ((sess (haskell-session)))
        (if sess (haskell-session-interactive-buffer sess)
          nil)))))

(defun layout-for-haskell ()
  (interactive)
  (save-selected-window
  (let ((haskell-buffer (get-haskell-buffer)))
  (progn
  (mapc 'delete-window (get-buffer-window-list haskell-buffer))
  (let ((buffer-next-window (window-buffer (next-window))))
  (progn
    (delete-other-windows)
    (let* ((right-window (split-window-right -80))
           (haskell-window (split-window-below -20))
           (middle-window (split-window-right 80)))
      (progn
        (set-window-buffer haskell-window haskell-buffer )
        (set-window-dedicated-p haskell-window t )
        (set-window-parameter haskell-window 'no-other-window t)
        (set-window-buffer right-window buffer-next-window )))))))))

(defun split-windows-threeway ()
    (interactive)
    (let ((buffer-next-window (window-buffer (next-window)))
          (buffer-previous-window (window-buffer (previous-window)))
          )
      (progn
        (delete-other-windows)
        (let* ((right-window (split-window-right (- (/ (window-total-width) 3))))
               (middle-window (split-window-right)))
          (set-window-buffer (selected-window) buffer-previous-window)
          (set-window-buffer right-window buffer-next-window)
          (select-window middle-window)
          ))))

(defun layout-for-haskell2 ()
  "3-column layout for 4k screen"
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

(defun layout-for-haskell-blackbird ()
  "2-column layout for laptop screen"
  (interactive)
  (save-selected-window
    (let ((haskell-buffer (get-haskell-buffer)))
      (progn
        (mapc 'delete-window (get-buffer-window-list haskell-buffer))
        (let ((buffer-next-window (window-buffer (next-window))))
          (progn
            (delete-other-windows)
            (let* ((right-window (split-window-right (- (/ (window-total-width) 2))))
                   (haskell-window (split-window-below -20)))
              (progn
                (set-window-buffer haskell-window haskell-buffer )
                (set-window-dedicated-p haskell-window t )
                ;; (with-selected-window compile-window
                ;;   (switch-to-buffer "*haskell-compilation*")
                ;;   (set-window-dedicated-p compile-window t ))
                (set-window-parameter haskell-window 'no-other-window t)
                (set-window-buffer right-window buffer-next-window )))))))))

(defun layout-for-haskell-choose-by-hostname ()
  "Split windows depending on hostname"
  (interactive)
  (let* ((hostname (system-name)))
    (cond
     ((string= hostname "blackbird") (layout-for-haskell-blackbird))
     ((string= hostname "tukan") (layout-for-haskell2))
     (t (layout-for-haskell2)))))

(defun haskell-tag-find (ident &optional next-p)
  "Jump to tag"
  (interactive "MIdentifier to jump to: ")
  (let ((tags-file-dir (haskell-cabal--find-tags-dir))
        (tags-revert-without-query t))
    (when (and ident
               (not (string= "" (haskell-string-trim ident)))
               tags-file-dir)
      (let ((tags-file-name (concat tags-file-dir "TAGS")))
        (cond ((file-exists-p tags-file-name)
               (let ((xref-prompt-for-identifier next-p))
                 (xref-find-definitions ident)))
              (t (haskell-mode-generate-tags ident)))))))


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

(defun haskell-process-save-current-window ()
  (setq haskell-process-old-window (selected-window)))

(defun haskell-go-to-haskell-window ()
  (interactive)
  (let* ((haskell-buffer (get-haskell-buffer))
         (haskell-window (get-buffer-window haskell-buffer)))
    (if haskell-window (progn (haskell-process-save-current-window)
                              (select-window haskell-window)))))

(defun haskell-go-to-old-window ()
  (interactive)
  (if haskell-process-old-window
      (select-window haskell-process-old-window)))

(defun haskell-repl-clear-buffer ()
  (interactive)
  (cond
   ((string= major-mode "intero-repl-mode") (intero-repl-clear-buffer))
   ((string= major-mode "haskell-interactive-mode") (haskell-interactive-mode-clear))))

(defun haskell-clear-interactive-window ()
  (interactive)
  (let* ((haskell-buffer (get-haskell-buffer)))
    (if haskell-buffer (with-current-buffer haskell-buffer
                         (haskell-repl-clear-buffer)))))

(defun haskell-clear-process-log ()
  (interactive)
  (let ((log-buffer (get-buffer "*haskell-process-log*")))
    (when log-buffer (with-current-buffer log-buffer
                       (let ((inhibit-read-only t))
                         (erase-buffer))))))

;; (defadvice haskell-interactive-mode-reset-error (after clear-process-log activate)
;;   "clear the haskell process log when resetting errors"
;;   (haskell-clear-process-log))

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

(defun underscore-to-camelcase (rbeg rend)
  "Convert underscores to camelCase"
  (interactive "r")
  (destructuring-bind (beg . end)
      (if (use-region-p)
          (cons rbeg rend)
        (bounds-of-thing-at-point 'symbol))
    (goto-char beg)
    (if (looking-at "\\([[:alnum:]]+\\)_")
        (progn
          (replace-match (concat (s-downcase (match-string 1)) "_") t)
          (goto-char beg)
          (while (re-search-forward "_\\([[:alnum:]]+\\)" end t)
            (replace-match (s-capitalize (match-string 1)) t)))))
  )


(defun camelcase-to-underscore (rbeg rend)
  "Convert underscores to camelCase"
  (interactive "r")
  (let ((case-fold-search nil))
    (destructuring-bind (beg . end)
        (if (use-region-p) (cons rbeg rend) (bounds-of-thing-at-point 'symbol))
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


(defun decap (str)
  "Transform the first letter of STR to lower case"
  (if (< (length str) 1)
      ""
    (concat (downcase (substring str 0 1)) (substring str 1))))

(defun cap (str)
  "Transform the first letter of STR to lower case"
  (if (< (length str) 1)
      ""
    (concat (upcase (substring str 0 1)) (substring str 1))))


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


(defun haskell-find-definition-at-point ()
  (interactive)
  (let* ((word (word-at-point))
         (session (or (haskell-session-maybe) (error "No haskell session")))
         (process (or (haskell-session-process session) (error "No session process")))
         (response (haskell-process-queue-sync-request
                    process
                    (concat ":i " word)))
         )
    (if (string-match "[^-]*-- Defined at \\([^:]+\\):.*" response)
        ;; (find-file (match-string 1 response)
        (message response)
        )
      ))


(defun comint-clear-interactive-buffer ()
  "Clear the interactive window"
  (interactive)
  (require 'comint)
  (comint-clear-buffer))

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun duplicate-region ()
  (interactive)
  (let* ((region-text (buffer-substring (region-beginning) (region-end))))
    (goto-char (region-end))
    (unless (= (point) (line-beginning-position)) (insert "\n"))
    (save-excursion
      (insert region-text))))

(defun duplicate-line (&optional prefix)
  (interactive "p")
  (save-excursion  (let* ((line (buffer-substring (line-beginning-position) (line-end-position))))
                     (end-of-line)
                     (loop for i from 1 upto (max 1 prefix)
                           do (progn (insert "\n")
                                     (insert line))
                     )
                     ))
  (next-line)
  )

(defun duplicate-line-or-region ()
  (interactive)
  (if (region-active-p)
      (call-interactively 'duplicate-region)
    (call-interactively 'duplicate-line)))

(defun open-with (program)
  (interactive "MProgram: ")
  (call-process program nil nil nil buffer-file-name))

(provide 'custom-functions)

(defun open-project-todo ()
  (interactive)
  (let ((dir (or (locate-dominating-file default-directory "TODO.org")
                 (projectile-project-root))))
    (find-file-other-window (concat dir "/" "TODO.org"))))
