;;; unicode-inspect-mode.el --- inspect a buffer codepoint by codepoint -*- lexical-binding: t -*-

(defgroup unicode-inspect nil
  "Inspect the Unicode content of a buffer."
  :group 'convenience)

(defface unicode-inspect-nonprintable-face
  '((t :foreground "dark red" :weight bold))
  "Face for non-printable characters and their stand-in glyph.")

(defface unicode-inspect-grapheme-face
  '((((background light)) :background "grey85")
    (((background dark))  :background "grey25"))
  "Face for multi-codepoint grapheme clusters.")

(defcustom unicode-inspect-replacement-char ?•
  "Glyph used in place of non-printable characters."
  :type 'character)

(defcustom unicode-inspect-newline-marker ?↵
  "Glyph shown before each newline (the newline itself still breaks the line)."
  :type 'character)

(defun unicode-inspect--printable-p (ch)
  "Non-nil when CH is displayable and not in a control/format category."
  (and (char-displayable-p ch)
       (not (memq (get-char-code-property ch 'general-category)
                  '(Cc Cf Cn Cs)))))

(defun unicode-inspect--clear (beg end)
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'unicode-inspect)
      (delete-overlay ov))))

(defun unicode-inspect--annotate (beg end)
  "Walk BEG..END, attaching overlays for clusters and non-printables."
  (unicode-inspect--clear beg end)
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let* ((pos (point))
             (comp (find-composition pos end nil t)))
        (cond
         ;; Multi-codepoint composition → grapheme cluster
         ((and comp (> (- (nth 1 comp) (nth 0 comp)) 1))
          (let ((ov (make-overlay (nth 0 comp) (nth 1 comp))))
            (overlay-put ov 'unicode-inspect t)
            (overlay-put ov 'face 'unicode-inspect-grapheme-face)
            (overlay-put ov 'priority 10))
          (goto-char (nth 1 comp)))
         ;; Single codepoint
         (t
          (let ((ch (char-after pos)))
            (cond
             ((eq ch ?\n)
              (let ((ov (make-overlay pos (1+ pos))))
                (overlay-put ov 'unicode-inspect t)
                (overlay-put ov 'before-string
                             (propertize (string unicode-inspect-newline-marker)
                                         'face 'unicode-inspect-nonprintable-face))))
             ((not (unicode-inspect--printable-p ch))
              (let ((ov (make-overlay pos (1+ pos))))
                (overlay-put ov 'unicode-inspect t)
                (overlay-put ov 'display
                             (propertize (string unicode-inspect-replacement-char)
                                         'face 'unicode-inspect-nonprintable-face))))))
          (forward-char 1)))))))

(defun unicode-inspect--after-change (beg end _len)
  ;; Be generous with boundaries — compositions may straddle the change.
  (unicode-inspect--annotate
   (max (point-min) (- beg 4))
   (min (point-max) (+ end 4))))

;;;###autoload
(define-derived-mode unicode-inspect-mode fundamental-mode "U-Inspect"
  "Major mode that exposes the Unicode skeleton of a buffer."
  (setq-local eldoc-idle-delay 0.1)
  (add-hook 'eldoc-documentation-functions #'describe-char-eldoc nil t)
  (add-hook 'after-change-functions #'unicode-inspect--after-change nil t)
  (eldoc-mode 1)
  (unicode-inspect--annotate (point-min) (point-max)))

(provide 'unicode-inspect-mode)
