;;; -*- lexical-binding: t; -*-
(defcustom nejla-share-directory "~/nejla/downloads"
  "Directory where nejla_share downloads are stored."
  :type 'directory
  :group 'nejla)

(defcustom nejla-share-preview-max 1
  "Maximum number of files to show in directory content preview."
  :type 'integer
  :group 'nejla)

(defcustom nejla-share-own-user "philipp"
  "Username to identify own shares, which are hidden by default.
Matched against the name portion of share filenames (the part after
the timestamp prefix, e.g. \"alice\" in \"20260219_083312_alice\")."
  :type '(choice (const :tag "None" nil) string)
  :group 'nejla)

(defcustom nejla-share-old-days 30
  "Entries older than this many days are hidden unless marked important."
  :type 'integer
  :group 'nejla)

(defcustom nejla-share-done-hours 24
  "Entries done for longer than this many hours are hidden."
  :type 'integer
  :group 'nejla)

(defun nejla-share-open (filename)
  "Download (if needed) and open a nejla_share archive.
FILENAME should be of the form \"20260219_083312_name.gpg\"."
  (interactive
   (list (read-string "Share filename (.gpg): ")))
  (let* ((dirname (file-name-sans-extension filename))
         (target (expand-file-name dirname nejla-share-directory)))
    (unless (file-directory-p target)
      (message "Downloading %s..." filename)
      (let ((default-directory nejla-share-directory))
        (unless (zerop (call-process "nejla_share" nil "*nejla_share*" nil
                                     "download" filename))
          (error "nejla_share download failed—see *nejla_share* buffer"))))
    (dired target)))

;; --- Helpers ---

(defun nejla-share--remote-list ()
  "Return list of filenames from `nejla_share list'."
  (let ((output (with-output-to-string
                  (with-current-buffer standard-output
                    (unless (zerop (call-process "nejla_share" nil t nil "list"))
                      (error "nejla_share list failed"))))))
    (split-string (string-trim output) "\n" t)))

(defun nejla-share--local-dirs ()
  "Return list of directory basenames in `nejla-share-directory'."
  (when (file-directory-p nejla-share-directory)
    (cl-remove-if-not
     (lambda (f)
       (file-directory-p (expand-file-name f nejla-share-directory)))
     (directory-files nejla-share-directory nil "\\`[^.]"))))

(defun nejla-share--parse-name (base)
  "Parse BASE into (name date-time-string) or nil if no pattern match."
  (when (string-match "\\`\\([0-9]\\{8\\}\\)_\\([0-9]\\{6\\}\\)_\\(.*\\)\\'" base)
    (let* ((ds (match-string 1 base))
           (ts (match-string 2 base))
           (name (match-string 3 base))
           (year  (string-to-number (substring ds 0 4)))
           (month (string-to-number (substring ds 4 6)))
           (day   (string-to-number (substring ds 6 8)))
           (hour  (string-to-number (substring ts 0 2)))
           (min   (string-to-number (substring ts 2 4)))
           (dow   (format-time-string "%a" (encode-time 0 0 0 day month year))))
      (list name (format "%s %02d-%02d-%04d %02d:%02d" dow day month year hour min)))))

(defun nejla-share--format-kb (bytes)
  "Format BYTES as KB with underscore thousands separators."
  (let* ((kb (max 1 (/ (+ bytes 512) 1024)))
         (s (number-to-string kb))
         (out ""))
    (while (> (length s) 3)
      (setq out (concat "_" (substring s -3) out))
      (setq s (substring s 0 -3)))
    (concat s out)))

(defun nejla-share--all-files (dir)
  "Return all files under DIR recursively, excluding .notes and dotfiles.
Paths are relative to DIR."
  (let ((abs (directory-files-recursively dir "\\`[^.]" nil)))
    (cl-remove-if
     (lambda (f) (string-match-p "/\\." f))
     (mapcar (lambda (f)
               (let ((rel (file-relative-name f dir)))
                 (unless (string-match-p "\\.notes\\'" rel) rel)))
             abs))))

(defun nejla-share--text-file-p (path)
  "Heuristic: return non-nil if PATH is likely a text file."
  (and (file-regular-p path)
       (< (file-attribute-size (file-attributes path)) 1048576)
       (let ((ext (file-name-extension path)))
         (not (member (downcase (or ext ""))
                      '("gpg" "gz" "zip" "tar" "bz2" "xz" "zst"
                        "png" "jpg" "jpeg" "gif" "bmp" "pdf"
                        "mp3" "mp4" "mkv" "avi" "wav" "ogg"
                        "bin" "exe" "so" "o" "a" "dll"))))))

(defun nejla-share--first-nonblank-line (path)
  "Return first non-blank line of PATH, or nil."
  (when (nejla-share--text-file-p path)
    (with-temp-buffer
      (insert-file-contents path nil 0 4096)
      (goto-char (point-min))
      (while (and (not (eobp))
                  (looking-at-p "\\`[ \t]*$"))
        (forward-line 1))
      (unless (eobp)
        (string-trim (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))))))

(defun nejla-share--read-timestamp-file (path)
  "Read an ISO 8601 timestamp from PATH and return its float-time, or nil."
  (when (file-exists-p path)
    (condition-case nil
        (let ((ts (with-temp-buffer
                    (insert-file-contents path nil 0 64)
                    (string-trim (buffer-string)))))
          (when (> (length ts) 0)
            (float-time (date-to-time ts))))
      (error nil))))

(defun nejla-share--entry-age-days (base)
  "Return age in days of entry BASE based on filename timestamp, or nil."
  (when (string-match "\\`\\([0-9]\\{8\\}\\)_" base)
    (let* ((ds (match-string 1 base))
           (year  (string-to-number (substring ds 0 4)))
           (month (string-to-number (substring ds 4 6)))
           (day   (string-to-number (substring ds 6 8)))
           (entry-time (float-time (encode-time 0 0 0 day month year))))
      (/ (- (float-time) entry-time) 86400.0))))

(defun nejla-share--entry-own-p (base)
  "Return non-nil if BASE belongs to the current user."
  (and nejla-share-own-user
       (let ((parsed (nejla-share--parse-name base)))
         (and parsed (string= (car parsed) nejla-share-own-user)))))

(defun nejla-share--entry-hidden-p (base info)
  "Return non-nil if entry BASE with INFO plist should be hidden by default.
INFO may be nil for non-downloaded entries."
  (let* ((important (and info (plist-get info :important)))
         (hidden-flag (and info (plist-get info :hidden)))
         (done-time (and info (plist-get info :done-time)))
         (age-days (nejla-share--entry-age-days base)))
    (or
     ;; Done for more than N hours — even if important
     (and done-time
          (> (- (float-time) done-time) (* nejla-share-done-hours 3600)))
     ;; Explicitly hidden and not important
     (and hidden-flag (not important))
     ;; Old and not important
     (and age-days (> age-days nejla-share-old-days) (not important))
     ;; Own user's entries and not important
     (and (nejla-share--entry-own-p base) (not important)))))

(defun nejla-share--dir-info (dirname)
  "Return plist of info for DIRNAME.
Keys: :size :count :files :sole-file :notes :done :done-time
      :important :hidden :hint"
  (let* ((full (expand-file-name dirname nejla-share-directory))
         (notes-path (expand-file-name ".notes" full))
         (has-notes (file-exists-p notes-path))
         (done-path (expand-file-name ".done" full))
         (has-done (file-exists-p done-path))
         (done-time (nejla-share--read-timestamp-file done-path))
         (important-path (expand-file-name ".important" full))
         (has-important (file-exists-p important-path))
         (hidden-path (expand-file-name ".hidden" full))
         (has-hidden (file-exists-p hidden-path))
         (notes-line (when has-notes
                       (with-temp-buffer
                         (insert-file-contents notes-path nil 0 4096)
                         (goto-char (point-min))
                         (string-trim (buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position))))))
         (all-files (delq nil (nejla-share--all-files full)))
         (file-count (length all-files))
         (sole (when (= file-count 1) (car all-files)))
         (total-size 0)
         (hint (cond
                (has-notes notes-line)
                (sole (nejla-share--first-nonblank-line
                       (expand-file-name sole full)))
                (t nil))))
    (dolist (f (directory-files full t "\\`[^.]"))
      (unless (file-directory-p f)
        (cl-incf total-size (or (file-attribute-size (file-attributes f)) 0))))
    (list :size total-size
          :count file-count
          :files all-files
          :sole-file sole
          :notes (when has-notes notes-path)
          :done has-done
          :done-time done-time
          :important has-important
          :hidden has-hidden
          :hint (or hint ""))))

(defun nejla-share--preview-string (info)
  "Format file listing from INFO plist."
  (let* ((sole (plist-get info :sole-file))
         (files (plist-get info :files)))
    (if sole
        sole
      (let* ((shown (seq-take files nejla-share-preview-max))
             (overflow (- (length files) (length shown)))
             (listing (mapconcat #'identity shown ", ")))
        (if (> overflow 0)
            (format "%s … +%d more" listing overflow)
          listing)))))

;; --- Rendering ---

(defun nejla-share--entry-display-name (base)
  "Return the display name for BASE (parsed name or raw base)."
  (let ((parsed (nejla-share--parse-name base)))
    (if parsed (car parsed) base)))

(defun nejla-share--insert-entry (gpg-file base name-fmt info hidden-p)
  "Insert one formatted line for an entry.
INFO is the dir-info plist (nil if not downloaded).
HIDDEN-P means this is a hidden entry being shown with indicator."
  (let* ((parsed (nejla-share--parse-name base))
         (downloaded-p (not (null info)))
         (important (and info (plist-get info :important)))
         (prefix (if hidden-p "h " "  "))
         (beg (point)))
    (if (null parsed)
        ;; Unparseable name — print raw
        (insert prefix (propertize base 'face 'dired-directory) "\n")
      (let* ((name (car parsed))
             (date (cadr parsed))
             (name-face (cond (important 'warning)
                              ((not downloaded-p) 'dired-symlink)
                              (t 'dired-directory)))
             (meta-face 'shadow))
        (insert prefix)
        (insert (propertize (format name-fmt name) 'face name-face))
        (if downloaded-p
            (let* ((preview (nejla-share--preview-string info))
                   (hint (plist-get info :hint))
                   (sole (plist-get info :sole-file))
                   (done (plist-get info :done))
                   (full-dir (expand-file-name base nejla-share-directory)))
              (insert (propertize (format "  %8s KB  %s  "
                                          (nejla-share--format-kb
                                           (plist-get info :size))
                                          date)
                                  'face meta-face))
              (insert (propertize (if done "[X]" "[ ]")
                                  'face (if done 'success 'shadow)))
              (insert (propertize (format "  %3d files  "
                                          (plist-get info :count))
                                  'face meta-face))
              (insert (propertize preview 'face 'dired-ignored))
              (when (> (length hint) 0)
                (let ((display (if (> (length hint) 20)
                                   (concat (substring hint 0 20) "…")
                                 hint)))
                  (insert (propertize (format "  — %s" display)
                                      'face 'font-lock-comment-face))))

              ;; Store open target
              (put-text-property beg (point)
                                 'nejla-share-target
                                 (if sole
                                     (expand-file-name sole full-dir)
                                   full-dir))
              (when (plist-get info :notes)
                (put-text-property beg (point)
                                   'nejla-share-notes
                                   (plist-get info :notes))))
          ;; Not downloaded
          (insert (propertize (format "  %11s  %s  " "" date) 'face meta-face)
                  (propertize "Download" 'face 'dired-mark)))
        (insert "\n")))
    (put-text-property beg (1- (point)) 'nejla-share-file gpg-file)))

(defvar-local nejla-share--show-hidden nil
  "When non-nil, show hidden entries with indicator.")

(defun nejla-share--render ()
  "Render the share list buffer."
  (let* ((remotes (condition-case nil
                      (nejla-share--remote-list)
                    (error nil)))
         (locals (nejla-share--local-dirs))
         (remote-bases (mapcar #'file-name-sans-extension remotes))
         (orphans (cl-set-difference locals remote-bases :test #'string=))
         (inhibit-read-only t)
         (hidden-count 0)
         ;; First pass: classify all entries and collect visible ones
         (remote-entries
          (mapcar (lambda (gpg)
                    (let* ((base (file-name-sans-extension gpg))
                           (downloaded (member base locals))
                           (info (when downloaded (nejla-share--dir-info base)))
                           (hidden (nejla-share--entry-hidden-p base info)))
                      (list gpg base info hidden)))
                  remotes))
         (orphan-entries
          (mapcar (lambda (dir)
                    (let* ((info (nejla-share--dir-info dir))
                           (hidden (nejla-share--entry-hidden-p dir info)))
                      (list dir info hidden)))
                  (sort (copy-sequence orphans) #'string<)))
         ;; Compute name-col only from entries that will be rendered
         (name-col 0))

    ;; Compute max name width from visible entries only
    (dolist (entry remote-entries)
      (let ((hidden (nth 3 entry)))
        (when (or (not hidden) nejla-share--show-hidden)
          (setq name-col
                (max name-col
                     (length (nejla-share--entry-display-name (nth 1 entry))))))))
    (dolist (entry orphan-entries)
      (let ((hidden (nth 2 entry)))
        (when (or (not hidden) nejla-share--show-hidden)
          (setq name-col
                (max name-col
                     (length (nejla-share--entry-display-name (nth 0 entry))))))))

    (let ((name-fmt (format "%%-%ds" (+ name-col 2))))
      (erase-buffer)
      (insert (propertize "nejla_share\n" 'face 'dired-header))
      (insert (propertize (format "Directory: %s\n"
                                  (abbreviate-file-name nejla-share-directory))
                          'face 'shadow))
      ;; Placeholder for filter status — updated at the end
      (insert (propertize
               (if nejla-share--show-hidden
                   "Showing all entries (H to hide)"
                 "Hiding old/done/hidden entries (H to show)")
               'face 'shadow))
      (insert "\n\n")

      ;; Remote entries
      (dolist (entry remote-entries)
        (let ((gpg    (nth 0 entry))
              (base   (nth 1 entry))
              (info   (nth 2 entry))
              (hidden (nth 3 entry)))
          (cond
           ((not hidden)
            (nejla-share--insert-entry gpg base name-fmt info nil))
           (nejla-share--show-hidden
            (cl-incf hidden-count)
            (nejla-share--insert-entry gpg base name-fmt info t))
           (t
            (cl-incf hidden-count)))))

      ;; Orphan entries
      (let ((visible-orphans nil))
        (dolist (entry orphan-entries)
          (let ((dir    (nth 0 entry))
                (info   (nth 1 entry))
                (hidden (nth 2 entry)))
            (cond
             ((not hidden)
              (push (list dir info nil) visible-orphans))
             (nejla-share--show-hidden
              (cl-incf hidden-count)
              (push (list dir info t) visible-orphans))
             (t
              (cl-incf hidden-count)))))
        (when visible-orphans
          (insert "\n" (propertize "Local only (no remote):\n"
                                   'face 'dired-header))
          (dolist (entry (nreverse visible-orphans))
            (let ((dir    (nth 0 entry))
                  (info   (nth 1 entry))
                  (hidden (nth 2 entry)))
              (nejla-share--insert-entry
               (concat dir ".gpg") dir name-fmt info hidden)))))

      ;; Append hidden count to status line
      (when (> hidden-count 0)
        (save-excursion
          (goto-char (point-min))
          (forward-line 2)
          (end-of-line)
          (insert (propertize (format " [%d hidden]" hidden-count)
                              'face 'shadow)))))
    (goto-char (point-min))
    (forward-line 4)))

;; --- Interaction ---

(defun nejla-share-list-open ()
  "Open the share on the current line.
If downloaded and a sole file exists, open that file directly."
  (interactive)
  (let ((file (get-text-property (point) 'nejla-share-file))
        (target (get-text-property (point) 'nejla-share-target)))
    (cond
     (target
      (if (file-directory-p target)
          (dired target)
        (find-file target)))
     (file (nejla-share-open file))
     (t (user-error "No share on this line")))))

(defun nejla-share-list-notes ()
  "Open or create the .notes file for the share on the current line."
  (interactive)
  (let* ((file (get-text-property (point) 'nejla-share-file))
         (notes (get-text-property (point) 'nejla-share-notes)))
    (cond
     (notes (find-file notes))
     (file
      (let* ((base (file-name-sans-extension file))
             (dir (expand-file-name base nejla-share-directory)))
        (if (file-directory-p dir)
            (find-file (expand-file-name ".notes" dir))
          (user-error "Share not downloaded yet"))))
     (t (user-error "No share on this line")))))

(defun nejla-share--toggle-dot-file (dot-name label)
  "Toggle a dot-file DOT-NAME for the share on the current line.
LABEL is used in messages (e.g. \"done\", \"important\")."
  (let* ((file (get-text-property (point) 'nejla-share-file))
         (base (and file (file-name-sans-extension file)))
         (dir (and base (expand-file-name base nejla-share-directory))))
    (cond
     ((null file)
      (user-error "No share on this line"))
     ((not (file-directory-p dir))
      (user-error "Not downloaded yet"))
     (t
      (let ((path (expand-file-name dot-name dir))
            (line (line-number-at-pos)))
        (if (file-exists-p path)
            (progn (delete-file path)
                   (message "Unmarked %s." label))
          (with-temp-file path
            (insert (format-time-string "%Y-%m-%dT%H:%M:%S%z\n")))
          (message "Marked %s." label))
        (nejla-share-list-refresh)
        (goto-char (point-min))
        (forward-line (1- line)))))))

(defun nejla-share-list-done ()
  "Toggle done state for the share on the current line."
  (interactive)
  (nejla-share--toggle-dot-file ".done" "done"))

(defun nejla-share-list-important ()
  "Toggle important state for the share on the current line."
  (interactive)
  (nejla-share--toggle-dot-file ".important" "important"))

(defun nejla-share-list-hidden ()
  "Toggle hidden state for the share on the current line."
  (interactive)
  (nejla-share--toggle-dot-file ".hidden" "hidden"))

(defun nejla-share-list-toggle-show-hidden ()
  "Toggle display of hidden entries."
  (interactive)
  (setq nejla-share--show-hidden (not nejla-share--show-hidden))
  (nejla-share-list-refresh)
  (message (if nejla-share--show-hidden
               "Showing hidden entries."
             "Hiding filtered entries.")))

(defun nejla-share-list-refresh ()
  "Refresh the share list."
  (interactive)
  (nejla-share--render)
  (message "Refreshed."))

(defun nejla-share-list-delete ()
  "Delete the local directory for the share on the current line."
  (interactive)
  (let* ((file (get-text-property (point) 'nejla-share-file))
         (base (and file (file-name-sans-extension file)))
         (dir (and base (expand-file-name base nejla-share-directory))))
    (cond
     ((null file)
      (user-error "No share on this line"))
     ((not (file-directory-p dir))
      (user-error "Not downloaded — nothing to delete"))
     ((yes-or-no-p (format "Delete %s? " base))
      (delete-directory dir t t)
      (message "Deleted %s" base)
      (nejla-share-list-refresh)))))

;; --- Major mode ---

(defvar nejla-share-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "RET") #'nejla-share-list-open)
    (define-key map (kbd "n")   #'nejla-share-list-notes)
    (define-key map (kbd "g")   #'nejla-share-list-refresh)
    (define-key map (kbd "d")   #'nejla-share-list-delete)
    (define-key map (kbd "x")   #'nejla-share-list-done)
    (define-key map (kbd "i")   #'nejla-share-list-important)
    (define-key map (kbd "h")   #'nejla-share-list-hidden)
    (define-key map (kbd "H")   #'nejla-share-list-toggle-show-hidden)
    map)
  "Keymap for `nejla-share-list-mode'.")

(define-derived-mode nejla-share-list-mode special-mode "NejlaShare"
  "Major mode for browsing nejla_share files.
\\{nejla-share-list-mode-map}"
  (setq truncate-lines t))

(defun nejla-share-list ()
  "Show the nejla_share browser."
  (interactive)
  (let ((buf (get-buffer-create "*nejla-shares*")))
    (with-current-buffer buf
      (nejla-share-list-mode)
      (nejla-share--render))
    (pop-to-buffer buf)))

(provide 'nejla-share)
