;;; -*- lexical-binding: t; -*-
(defun my/auth-source--write (host user port password)
  "Helper: write a new entry and call its :save-function."
  (let* ((auth-source-save-behavior t)  ; we've already confirmed; skip y/n
         (entry (car (auth-source-search
                      :host host
                      :user user
                      :port port
                      :secret password
                      :create t)))
         (save-fn (and entry (plist-get entry :save-function))))
    (when (functionp save-fn)
      (funcall save-fn)
      t)))

(defun my/auth-source-store (host user port password)
  "Store credentials in auth-source.
If an entry for HOST/USER/PORT exists, show it and offer to replace."
  (interactive
   (list (read-string "Host: ")
         (read-string "User: " "root")
         (read-string "Port or method (e.g. ssh, https): " "sudo")
         (read-passwd "Password: ")))
  (unwind-protect
      (let ((existing (car (auth-source-search
                            :host host :user user :port port :max 1))))
        (cond
         ;; No existing entry — just save.
         ((null existing)
          (if (my/auth-source--write host user port password)
              (message "Saved credentials for %s@%s (%s)" user host port)
            (message "Could not save entry (backend may be read-only).")))
         ;; Existing entry — confirm replacement.
         ((yes-or-no-p
           (format "Existing entry: host=%s user=%s port=%s. Delete and replace? "
                   (plist-get existing :host)
                   (plist-get existing :user)
                   (plist-get existing :port)))
          ;; Remove from file, then drop the in-memory cache.
          (auth-source-search :host host :user user :port port :delete t)
          (auth-source-forget+ :host host :user user :port port)
          (if (my/auth-source--write host user port password)
              (message "Replaced credentials for %s@%s (%s)" user host port)
            (message "Deleted old entry, but failed to write new one.")))
         (t
          (message "Cancelled — existing entry kept."))))
    ;; Always wipe the password from memory, even on error or quit.
    (clear-string password)))
