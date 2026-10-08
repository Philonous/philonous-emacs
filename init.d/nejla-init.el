;;; -*- lexical-binding: t; -*-
;;; Code for nejla projects

(require 'sql)

(defconst ukaa-services
  '( "alvsbyn"
     "are"
     "borlange"
     "halmstad"
     "harnosand"
     "falkenberg"
     "jonkoping2"
     "koping"
     "laholm"
     "linkoping"
     "lulea"
     "norrkoping"
     "nykoping"
     "orebro"
     "ornskoldsvik"
     "ostersund"
     "sundsvall"
     "test"
     "tibro"
     "tranas"
     "uppsala"
     "vasteras"
     "eskilstuna"
     ))


(defun ukaa-backend-database (service)
  "Connect to a ukaa database SERVICE via Docker."
  (interactive
   (list (completing-read "Choose service: "
                          ukaa-services nil nil nil)))
  (let* ((sql-user "postgres")
         (sql-database "postgres")
         (sql-server "localhost")
         (buf-name (format "*SQL: ukaa-%s*" service))
         (default-directory
          (format "/ssh:ukaa.se|sudo:root@ukaa.se|docker:ukaa-database-%s-1:/" service)))
    (if (get-buffer buf-name)
        (pop-to-buffer buf-name)
      (sql-postgres)
      (rename-buffer buf-name))))


(defvar-local ukaa-shell-buffer nil
  "The comint shell buffer this buffer sends to.")

;;; Plumbing ---------------------------------------------------------------

(defun ukaa-shell--buffer-name (service)
  (format "*shell: ukaa-%s*" service))

(defun ukaa-shell--start (service)
  "Start a shell for SERVICE and return its buffer."
  (let* ((tramp-prefix
          (format "/ssh:ukaa.se|sudo:root@ukaa.se|docker:ukaa-backend-%s-1:" service))
         (default-directory (concat tramp-prefix "/"))
         (buf-name (ukaa-shell--buffer-name service))
         (explicit-shell-file-name
          (seq-find (lambda (sh)
                      (file-exists-p (concat tramp-prefix sh)))
                    '("/bin/zsh" "/bin/bash" "/bin/sh")
                    "/bin/sh")))
    (save-window-excursion (shell buf-name))
    (get-buffer buf-name)))

(defun ukaa-shell--ensure (service)
  "Return a live shell buffer for SERVICE, starting one if needed."
  (let ((buf (get-buffer (ukaa-shell--buffer-name service))))
    (unless (and buf (get-buffer-process buf))
      (setq buf (ukaa-shell--start service)))
    (with-current-buffer buf
      (local-set-key (kbd "M-`") #'ukaa-shell-jump))
    buf))

(defun ukaa-shell--process ()
  (let ((buf (and (buffer-live-p ukaa-shell-buffer) ukaa-shell-buffer)))
    (or (and buf (get-buffer-process buf))
        (user-error "No shell connected; (re)enable ukaa-shell-mode"))))

(defun ukaa-shell--continued-p ()
  "Non-nil if the current line ends with a backslash continuation."
  (save-excursion (end-of-line) (eq (char-before) ?\\)))

;;; Sending ----------------------------------------------------------------

(defun ukaa-shell-send-region (start end)
  "Send region to the connected shell."
  (interactive "r")
  (let* ((txt (buffer-substring-no-properties start end)))
    (message (concat "Sending: " txt))
    (comint-send-string
     (ukaa-shell--process)
     (concat txt "\n"))))

(defun ukaa-shell-send-buffer ()
  "Send the whole buffer."
  (interactive)
  (ukaa-shell-send-region (point-min) (point-max)))

(defun ukaa-shell-send-command ()
  "Send the command at point: the current line, extended across any
lines joined to it by trailing backslash continuations."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (and (not (bobp))
                (save-excursion (forward-line -1) (ukaa-shell--continued-p)))
      (forward-line -1))
    (let ((start (point)))
      (end-of-line)
      (while (and (ukaa-shell--continued-p) (not (eobp)))
        (forward-line 1) (end-of-line))
      (ukaa-shell-send-region start (point)))))

;;; Jumping

(defvar-local ukaa-shell--last-source nil
  "In a shell buffer, the text buffer that last jumped here.")

(defun ukaa-shell-jump ()
  "Toggle between a text buffer and its shell.
From a text buffer, jump to the connected shell and record this
buffer as its jump source.  From the shell, jump back to whichever
text buffer last jumped in."
  (interactive)
  (cond
   ((and (buffer-live-p ukaa-shell-buffer))          ; we're in a text buffer
    (let ((shell ukaa-shell-buffer)
          (origin (current-buffer)))
      (with-current-buffer shell (setq ukaa-shell--last-source origin))
      (pop-to-buffer shell)))
   ((buffer-live-p ukaa-shell--last-source)          ; we're in the shell
    (pop-to-buffer ukaa-shell--last-source))
   (t (user-error "No jump target"))))

;;; Minor mode -------------------------------------------------------------

(define-minor-mode ukaa-shell-mode
  "Send shell commands from this buffer to a ukaa backend container."
  :lighter " ukaa"
  :keymap (let ((m (make-sparse-keymap)))
            (define-key m (kbd "C-c b") #'ukaa-shell-send-buffer)
            (define-key m (kbd "C-c r") #'ukaa-shell-send-region)
            (define-key m (kbd "C-c e") #'ukaa-shell-send-command)
            (define-key m (kbd "M-`") #'ukaa-shell-jump)
            m)
  (if ukaa-shell-mode
      (let* ((origin (current-buffer))
             (service (completing-read "Choose service: " ukaa-services nil nil nil))
             (buf (ukaa-shell--ensure service)))
        (with-current-buffer origin (setq ukaa-shell-buffer buf))
        (display-buffer buf))
    (setq ukaa-shell-buffer nil)))

(require 'nejla-share)

(provide 'nejla)
