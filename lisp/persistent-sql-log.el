;;; persistent-log-to-sql.el --- Convert Persistent SQL logs to executable SQL  -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Takes a SQL statement logged by Haskell's `persistent' library --
;; with `?' placeholders followed by a `[PersistFoo ..., PersistBar ...]'
;; list of bound values -- and produces an executable SQL string with
;; the values inlined as proper SQL literals.  Intended for pasting log
;; output into psql while debugging.
;;
;; Usage: select the region containing the query plus its value list
;; and run M-x persistent-log-to-sql.  Result appears in *persistent-sql*.

(require 'cl-lib)
(require 'sql-format)

(defun persistent-log-to-sql ()
  "Convert Haskell Persistent SQL log to executable SQL, in place.
Operates on the active region if any, otherwise the whole buffer."
  (interactive)
  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end)       (point-max)))
         (out (persistent-log-convert
               (buffer-substring-no-properties beg end))))
    (save-excursion
      (delete-region beg end)
      (goto-char beg)
      (insert out)
      (sql-format--region beg (point))
      )))

(defun persistent-log-convert (text)
  "TEXT in, executable SQL out."
  (pcase-let* ((`(,sql . ,vals-str) (persistent--split text))
               (flat (replace-regexp-in-string "[ \t\n\r]+" " "
                                               (string-trim sql)))
               (vals (and vals-str (persistent--parse-list vals-str))))
    (persistent--fill-placeholders flat vals)))

(defun persistent--split (text)
  "Return (SQL . VALUES-STR) by matching the trailing [..] list, if any."
  (if (string-match
       "\\`\\(\\(?:.\\|\n\\)*?\\)[ \t\n]*\\[\\(\\(?:.\\|\n\\)*\\)\\][ \t\n]*\\'"
       text)
      (cons (match-string 1 text) (match-string 2 text))
    (cons text nil)))

(defun persistent--parse-list (str)
  "Split STR on top-level commas, respecting (), [], {} and \"...\"."
  (let ((items '()) (cur "") (depth 0) (in-str nil) (escape nil))
    (dotimes (i (length str))
      (let ((c (aref str i)))
        (cond
         (escape (setq cur (concat cur (string c)) escape nil))
         ((and in-str (eq c ?\\))
          (setq cur (concat cur (string c)) escape t))
         ((eq c ?\")
          (setq in-str (not in-str) cur (concat cur (string c))))
         (in-str (setq cur (concat cur (string c))))
         ((memq c '(?\( ?\[ ?\{))
          (cl-incf depth) (setq cur (concat cur (string c))))
         ((memq c '(?\) ?\] ?\}))
          (cl-decf depth) (setq cur (concat cur (string c))))
         ((and (eq c ?,) (zerop depth))
          (push (string-trim cur) items) (setq cur ""))
         (t (setq cur (concat cur (string c)))))))
    (when (> (length (string-trim cur)) 0)
      (push (string-trim cur) items))
    (nreverse items)))

(defun persistent--sql-string (s)
  (format "'%s'" (replace-regexp-in-string "'" "''" s)))

(defun persistent--unescape (s)
  "Undo basic Haskell escapes in S."
  (let ((s (replace-regexp-in-string "\\\\\\\\" "\0BS\0" s)))
    (setq s (replace-regexp-in-string "\\\\\"" "\"" s))
    (setq s (replace-regexp-in-string "\\\\n" "\n" s))
    (setq s (replace-regexp-in-string "\\\\t" "\t" s))
    (setq s (replace-regexp-in-string "\\\\r" "\r" s))
    (replace-regexp-in-string "\0BS\0" "\\\\" s)))

(defun persistent--value-to-sql (val)
  "Render one PersistValue string VAL as a SQL literal."
  (cond
   ((string-match "\\`PersistNull\\'" val) "NULL")

   ((string-match "\\`PersistBool[ \t]+\\(True\\|False\\)\\'" val)
    (if (string= "True" (match-string 1 val)) "TRUE" "FALSE"))

   ((string-match "\\`PersistInt64[ \t]+\\(-?[0-9]+\\)\\'" val)
    (match-string 1 val))

   ((string-match "\\`PersistDouble[ \t]+\\(-?[0-9.eE+-]+\\)\\'" val)
    (match-string 1 val))

   ((string-match "\\`PersistRational[ \t]+\\(.+\\)\\'" val)
    (format "(%s)" (match-string 1 val)))

   ((string-match "\\`PersistDay[ \t]+\\(.+\\)\\'" val)
    (format "'%s'::date" (match-string 1 val)))

   ((string-match "\\`PersistUTCTime[ \t]+\\(.+\\)\\'" val)
    (format "'%s'::timestamptz" (match-string 1 val)))

   ((string-match "\\`PersistTimeOfDay[ \t]+\\(.+\\)\\'" val)
    (format "'%s'::time" (match-string 1 val)))

   ((string-match
     "\\`PersistText[ \t]+\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"\\'" val)
    (persistent--sql-string
     (persistent--unescape (match-string 1 val))))

   ((string-match
     "\\`PersistByteString[ \t]+\"\\(\\(?:[^\"\\\\]\\|\\\\.\\)*\\)\"\\'" val)
    (format "%s::bytea"
            (persistent--sql-string
             (persistent--unescape (match-string 1 val)))))

   ((string-match "\\`Persist\\(?:Array\\|List\\)[ \t]+\\[\\(.*\\)\\]\\'" val)
    (format "ARRAY[%s]"
            (mapconcat #'persistent--value-to-sql
                       (persistent--parse-list (match-string 1 val))
                       ", ")))

   ((string-match
     "\\`PersistLiteral_?[ \t]+\\S-+[ \t]+\"\\(.+\\)\"\\'" val)
    (persistent--sql-string (match-string 1 val)))

   (t (format "/*UNHANDLED:%s*/" val))))

(defun persistent--fill-placeholders (sql vals)
  "Replace ? in SQL with rendered VALS, ignoring ?s inside '...' literals."
  (let ((rendered (mapcar #'persistent--value-to-sql vals))
        (out "") (in-str nil) (escape nil))
    (dotimes (i (length sql))
      (let ((c (aref sql i)))
        (cond
         (escape (setq out (concat out (string c)) escape nil))
         ((and in-str (eq c ?\\))
          (setq out (concat out (string c)) escape t))
         ((eq c ?\')
          (setq in-str (not in-str) out (concat out (string c))))
         ((and (eq c ??) (not in-str))
          (if rendered
              (progn (setq out (concat out (car rendered)))
                     (setq rendered (cdr rendered)))
            (setq out (concat out "?"))))
         (t (setq out (concat out (string c)))))))
    out))

(provide 'persistent-sql-log)
;;; persistent-log-to-sql.el ends here
