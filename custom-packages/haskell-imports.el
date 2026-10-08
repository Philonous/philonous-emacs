(defstruct haskell-import-line
  (qualified nil :type boolean)
  (package nil :type string)
  (module  nil :type string)
  (as  nil :type string)
  (idents  nil :type '(list string))
  (hiding  nil :type '(list string)))

(defvar haskell-import-ident-regexp
  "\\(?:\\(?:[a-zA-Z][^ ,()]*\\)\\|\\(?:([^()]+)\\)\\)")

(defvar haskell-import-ident-group-regexp
  (format "\\(?:[ ]*([ ]*\\(\\(?:%s\\(?:[ ]*,[ ]*%s\\)*\\)\\)[ ]*)\\)"
          haskell-import-ident haskell-import-ident-regexp))

(defvar haskell-import-regexp
  (concat "^\\(?:import[ ]+\\)"
          "\\(?:\\(qualified\\) \\)?"
          "[ ]*\\(?:\"\\([^\"]*\\)\" \\)?"
          "[ ]*\\([A-Za-z0-9_.']+\\)"
          "[ ]*\\(?:[ ]*as \\([A-Z][^ ]*\\)\\)?"
          (format "%s?" haskell-import-ident-group-regexp)
          (format "\\(?:[ ]*hiding %s\\)?" haskell-import-ident-group-regexp)
          "[ ]*\\(?:--\\(.*\\)\\)?[ ]*$")
  "Regex used for matching components of an import.")

(defun haskell-import--split-idents (ids-string)
  (when ids-string
    (save-match-data
      (cl-sort (split-string ids-string "[ ]*,[ ]*") 'string< ))))

(defun haskell-import-parse (line &optional raw-p)
  (let* ((work-line (if raw-p (concat "import " line) line))
         (matched (string-match haskell-import-regexp work-line)))
    (and matched (make-haskell-import-line
                  :qualified (string= "qualified" (match-string 1 work-line))
                  :package  (match-string 2 work-line)
                  :module (match-string 3 work-line)
                  :as (match-string 4 work-line)
                  :idents (haskell-import--split-idents
                           (match-string 5 work-line))

                  :hiding (haskell-import--split-idents
                           (match-string 6 work-line))))))

(defmacro with-import-line (line suff body)
  "Destructure an import line struct to local variables called
qualified, package, module, as, idents and hiding"
  (let* ((parts (list "qualified" "package" "module" "as" "idents" "hiding"))
         (names (mapcar (lambda (p) (intern (concat p suff))) parts))
         (tmp (make-symbol "import")))

    `(let* ((,tmp ,line)
            ,@(mapcar* (lambda (part name)
                        (list name
                              (list (intern (concat "haskell-import-line-" part ))
                                    tmp)))
                       parts names))
       ,body)))

(defun haskell-import-print (line)
  (with-import-line line ""
    (concat "import "
            (when qualified "qualified ")
            (when package (concat "\"" package "\" "))
            module
            (when as (concat " as " as))
            (when idents (concat " ("
                                 (mapconcat 'identity idents ", ")
                                 ")"))
            (when hiding (concat " hiding ("
                                 (mapconcat 'identity hiding ", ")
                                 ")"))
            )))

(defmacro haskell-import--on (op acc x1 x2)
  `(,op (,acc ,x1) (,acc ,x2)))

(defmacro haskell-import--union-fields (field line1 line2)
  `(setf (,field ,line1)
         (cl-sort
          (union (,field ,line1) (,field ,line2 ) :test (quote string=))
          (quote string<))))

(defmacro haskell-import--intersection-fields (field line1 line2)
  `(setf (,field ,line1)
         (cl-sort
          (intersection (,field ,line1) (,field ,line2 ) :test (quote string=))
          (quote string<))))

(defun haskell-import-mergeable (line1 line2)
  (and (haskell-import--on eq haskell-import-line-qualified line1 line2)
       (haskell-import--on string= haskell-import-line-package line1 line2)
       (haskell-import--on string= haskell-import-line-module line1 line2)
       (haskell-import--on string= haskell-import-line-as line1 line2)
       (not (and (haskell-import-line-idents line1)
                 (haskell-import-line-hiding line2)))
       (not (and (haskell-import-line-hiding line1)
                 (haskell-import-line-idents line2)))))

(defun haskell-import-merge-idents (line1 line2)
  "Merge imported and hidden identifiers of inport lines. The
qualified declaration, module name and as-name have to match or
nil is returned"
  (when (haskell-imports-mergeable line1 line2)
    (let ((new-line (copy-haskell-import-line line1)))
      (haskell-import--union-fields haskell-import-line-idents new-line line2)
      (haskell-import--intersection-fields haskell-import-line-hiding
                                           new-line line2)
      new-line)))

(defmacro haskell-import-with-line (line ident &rest body)
  `(let ((,ident (haskell-import-parse ,line)))
     (when ,ident
       ,body
       (haskell-import-print ,ident))))

(defmacro haskell-import-with-current-line (ident &rest body)
  (let ((line-str (make-symbol "line"))
        (new-line (make-symbol "new-line")))
    `(let* ((,line-str (buffer-substring-no-properties (line-beginning-position)
                                                       (line-end-position)))
            (,new-line (haskell-import-with-line ,line-str ,ident ,body)))
      (when ,new-line
        (delete-region (line-beginning-position) (line-end-position))
        (insert ,new-line)))))

(defun abbreviate-module (module-name)
  (let ((parts (split-string module-name "[.]")))
  (mapconcat (lambda (str) (substring str 0 1)) parts "")))

(defun haskell-import-toggle-as ()
  (interactive)
  (haskell-import-with-current-line line
   (if (haskell-import-line-as line)
      (setf (haskell-import-line-as line) nil)
     (setf (haskell-import-line-as line)
           (abbreviate-module (haskell-import-line-module line))))))
