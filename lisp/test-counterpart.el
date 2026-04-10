;;; -*- lexical-binding: t; -*-

(require 'project)

(defcustom test-counterpart-haskell-test-module-format "%sSpec"
  "Format for test module base names. %s is the source module base name.
E.g. \"%sSpec\" turns Foo into FooSpec, \"Test%s\" turns Foo into TestFoo."
  :type 'string
  :group 'haskell)

(defcustom test-counterpart-haskell-test-dir "test"
  "Test directory relative to project root."
  :type 'string
  :group 'haskell)

(defcustom test-counterpart-haskell-src-dir "src"
  "Source directory relative to project root."
  :type 'string
  :group 'haskell)

(defun test-counterpart-haskell-test-name (source-base)
  "Apply format to get test file base name."
  (format test-counterpart-haskell-test-module-format source-base))

(defun test-counterpart-haskell-source-name (test-base)
  "Reverse the format to recover source file base name."
  (let* ((parts (split-string test-counterpart-haskell-test-module-format "%s" t))
         (re (concat "\\`"
                     (mapconcat #'regexp-quote parts "\\(.*\\)")
                     "\\'")))
    (if (string-match re test-base)
        (match-string 1 test-base)
      test-base)))

(defun test-counterpart-haskell-extract-imports (file)
  "Grab all import lines from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let (lines)
      (while (re-search-forward "^import .*$" nil t)
        (push (match-string 0) lines))
      (nreverse lines))))

(defun test-counterpart-haskell-module-name (file)
  "Derive a module name from FILE relative to project test/src dir."
  (let* ((root (project-root (project-current t)))
         (rel (file-relative-name file root))
         ;; strip leading src/ or test/ or whatever
         (trimmed (replace-regexp-in-string
                   "^[^/]+/" "" rel)))
    (replace-regexp-in-string
     "/" "."
     (replace-regexp-in-string "\\.hs\\'" "" trimmed))))

(defun test-counterpart-haskell-scaffold-test (test-file source-module)
  "Generate test scaffold for TEST-FILE that tests SOURCE-MODULE."
  (let* ((dir (file-name-directory test-file))
         (sibling (car (directory-files dir t "\\.hs\\'" t)))
         (imports (if sibling
                      (test-counterpart-haskell-extract-imports sibling)
                    '("import Test.Hspec")))
         (test-module (test-counterpart-haskell-module-name test-file)))
    (concat "module " test-module " (spec) where\n\n"
            (string-join imports "\n") "\n"
            "import " source-module "\n\n"
            "spec :: Spec\n"
            "spec = describe \"" source-module "\" $ do\n"
            "  it \"works\" pending\n")))

(defun test-counterpart-haskell-toggle-test ()
  (interactive)
  (let* ((file (buffer-file-name))
         (root (project-root (project-current t)))
         (test-prefix (concat root test-counterpart-haskell-test-dir "/"))
         (src-prefix (concat root test-counterpart-haskell-src-dir "/"))
         (is-test (string-prefix-p test-prefix file))
         (base (file-name-base file))
         (target
          (if is-test
              (let ((src-base (test-counterpart-haskell-source-name base)))
                (replace-regexp-in-string
                 (regexp-quote test-prefix) src-prefix
                 (replace-regexp-in-string
                  (regexp-quote base) src-base file t t)))
            (let ((test-base (test-counterpart-haskell-test-name base)))
              (replace-regexp-in-string
               (regexp-quote src-prefix) test-prefix
               (replace-regexp-in-string
                (regexp-quote base) test-base file t t)))))
         (source-module (unless is-test
                          (test-counterpart-haskell-module-name file))))
    (if (file-exists-p target)
        (find-file target)
      (when (y-or-n-p (format "Create %s?" (file-name-nondirectory target)))
        (make-directory (file-name-directory target) t)
        (find-file target)
        (when (and (= (buffer-size) 0) source-module)
          (insert (test-counterpart-haskell-scaffold-test target source-module)))))))


(provide 'test-counterpart)
