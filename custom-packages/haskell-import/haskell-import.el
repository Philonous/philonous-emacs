(defcustom haskell-import-module-abbrevs
  nil
  "List of module abbreviations"
  :group 'haskell
  :type '(cons string (cons string (plist symbol string)))
  )

(setq haskell-import-module-abbrevs '(("Text" "Data.Text (Text)"
                               "qualified Data.Text as Text"
                               :language "OverloadedStrings")
                       ("Encoding" "qualified Data.Text.Encoding as Text")
                       ("Aeson" "qualified Data.Aeson as Aeson")
                       ("Map"  "qualified Data.Map.Strict as Map"
                               "Data.Map.Strict (Map)")
                       ("HMap" "qualified Data.HashMap.Strict as HMap"
                               "Data.HashMap.Strict (HashMap)")
                       ("Set" "Data.Set (Set)"
                              "qualified Data.Set as Set")
                       ("App"  "Control.Applicative")
                       ("Monad" "Control.Monad")
                       ("BS"   "qualified Data.ByteString as BS"
                               "Data.ByteString (ByteString)")
                       ("BS8"  "qualified Data.ByteString.Char8 as BS8"
                               :language "OverloadedStrings")
                       ("BSL"  "qualified Data.ByteString.Lazy as BSL")
                       ("BSL8"  "qualified Data.ByteString.Lazy.Char8 as BSL")
                       ("Concurrent" "Control.Concurrent")
                       ("Async" "qualified Control.Concurrent.Async as Async")
                       ("STM" "Control.Concurrent.STM")
                       ("Xmpp" "qualified Network.Xmpp as Xmpp")
                       ("XML" "qualified Data.XML.Types as XML"
                              "Data.XML.Pickle"
                              :language "OverloadedStrings")
                       ("Ex" "qualified Control.Monad.Catch as Ex")
                       ("Monoid" "Data.Monoid")
                       ("Function" "Data.Function")
                       ("Foldable" "qualified Data.Foldable as Foldable")
                       ("Traversable" "qualified Data.Traversable as Traversable")
                       ("Ord" "Data.Ord")
                       ("Char" "Data.Char")
                       ("List" "qualified Data.List as List")
                       ("Lens" "Control.Lens")
                       ("Logger" "Control.Monad.Logger")
                       ("Trans" "Control.Monad.Trans")
                       ("liftIO" "Control.Monad.Trans")
                       ("Reader" "Control.Monad.Reader")
                       ("State" "Control.Monad.State")
                       ("Writer" "Control.Monad.Writer")
                       ("STM" "Control.Concurrent.STM")
                       ("IORef" "Data.IORef")
                       ("UUID" "Data.UUID (UUID)"
                               "qualified Data.UUID as UUID" )
                       ("Word" "Data.Word")
                       ("Int" "Data.Int")
                       ("UTCTime" "Data.Time.Clock (UTCTime)"
                                  "qualified Data.Time.Clock as Time")
                       ("Typeable" "Data.Typeable")
                       ("Data" "Data.Data")
                       ("Typeable" "Data.Typeable")
                       ("Base64" "qualified Data.ByteString.Base64 as Base64")
                       ("Warp" "qualified Network.Wai.Handler.Warp as Warp")
                       ("DataTypeable"
                          "Data.Typeable" "Data.Data"
                          :language "DeriveDataTypeable")
                       ("Vector" "Data.Vector (Vector)" "qualified Data.Vector as Vector")
                       ("Free" "Control.Monad.Trans.Free")
                       ("TH" "Language.Haskell.TH"
                              :language "TemplateHaskell")
                       ("QQ" "Language.Haskell.TH"
                             "Language.Haskell.TH.Quote"
                              :language "TemplateHaskell")
                       ("fromJust" "Data.Maybe (fromJust)")
                       ("th" :language "TemplateHaskell")
                       ("nmr" :language "NoMonomorphismRestriction")
                       ("ols" :language "OverloadedStrings")
                       ("qq" :language "QuasiQuotes")
                       ("gadts" :language "GADTs")
                       ("lcase" :language "LambdaCase")
                       ("gnt" :language "GeneralizedNewtypeDeriving")
                       ("i" "Data.String.Interpolate.IsString (i)" :language "QuasiQuotes")
                       ))

(defun haskell-exists-import-line (line)
  (save-excursion
    (beginning-of-buffer)
    (not (not (re-search-forward (concat "^" "import[ ]*" line "$")
                                 (point-max) t)))))

(defun haskell-import-toggle-qualified ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (cond ((looking-at "[ ]*qualified[ ]+") (replace-match ""))
          ((looking-at "[ ]*") (replace-match "qualified ")))))

(defun haskell-import-read-import-lines ()
  "Read modules to import from the minibuffer with `haskell-import-keymap' "

  (read-from-minibuffer "Module(s): " "" haskell-import-keymap))

(defun haskell-import-check-expansion-and-return ()
  "Check whether minibuffer contains a known abbreviation and return"
  (interactive)
  (let* ((input (minibuffer-contents-no-properties))
         (content
          (progn (string-match "\\(?:[ ]*qualified[ ]\\)?[ ]*\\([^ ]*\\)[ ]*"
                               input)
                 (match-string 1 input)))
         (expansion (assoc content haskell-import-module-abbrevs)))
    (if expansion
        (exit-minibuffer)
        (beep))))

(defvar haskell-import-keymap (copy-keymap minibuffer-local-map))
(define-key haskell-import-keymap (kbd "C-q")
  'haskell-import-toggle-qualified)
(define-key haskell-import-keymap (kbd "C-c C-q")
  'haskell-import-toggle-qualified)
(define-key haskell-import-keymap (kbd "C-<return>")
  'haskell-import-check-expansion-and-return)

(defun haskell-import-lines (import-line)
  "Add an import line to the current buffer.

   import-line can be an abbreviation defined in the
   haskell-import-module-abbrevs variable. If this is the case, all the elements
   in the matching entry are inserted (no further expansion
   is performed)

   If no abbreviation entry matches, the input it is inserted verbatim

   Prefixing the input with an exclamation mark (!) forces the
   rest of the input to be treated as an abbreviation. An error
   is thrown when the given string doesn't match any entry
  "
  (interactive (list (haskell-import-read-import-lines)))
  (require 'haskell-align-imports)
  (require 'haskell-sort-imports)
  (save-excursion
    (let* ((force-abbrev (string= (substring import-line 0 1) "!"))
           (import (if force-abbrev (substring import-line 1) import-line))
           (args (or (cdr (assoc import haskell-import-module-abbrevs))
                      (when force-abbrev
                        (error "Module abbreviation \"%s\" not found" import))
                      (list import)))
           (imports (loop for decl in args while (stringp decl) collect decl))
           (rest (loop for tail on args while (stringp (car tail))
                       finally (return tail)))
           (extensions-val (plist-get rest :language))
           (extensions (cond
                        ((stringp extensions-val) (list extensions-val) )
                        ((sequencep extensions-val) extensions-val)
                        (t (error "extension must be string or sequence")))))
      (mapc (lambda (decl)
              (unless (haskell-exists-import-line decl)
                (beginning-of-buffer)
                (haskell-navigate-imports)
                (insert (concat "import " decl "\n"))))
            imports)
      (haskell-sort-imports)
      (haskell-align-imports)
      (mapc 'haskell-insert-language-pragma-if-missing extensions))))

(defun haskell-qualify-import-line (&optional no-toggle)
  "Toggle \"qualified\" modifier of current import line"
  (interactive "P")
  (require 'haskell-align-imports)
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "import qualified\\([ ]?\\)[ ]*")
      (unless no-toggle
        (replace-match "import\\1")
        (haskell-align-imports)))
     ((looking-at "import\\([ ]?\\)[ ]*")
      (replace-match "import qualified\\1")
      (haskell-align-imports))
)))

(defun haskell-language-pragma-regexp (feature)
  (format "^[ ]*{-#[ ]*LANGUAGE [[:alnum:] ,\n]*\\(%s\\)[^#-}]*#-}"
          feature))

(defun haskell-find-language-pragma (feature)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (haskell-language-pragma-regexp feature) nil t)
      (match-beginning 1))))

(defun haskell-insert-language-pragma-if-missing (feature)
  (save-excursion
    (unless (haskell-find-language-pragma feature)
      (goto-char (point-min))
      (insert (format  "{-# LANGUAGE %s #-}\n" feature)))))

(provide 'haskell-import)
