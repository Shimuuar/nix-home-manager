;;;
;;; Alexey Khudyakov
;;; Abbreviations
;;;

;; =================


;; =========================================================
;; Python abbreviations
;; =========================================================
(define-skeleton my/skel-python-import-list
  "``Import all'' clause"
  "Import from: "
  "from " str " import " _ "")

(define-skeleton my/skel-python-range
  "skeleton for python ``range''"
  "Range: "
  "range(" str ")" _ )

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ; import clause  
    ("imp"   "import" nil 0)
    ("impa"  ""       my/skel-python-import-list 0)
    ("ra"    ""       my/skel-python-range       0)
    ; Return
    ("ret"   "return" nil 0)
    ))
;; =================

;; =========================================================
;; Haskell abbreviations
;; =========================================================
(define-skeleton my/skel-haskell-LANG
  "Skeleton for haskell LANGUAGE pragma"
  ""
  "{-# LANGUAGE "
  (completing-read "Language extension: " my/haskell-language-pragmas)
  " #-}" 
  )
(define-skeleton my/skel-haskell-OPTGCH
  "Skeleton for haskell OPTIONS_GHC pragma"
  ""
  "{-# OPTIONS_GHC "
  (completing-read "Command line: " my/haskell-ghc-options)
  " #-}" 
  )
(define-skeleton my/skel-haskell-import
  "Skeleton for common haskell imports"
  ""
  "import "
  (completing-read "Import module: "
		   '("Control.Applicative"
		     "Control.Arrow"
		     "Control.Exception"
		     "Control.Monad"
		     "Control.Monad.Error"
		     "Control.Monad.Reader"
		     "Control.Monad.State"
		     "Control.Monad.Trans"
		     "Control.Monad.Writer"
		     "Data.Accesor"
		     "Data.Accesor.Template"
		     "Data.Default"
		     "Data.List"
		     "Data.Maybe"
		     "Data.Monoid"
		     "Language.Haskell.TH"
		     "Prelude"
		     "System"
		     "System.Directory"
		     "System.IO"
		     "System.Process"
		     "Text.Printf"
		     ; Modules which usually imported qualified
		     "qualified Data.Map as Map"
		     "qualified Data.IntMap as IntMap"
		     "qualified Data.Set as Set"
		     "qualified Data.ByteString"
		     "qualified Data.ByteString.Lazy"
		     )))


(define-abbrev-table 'haskell-mode-abbrev-table
  '(
    ; Import
    ("imp"  "import"           nil                    0)
    ("impq" "import qualified" nil                    0)
    ("impa" ""                 my/skel-haskell-import 0)
    ; Shorhands
    ("ret"  "return"           nil                    0)
    ("pr"   "print"            nil                    0)
    ("fi"   "fromIntegral"     nil                    0)
    ; Pragmas
    ("plang" ""                my/skel-haskell-LANG   0)
    ("opt_ghc" ""              my/skel-haskell-OPTGCH 0)
    ("ghc_opt" ""              my/skel-haskell-OPTGCH 0)
    ))

(provide 'my-abbrevs)
