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
