;;;
;;; Alexey Khudyakov
;;; Abbreviations
;;;

;; =========================================================
;; C/C++ abbreviations
;; =========================================================
(define-abbrev-table 'c-mode-abbrev-table 
  '( 
    ; Control structures
    ("sw"   "switch"   nil 0)
    ("ca"   "case"     nil 0)
    ("br"   "break"    nil 0)
    ("wh"   "while"    nil 0)
    ("ret"  "return"   nil 0)
    ("cont" "continue" nil 0)
    ; Classes
    ("pub"  "public"   nil 0)
    ("priv" "private"  nil 0)
    ; Types and modifiers 
    ("co" "const"      nil 0)
    ("vo" "void"       nil 0)
    ("bo" "bool"       nil 0)
    ; Preprocessor directives 
    ("inc" "#include"  nil 0)
    ("def" "#define"   nil 0)
    ))
;; =================


;; =========================================================
;; Python abbreviations
;; =========================================================
(define-skeleton my-skel-python-import-all
  "``Import all'' clause"
  "Import from: "
  "from " str " import *" _ "")

(define-skeleton my-skel-python-range
  "skeleton for python ``range''"
  "Range: "
  "range(" str ")" _ )

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ; import clause  
    ("imp"   "import" nil 0)
    ("impa"  ""       my-skel-python-import-all 0)
    ("ra"    ""       my-skel-python-range      0)
    ))
;; =================

(provide 'my-abbervs)
