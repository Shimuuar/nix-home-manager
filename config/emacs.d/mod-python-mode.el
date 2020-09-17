;;; Alexey Khudyakov
;;;
;;; Tweaks for python-mode


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



(provide 'mod-python-mode)

