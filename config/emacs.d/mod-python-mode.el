;;; Alexey Khudyakov
;;;
;;; Tweaks for python-mode


;; =========================================================
;; Python abbreviations
;; =========================================================

; Use python-mode from MELPA. Builtin doesn't have support for match
(require 'python-mode () t)

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
    ; Return
    ("ret"   "return" nil 0)
    ))


;; Delete traling whitespace on save
(add-hook 'python-mode-hook
	  (lambda () (add-hook 'before-save-hook
			       (lambda () (save-excursion (delete-trailing-whitespace))) nil t)))

(provide 'mod-python-mode)
