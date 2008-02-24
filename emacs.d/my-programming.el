;;;
;;; Alexey Khudyakov
;;; Programming enhancement
;;;

;; =========================================================
;; Useful functions 
;; =========================================================
(defun my-insert-guard(title)
  "Insert C/C++ header guards quickly"
  (interactive "sType guard name ")
  (insert "#ifndef " title 
          "\n#define " title 
          "\n\n#endif /* " title " */\n" ))

(defun my-indent-buffer()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun my-indent-line()
  "indent line and move to next"
  (interactive)
  (indent-according-to-mode)
  (next-line))

(defun my-match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\(")   (forward-list  1))
        ((looking-back "\)") (backward-list 1))))
;;; =================

; turn on syna
(global-font-lock-mode t)
; Highlight parensthesis
(show-paren-mode t) 
; Highlight whole expression if it's not fully visible
(setq show-paren-style 'mixed) 

; Indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "bsd" c-basic-offset 4)

;; ========================================================
;; Define hooks 
;; =========================================================

(defun my-ret-hook() 
  (local-set-key (kbd "RET") 'newline-and-indent)
  )

(defun my-c-abbrevs() 
  "Abbreviation table for C/C++"
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
      )))

(define-skeleton my-skel-python-import-all
  "Import all clause"
  "Import from: "
  "from " str " import *" _ "")

(defun my-python-hooks ()
  (define-abbrev-table 'python-mode-abbrev-table
    '(
      ("imp"   "import" nil 0)
      ("impa"  ""       my-skel-python-import-all 0)
      ))
  (setq tab-width 4)
  (abbrev-mode t))

;; =========================================================
;; Set hooks 
;; =========================================================
; C hooks 
(add-hook 'c-mode-hook          'my-ret-hook)
(add-hook 'c-mode-hook          'my-c-abbrevs)
; C++ hooks 
(add-hook 'c++-mode-hook        'my-ret-hook)
(add-hook 'c++-mode-hook        'my-c-abbrevs)
; Python hooks 
(add-hook 'python-mode-hook     'my-python-hooks)
(add-hook 'python-mode-hook     'my-python-hooks)
; Shell hooks 
(add-hook 'sh-mode-hook         'my-ret-hook)
; Lisp hooks 
(add-hook 'lisp-mode-hook       'my-ret-hook)
(add-hook 'emacs-lisp-mode-hook 'my-ret-hook)
;; =================

(provide 'my-programming)
