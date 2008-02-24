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

(defun my-programming-hooks() 
  "Hooks common for all programming languages"
  ; Make RET more indent friendlier
  (local-set-key (kbd "RET") 'newline-and-indent)
  )

(defun my-c-hooks () 
  (my-programming-hooks)
  )

(defun my-c++-hooks () 
  (my-programming-hooks)
  )

(defun my-python-hooks ()
  (my-programming-hooks)
  (setq tab-width 4)
  )

(defun my-bash-hooks ()
  (my-programming-hooks)
  )

(defun my-lisp-hooks () 
  (my-programming-hooks)
  )

;; =========================================================
;; Set hooks 
(add-hook 'c-mode      'my-c-hooks)
(add-hook 'c++-mode    'my-c++-hooks)
(add-hook 'python-mode 'my-python-hooks)
(add-hook 'sh-mode     'my-bash-hooks)
(add-hook 'lisp-mode   'my-lisp-hooks)


(provide 'my-programming)
