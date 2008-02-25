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
  "Jump to matching parenthesis"
  (interactive "p")
  (cond ((looking-at "\(")   (forward-list  1))
        ((looking-back "\)") (backward-list 1))))
;; =================


;; ===============================================
;; Syntax highlighting
;; ===============================================
; turn on syntax highlighting
(global-font-lock-mode t)
; Highlight parensthesis
(show-paren-mode t) 
; Highlight whole expression if it's not fully visible
(setq show-paren-style 'mixed) 
;; =================


;; ===============================================
;; Indentation
;; ===============================================
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "bsd" c-basic-offset 4)
;; =================


;; ========================================================
;; Define hooks 
;; =========================================================
(defun my-ret-hook()
  "Make new lines indented"
  (local-set-key (kbd "RET") 'newline-and-indent)
  )

(defun my-python-hooks ()
  (setq tab-width 4) ; Override tab width
  (abbrev-mode t)    ; Set abberviation mode
  ; Insert shebang into empty files
  (if (= (- (point-min) (point-max)) 0)
      (insert "#!/usr/bin/python\n\"\"\"\n\"\"\""))
  )

; C hooks 
(add-hook 'c-mode-hook          'my-ret-hook)
; C++ hooks 
(add-hook 'c++-mode-hook        'my-ret-hook)
; Python hooks 
(add-hook 'python-mode-hook     'my-ret-hook)
(add-hook 'python-mode-hook     'my-python-hooks)
; Shell hooks 
(add-hook 'sh-mode-hook         'my-ret-hook)
; Lisp hooks 
(add-hook 'lisp-mode-hook       'my-ret-hook)
(add-hook 'emacs-lisp-mode-hook 'my-ret-hook)
;; =================

(provide 'my-programming)
