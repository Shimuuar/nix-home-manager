;;;
;;; Alexey Khudyakov
;;; Programming enhancement
;;;

(defun my-insert-guard(title)
  "Insert C/C++ header guards quickly"
  (interactive "sType guard name ")
  (insert "#ifndef " title "\n#define " title "\n\n#endif /* " title " */\n" ))


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
;; Hooks
(defun my-python-hooks ()
  (setq tab-width 4)
  )

;; =========================================================
;; Set hooks 
(add-hook 'python-mode 'my-python-hooks)


(provide 'my-programming)