;;;
;;; Alexey Khudyakov
;;; Programming enhancement
;;;

(defun my-insert-guard(title)
  "Insert C/C++ header guards quickly"
  (interactive "sType guard name ")
  (insert "#ifndef " title "\n#define " title "\n\n#endif /* " title " */\n" ))


;; Hooks
(defun my-python-hooks ()
  (setq tab-width 4)
  )

(add-hook 'python-mode 'my-python-hooks)


(provide 'my-programming)