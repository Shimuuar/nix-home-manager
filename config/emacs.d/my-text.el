;;;
;;; Alexey Khudyakov
;;; Text editing customization
;;;


(defun my/switch-dict ()
  "Swtich between dictionaries"
  (interactive)
  (ispell-change-dictionary (if (string= "ru" ispell-dictionary)
                                "en" "ru")))


(defun my/prepare-for-telegram(str)
  "Reformat text for inserion to telegram/any other messenger"
  (let* ((trim   (lambda (s) (replace-regexp-in-string  "\n" " "
  			     (replace-regexp-in-string "^[ \\t]+" "" s))))
	 (para    (split-string str "\n\\([ \t]*\n\\)+"))
	 (trimmed (mapcar trim para))
	 )
    (mapconcat 'identity trimmed "\n\n")
    ))

(defun my/to-telegram(start end)
  "Reformat text and insert it into kill ring"
  (interactive "r")
  (if (use-region-p)
      (kill-new
       (my/prepare-for-telegram
	(buffer-substring-no-properties start end))))
  )



;; ================================================================
;; Global settings
;; ================================================================

; orphography check
(setq         ispell-dictionary   "en"    )
(setq-default ispell-program-name "aspell")



;; =========================================================
;; Hooks
;; =========================================================
; Basic text hook

(add-hook 'text-mode-hook  'my/hook/base-text)

(provide 'my-text)
