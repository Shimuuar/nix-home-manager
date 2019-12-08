;;;
;;; Alexey Khudyakov
;;; Text editing customization
;;;


(defun my/switch-dict ()
  "Swtich between dictionaries"
  (interactive)
  (ispell-change-dictionary (if (string= "ru" ispell-dictionary)
                                "en" "ru")))


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
