;;;
;;; Alexey Khudyakov
;;; Text editing customization
;;;


(defun my/switch-dict ()
  "Swtich between dictionaries"
  (interactive)
  (ispell-change-dictionary (if (string= "ru" ispell-dictionary)
                                "en" "ru")))

(defun my/format-for-telegram()
  "Format text for pasting to telegram/any other messenger"
  (interactive)
  (if (use-region-p)
      (let ((oldbuf (current-buffer))
	    (buf    (generate-new-buffer " clean-org"))
	    )
	(save-current-buffer
	  (copy-to-buffer buf (region-beginning) (region-end))
	  (set-buffer buf)
 	  ;; Clean up spaces
	  (replace-regexp "^ *" "" nil (point-min) (point-max))
	  (replace-regexp " *$" "" nil (point-min) (point-max))
	  ;; Make paragraphs 1-lines. Hack arguably
	  (set-fill-column 10000)
	  (fill-region (point-min) (point-max))
 	  ;; Copy to kill-ring
	  (kill-ring-save (point-min) (point-max))
	  (kill-buffer buf)
	  ))
    (message "foofoo: No selection")
    ))


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
