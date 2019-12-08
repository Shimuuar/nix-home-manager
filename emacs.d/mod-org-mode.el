;;; Alexey Khudyakov
;;;
;;; Tweaks for org-mode
;;;

(setq org-todo-keywords
      '((sequence "TODO" "PROG" "RC" "BLOCKED" "|" "DONE" "DROPPED")))

; FIXME: is this a right way?
(custom-set-variables
  '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("BLOCKED" . "red")
      ("RC"      . "yellow"))))))

(provide 'mod-org-mode)
