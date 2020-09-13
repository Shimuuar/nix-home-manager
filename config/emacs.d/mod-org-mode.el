;;; Alexey Khudyakov
;;;
;;; Tweaks for org-mode
;;;

(setq org-todo-keywords
      '((sequence "TODO" "PROG" "BLOCKED" "|" "DONE" "DROPPED")))

;; FIXME: is this a right way?
(custom-set-variables
  '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("BLOCKED" . "red")
      )))))
;; Set up agenda files
(add-hook 'org-mode-hook (lambda ()
  (local-set-key (kbd "C-c d") (lambda () (interactive) (org-todo "DONE")))
  (setq org-agenda-files
	'("~/data/tracker/todo.org"
	  "~/data/tracker/realty.org"
	  "~/data/tracker/work.org"
	  "~/data/tracker/trading.org"
	  ))
  ))

(provide 'mod-org-mode)
