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

;; Customize org-mode
(setq org-agenda-start-on-weekday 1)
(setq calendar-week-start-day     1)
(setq org-agenda-files
      '("~/data/tracker/todo.org"
	"~/data/tracker/realty.org"
	"~/data/tracker/work.org"
	"~/data/tracker/trading.org"
	))

;; Set up agenda files
(add-hook 'org-mode-hook (lambda ()
  ;; Key bindings
  (local-set-key (kbd "C-c d") (lambda () (interactive) (org-todo "DONE")))
  ;; Delete trailing space on save
  (add-hook 'before-save-hook
	    (lambda () (delete-trailing-whitespace))
	    nil
	    'local)
  ))

(provide 'mod-org-mode)
