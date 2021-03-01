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
	"~/data/tracker/work.org"
	"~/data/tracker/delayed.org"
	"~/data/tracker/trading.org"
	))

;; ----------------------------------------------------------------
;; Hooks and keybindings
(add-hook 'org-mode-hook (lambda ()
  ;; Key bindings
  (local-set-key (kbd "C-c d")    (lambda () (interactive) (org-todo "DONE")))
  (local-set-key (kbd "S-<up>")   (lambda () (interactive) (other-window 1)))
  (local-set-key (kbd "S-<down>") (lambda () (interactive) (other-window -1)))
  (local-set-key (kbd "M-<up>")   (lambda () (interactive) (org-shiftup)))
  (local-set-key (kbd "M-<down>") (lambda () (interactive) (org-shiftdown)))
  ;; Delete trailing space on save
  (add-hook 'before-save-hook
	    (lambda () (delete-trailing-whitespace))
	    nil
	    'local)
  ))

;; ----------------------------------------------------------------
;; org-agenda
(add-hook 'org-agenda-mode-hook (lambda ()
  ;; Key bindings
  (local-set-key (kbd "S-<up>")   (lambda () (interactive) (other-window 1)))
  (local-set-key (kbd "S-<down>") (lambda () (interactive) (other-window -1)))
  ))
;; Add CLOSED when entry marked as DONE
(setq org-log-done 'time)

(provide 'mod-org-mode)
