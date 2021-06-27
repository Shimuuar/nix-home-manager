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
(setq org-agenda-files '( "~/data/tracker/"))

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


;; ----------------------------------------------------------------
;; Set up org-roam
(when (boundp 'org-roam-mode)
  ;; Data source 
  (setq org-roam-directory '"~/data/zettel")
  (setq org-roam-completion-system 'helm)
  ;; Key bindings
  (global-set-key (kbd "C-c q") 'org-roam-find-file)
  )
;(setq reftex-default-bibliography	;
 ;/zettel/bib/hep.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/data/zettel/org-ref.org")
(setq org-ref-default-bibliography
      (directory-files "~/data/zettel/bib" t ".\.bib"))
(setq org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")

(provide 'mod-org-mode)
