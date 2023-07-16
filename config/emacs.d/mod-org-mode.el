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
(setq org-adapt-indentation t)

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
(setq org-roam-v2-ack t)
(when (require 'org-roam-mode nil t)
  ;; Data source 
  (setq org-roam-directory '"~/data/zettel")
  (setq org-roam-completion-system 'helm)
  ;; We need to disable exclusion oherwise whole direcory gets excluded. See:
  ;;  - https://github.com/org-roam/org-roam/issues/2165
  ;;  - https://github.com/org-roam/org-roam/pull/2178
  ;;
  ;; So far has not propagated to nixpkgs yet
  (setq org-roam-file-exclude-regexp nil)
  ;; Key bindings
  (global-set-key (kbd "C-c q") 'org-roam-node-find)
  ;; Render tags too
  (setq org-roam-node-display-template "${title:*} ${tags:50}")
  ;; Enable DB autosync
  (org-roam-db-autosync-enable)
  )
;(setq reftex-default-bibliography	;
 ;/zettel/bib/hep.bib"))

;; see org-ref for use of these variables
(when (require 'org-ref nil t)
  (setq bibtex-completion-bibliography (directory-files "~/data/zettel/bib" t ".\.bib"))
  (setq bibtex-completion-library-path "~/data/pdf/")  
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  )

(provide 'mod-org-mode)
