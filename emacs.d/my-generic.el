;;;
;;; Alexey Khudyakov
;;; Generic customization
;;;


;; Backups 
;;
; Place Backup Files in Specific Directory 
(setq make-backup-files t)
(setq version-control   t)
; Place Backup Files in Specific Directory 
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")) )
; Remove excess backups silently
(setq delete-old-versions t)


; Add scons files to automode list
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))


; Support for mouse wheel 
(mouse-wheel-mode t)
; my favourite scrolling
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position t)
(setq scroll-margin 10)


;; Programming enhacements
; Visible mark
(setq transient-mark-mode t)
; Syntax highlighting
(global-font-lock-mode t)
; Highlight parensthesis
(show-paren-mode t)
; Indentation
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "bsd" c-basic-offset 4)


; Start emacs server
(server-start)


(provide 'my-generic)