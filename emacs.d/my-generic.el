;;;
;;; Alexey Khudyakov
;;; Generic customization
;;;


;; =========================================================
;; Backups 
;; =========================================================
; Place Backup Files in Specific Directory 
(setq make-backup-files t)
(setq version-control   t)
; Place Backup Files in Specific Directory 
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")) )
; Remove excess backups silently
(setq delete-old-versions t)
;;=================

;; =========================================================
;; Automode list
;; =========================================================
(setq auto-mode-alist (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SConscript" . python-mode) auto-mode-alist))
;; ===========================


; Support for mouse wheel 
(mouse-wheel-mode t)

; my favourite scrolling
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position t)
(setq scroll-margin 10)

; Visible mark
(setq transient-mark-mode t)

; replace yes/no question with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;============================================================
;; iswitchb
;;============================================================
(require 'iswitchb)  
(iswitchb-mode 1)
; Ignores
(add-to-list 'iswitchb-buffer-ignore "*Messages*")
(add-to-list 'iswitchb-buffer-ignore "*Buffer")
(add-to-list 'iswitchb-buffer-ignore "*Completions")
(add-to-list 'iswitchb-buffer-ignore "*scratch*")
;; =================

; Start emacs server
(server-start)


(provide 'my-generic)