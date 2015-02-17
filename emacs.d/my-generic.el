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



;; =========================================================
;; Automode list
;; =========================================================
; scons files
(setq auto-mode-alist
  (append
   ; SCons
   '(("SConstruct" . python-mode)
     ("SConscript" . python-mode)
   ; ReST mode
     ("\\.rst$"  . rst-mode)
     ("\\.rest$" . rst-mode)
   ; Markdown mode
     ("\\.md$"       . markdown-mode)
     ("\\.mark$"     . markdown-mode)
     ("\\.markdown$" . markdown-mode)
   ; Haskell
     ("\\.hs"  . haskell-mode)
     ("\\.hsc" . haskell-mode)
     ) auto-mode-alist))


;; =========================================================
;; Scrolling
;; =========================================================
; Support for mouse wheel
(mouse-wheel-mode t)
; my favourite scrolling
(setq scroll-conservatively           50)
(setq scroll-preserve-screen-position t )
(setq scroll-margin                   10)



;;============================================================
;; iswitchb
;;============================================================
(require 'iswitchb)
(iswitchb-mode 1)
; Ignores
(mapcar (lambda (buf) (add-to-list 'iswitchb-buffer-ignore buf))
	'("*Buffer"    "*Completions" "*ESS"
	  "*Apropos"    "*Warnings"  "*Quail"))



;; =========================================================
;; Miscelanneous
;; =========================================================
; Visible mark
(setq transient-mark-mode t)
; replace yes/no question with y/n
(fset 'yes-or-no-p 'y-or-n-p)
; Disable abbreviation saving
(setq save-abbrevs nil)



;; =========================================================
;; Start emacs server
;; =========================================================
; Temporarily disabled
;(server-start))
;; =================


;; =========================================================
;; Useful functions
;; =========================================================
(defun my/insert-if-empty(&rest msg)
  (if (= 0 (- (point-min) (point-max)))
      (mapcar 'insert msg)))

(defun add-hook-list (hook hooks-list)
  "Add list of hooks"
  (mapcar (lambda (one-hook) (add-hook hook one-hook)) hooks-list))

(defun my/make-hook()
  "Add quick binding for compile"
  (local-set-key [f8] (lambda () (interactive)
			(compile "make -k")))
  )


(provide 'my-generic)
