;;;
;;; Alexey Khudyakov
;;; Global key bindings
;;;

;; =========================================================
;; F# keys shortcuts 
;; =========================================================
; Move between windows quickly quickly
(global-set-key [(shift up)]  
                (lambda () (interactive) (other-window 1)) )
(global-set-key [(shift down)] 
                (lambda () (interactive) (other-window -1)) )
; Close curent buffer
(global-set-key [f10]            
                (lambda () (interactive) (kill-buffer (current-buffer))) )
; Swith to buffer (same as 'C-x b')
(global-set-key [f12] 'switch-to-buffer)
; Call last recorded macro
(global-set-key [f2] 'call-last-kbd-macro)
;; Swap windows
(global-set-key (kbd "C-x t") 'my/swap-windows)
;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; =========================================================
;; kill-yank
;; =========================================================
; Replace kill-region with backward-kill-word
(global-set-key (kbd "C-w") 'backward-kill-word)
; Set kill-region to "C-x C-k"
(global-set-key (kbd "C-x C-k") 'kill-region)

;; =========================================================
;; Programming and identation 
;; =========================================================
; Autoidentation of source code 
(global-set-key (kbd "C-<tab>") 'my/indent-line)
; Find matching paren
(global-set-key (kbd "C-%") 'my/match-paren)


(provide 'my-bindings)
