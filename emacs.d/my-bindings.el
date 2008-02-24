;;;
;;; Alexey Khudyakov
;;; Global key bindings
;;;


; Move between windows quickly quickly
(global-set-key [(shift up)]  
                (lambda () (interactive) (other-window 1)) )
(global-set-key [(shift down)] 
                (lambda () (interactive) (other-window -1)) )

; Close curent buffer
(global-set-key [f10]            
                (lambda () (interactive) (kill-buffer (current-buffer))) )

; Finish editing when acting as server
(global-set-key [f9] 'server-edit)

; Swith to buffer (same as 'C-x b')
(global-set-key [f12] 'switch-to-buffer)

; Call last recorded macro
(global-set-key [f2] 'call-last-kbd-macro)

; Replace kill-region with backward-kill-word
(global-set-key "\C-w" 'backward-kill-word)

; Set kill-region to "C-x C-k"
(global-set-key "\C-x\C-k" 'kill-region)

; Autoidentation of source code 
(global-set-key [\C-tab]
               '(lambda () (interactive) (c-indent-line) (next-line)))


(provide 'my-bindings)