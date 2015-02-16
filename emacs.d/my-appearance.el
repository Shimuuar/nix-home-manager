;;;
;;; Alexey Khudyakov
;;; Emacs appearance
;;;


;; ========================================
;; General settings
;; ========================================
; Font faces
(if (intern-soft "font-lock-comment-face")
    (set-face-foreground (symbol-value (intern-soft "font-lock-comment-face")) "red3"))
; No startup message
(setq inhibit-startup-message t)
; No menu, no toolbar, no scroll bar. 
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(menu-bar-mode   -1)
; Set cursor and mouse-pointer colours
(set-cursor-color "red")
(set-mouse-color  "goldenrod")
(set-face-background 'region "gray30")
(set-foreground-color "white")
(set-background-color "black")
; No annoying beeps
(setq visible-bell t)
;; ================


;; ========================================
;; Mode-line settings
;; ========================================
; Line and column numbering
(line-number-mode   t)
(column-number-mode t)
; Display time (May be it's useful)
(display-time)
; Set mode line format
(custom-set-variables
 '(mode-line-format
   (quote ("%e"
	   mode-line-mule-info
	   mode-line-modified
	   mode-line-frame-identification
	   mode-line-buffer-identification
	   "  "
	   mode-line-position
	   (vc-mode vc-mode)
	   " "
	   mode-line-modes
	   ; Tab width display for C/C++ modes and indentation type for haskell mode
	   "-- "
	   (:eval (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
		      (format "--[%s Tab:%i]" (if indent-tabs-mode "tabs" "ws") tab-width)
		      ""
		    ))
	   (:eval (if (eq major-mode 'haskell-mode)
		      (cond ((eq haskell-indentation-layout-offset 2) "[MY]")
			    ((eq haskell-indentation-layout-offset 4) "[JOHAN]")
			    ( t                                       "[WTF!!!]")
		       )
		    ""))
	   )
	   )))
;; ================


(provide 'my-appearance)
