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
;; Host specific choices
(pcase system-name
  ("ogma"
   (custom-set-faces
    '(default ((t (:inherit nil :extend nil :stipple nil
			    :background "black" :foreground "white"
			    :inverse-video nil :box nil :strike-through nil
			    :overline nil :underline nil
			    :slant normal :weight normal :height 90
			    :width normal
			    :foundry "CTDB" :family "Fira Code")))))
  )
  ("lug"
   ;; For some mysterious reason emacs refuses to set font to terminus
   ;; if it isn't changed to something else before
   (progn
     (custom-set-faces
      '(default ((t (:inherit nil :extend nil :stipple nil
			      :background "black" :foreground "white"
			      :inverse-video nil :box nil :strike-through nil
			      :overline nil :underline nil
			      :slant normal :weight normal :height 90
			      :width normal
			      :foundry "CTDB" :family "Fira Code")))))
     (custom-set-faces
      '(default ((t (:family "Terminus" :foundry "Xos4" :slant normal
			     :weight regular :height 135 :width normal))))))
   )
  )


;; No startup message
(setq inhibit-startup-message t)
;; No menu, no toolbar, no scroll bar.
(tool-bar-mode   -1)
(scroll-bar-mode -1)
(menu-bar-mode   -1)
;; Set cursor and mouse-pointer colours
(set-cursor-color "red")
(set-mouse-color  "goldenrod")
(set-face-background 'region "gray30")
(set-foreground-color "white")
(set-background-color "black")
;; Change cursor color depending on input method
(if (boundp 'window-selection-change-functions)
    (progn
      (defun my/set-cursor-by-input-method()
	(cond ((equal   nil                current-input-method) (set-cursor-color "red"))
	      ((string= "russian-computer" current-input-method) (set-cursor-color "#CC44FF"))
	      ))
      ;; Change cursor color when we switch between buffers
      (add-to-list
       'window-selection-change-functions
       (lambda (x) (my/set-cursor-by-input-method))
      ;; Change cursor color when we change layout
      (add-hook 'input-method-activate-hook 'my/set-cursor-by-input-method)
      (add-hook 'input-method-deactivate-hook
		(lambda () (set-cursor-color "red")))
      )))
;; No annoying beeps
(setq visible-bell t)
;; ================


;; ========================================
;; Mode-line settings
;; ========================================
; Line and column numbering
(line-number-mode   t)
(column-number-mode t)
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

;; Set up rich-minority
(when (fboundp 'rich-minority-mode)
  (rich-minority-mode t)
  (setq rm-blacklist
	'(" Undo-Tree"
	  " Fill"

	  ))
  (setq rm-text-properties
	'(("\\` Ovwrt\\'" 'face 'font-lock-comment-face)))
  )


(provide 'my-appearance)
