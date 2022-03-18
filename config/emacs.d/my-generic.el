;;;
;;; Alexey Khudyakov
;;; Generic customization
;;;

;; =========================================================
;; Useful functions
;; =========================================================
(defun my/insert-if-empty(&rest msg)
  (if (= 0 (- (point-min) (point-max)))
      (mapcar 'insert msg)))

(defun my/add-hook-list (hook hooks-list)
  "Add list of hooks"
  (mapcar (lambda (one-hook) (add-hook hook one-hook)) hooks-list))

(defun my/indent-buffer()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun my/indent-line()
  "Indent line and move to next"
  (interactive)
  (indent-according-to-mode)
  (next-line))

(defun my/match-paren (arg)
  "Jump to matching parenthesis"
  (interactive "p")
  (cond ((looking-at  "[\[<\(\{]") (forward-list  1))
        ((looking-back "[]>\)\}]") (backward-list 1))))

(defun my/comment-or-uncomment ()
  "Comment or uncomment line under cursor or current selection"
  (interactive)
  (if mark-active
    (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (point-at-bol)     (point-at-eol))))

(defun my/try-flycheck()
  "Enable flycheck if avaialble and proform actions"
  (when (fboundp 'flycheck-mode)
    (flycheck-mode)))

(defun my/settab(wid)
  "Set tab width"
  (interactive "nType tab width ")
  (setq tab-width wid))

(defun my/swap-windows ()
  "Swap windows. Will only work if there're only two windows"
  (interactive)
  (if (= 2 (length (window-list)))
    (let ((this-win (window-buffer))
	  (next-win (window-buffer (funcall 'next-window)))
	  )
      (set-window-buffer (selected-window)      next-win)
      (set-window-buffer (funcall 'next-window) this-win)
      (select-window (funcall 'next-window))
      )
    (error "Can only swap 2 panes")))


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
(setq auto-mode-alist
  (append
   ;; SCons
   '(("SConstruct" . python-mode)
     ("SConscript" . python-mode)
   ;; ReST mode
     ("\\.rst$"  . rst-mode)
     ("\\.rest$" . rst-mode)
   ;; Markdown mode
     ("\\.md$"       . markdown-mode)
     ("\\.mark$"     . markdown-mode)
     ("\\.markdown$" . markdown-mode)
   ;; Haskell
     ("\\.hs"  . haskell-mode)
     ("\\.hsc" . haskell-mode)
     ;; CUDA
     ("\\.cu"  . c++-mode)
   ;; Conf files
     (".offlineimaprc" . conf-mode)
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
;; Switching between buffers
;;============================================================
(when (boundp 'helm-mode)
  (helm-mode)
  ;; Bound helm-based functions
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key helm-find-files-map (kbd "C-w") 'helm-find-files-up-one-level)
  (define-key helm-find-files-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-find-files-map (kbd "S-<tab>") 'helm-execute-action)  
  ;; Helm uses current input method of buffer. This is usually very
  ;; wrong thing. This resets input method to default
  (add-hook 'helm-minibuffer-set-up-hook
            (lambda ()
              (deactivate-input-method)))
  )
;; (require 'ido)
;; (ido-mode 1)
;; ;; Ignores following buffers
;; (setq ido-ignore-buffers
;;   (append '("*Buffer"    "*Completions" "*ESS"
;; 	    "*Apropos"    "*Warnings"  "*Quail"
;; 	    "*Flycheck error messages*"
;; 	    )
;; 	   ido-ignore-buffers
;; 	  ))
;; ;; Ensure case sensitivity
;; (when ido-case-fold (ido-toggle-case))
;; ;; Tweak appearance
;; (setq ido-decorations
;;       '("\n{" "}" " | " " | ..."
;; 	"[" "]"
;; 	" [No match]"
;; 	" [Matched]"
;; 	" [Not readable]"
;; 	" [Too big]"
;; 	" [Confirm]"))
;; ;; 
;; (add-hook 'ido-setup-hook (lambda ()
;;   (define-key ido-completion-map (kbd "C-w")    'backward-kill-word)
;;   (define-key ido-completion-map (kbd "<up>")   'ido-prev-work-file)
;;   (define-key ido-completion-map (kbd "<down>") 'ido-next-work-file)
;;   ))


;; =========================================================
;; Miscelanneous
;; =========================================================

;; Adds undo/redo functionality for window configuration
;;  C-c <Left>  - Undo
;;  C-c <Right> - redo
(when (fboundp 'winner-mode)
  (winner-mode 1))
;; Add undo tree mode
(when (require 'undo-tree nil t)
  (progn
    (global-undo-tree-mode)
    ))

;; Visible mark
(setq transient-mark-mode t)
;; replace yes/no question with y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; Disable abbreviation saving
(setq save-abbrevs nil)



(provide 'my-generic)
