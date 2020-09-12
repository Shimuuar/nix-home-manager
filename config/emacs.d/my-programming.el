;;;
;;; Alexey Khudyakov
;;; Programming enhancement
;;;


;; ===============================================
;; Syntax highlighting
;; ===============================================
(global-font-lock-mode t)      ; turn on syntax highlighting
(show-paren-mode t)            ; Highlight parenthesis
(setq show-paren-style 'mixed) ; Highlight whole expression if it's not fully visible



;; ================================================================
;; Flycheck hooks
;; ================================================================
(add-hook 'flycheck-mode-hook (lambda ()
  #'flycheck-haskell-setup
  ; Redefine flycheck prefix. "C-c !" is insane
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c c"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map)
  ; Increase delay before displaying error
  (set-variable 'flycheck-display-errors-delay 2)
  ))

;; ================================================================
;; Agda mode
;; ================================================================
(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
	          (shell-command-to-string "agda-mode locate")))
  )

;; ================================================================
;; Python hooks
;; ================================================================

(add-hook 'python-mode-hook (lambda ()
  "Python hooks"
  (abbrev-mode t)
  (my/try-flycheck)
  (my/hook/indent)
  (my/hook/folding)
  (my/hook/comment)
  ; Python specific hooks
  (setq tab-width        4  )		; Override tab width
  (setq indent-tabs-mode nil)		; Use spaces for indent
  (setq python-indent    4  )		; Python indentation
  ;; Insert shebang into empty files
  (my/insert-if-empty "#!/usr/bin/python\n"
                      "\"\"\"\n"
                      "\"\"\"\n")
  ))


;; ================================================================
;; Set up hooks
;; ================================================================
;; Shell hooks
(add-hook 'sh-mode-hook (lambda ()
  (my/hook/indent)
  (my/insert-if-empty "#!/bin/sh\n\n")
  ))
;; Lisp hooks
(add-hook 'lisp-mode-hook (lambda ()
  (my/hook/indent)
  (my/hook/comment)
  ))
;; Elisp hooks
(add-hook 'emacs-lisp-mode-hook (lambda ()
  (my/hook/indent)
  (my/hook/comment)
  ))
;; JavaScript hooks
(add-hook 'js-mode-hook (lambda ()
  (my/hook/indent)
  (my/hook/comment)
  (my/hook/folding)
  (setq js-indent-level  2)
  (setq indent-tabs-mode nil)
  ))

(provide 'my-programming)
