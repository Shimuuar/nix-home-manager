;;;
;;; Alexey Khudyakov
;;; Programming enhancement
;;;

;; =========================================================
;; Useful functions
;; =========================================================
(defun my-insert-guard(title)
  "Insert C/C++ header guards quickly"
  (interactive "sType guard name: ")
  (insert "#ifndef " title
          "\n#define " title
          "\n\n#endif /* " title " */\n" ))

(defun my-insert-c-template(foo)
  "Insert C program template"
  (interactive "i")
  (insert
"
#include <stdlib.h>
#include <stdio.h>


int main(int argc, char** argv)
{
    return 0;
}
"))

(defun my-insert-c++-template(foo)
  "Insert C program template"
  (interactive "i")
  (insert
"
#include <iostream>


int main(int argc, char** argv)
{
    return 0;
}
"))

(defun my-indent-buffer()
  "Indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil))

(defun my-indent-line()
  "Indent line and move to next"
  (interactive)
  (indent-according-to-mode)
  (next-line))

(defun my-match-paren (arg)
  "Jump to matching parenthesis"
  (interactive "p")
  (cond ((looking-at  "[\[<\(\{]") (forward-list  1))
        ((looking-back "[]>\)\}]") (backward-list 1))))

(defun my-comment-or-uncomment-line ()
  "Comment or uncomment line under cursor"
  (interactive)
  (comment-or-uncomment-region (point-at-bol) (point-at-eol)))

(defun my-settab(wid)
  "Set tab width"
  (interactive "nType tab width ")
  (setq tab-width wid))

(defun my-try-flycheck()
  "Enable flycheck if avaialble and proform actions"
  (when (fboundp 'flycheck-mode)
    (flycheck-mode)))

(defun my-haskell-insert-inline ()
  "Insert INLINE pragma. It inserts pragma in directly above the
line."
  (interactive)
  ; Find out indentation and identifier name
  (if (save-excursion
	(move-beginning-of-line 1)
	(looking-at "^\\([ \t]*\\)\\([a-zA-Z][a-zA-Z_1-9]*\\)")
	)
    (let ((spaces (buffer-substring (match-beginning 1) (match-end 1))) ; Indentation
	  (ident  (buffer-substring (match-beginning 2) (match-end 2))) ; Binding name
	  )
      ; Insert pragma
      (save-excursion
	(move-beginning-of-line 1)
	(insert (concat spaces "{-# INLINE " ident " #-}\n")))
      )
    (error "No identifier here!"))
  )

(defun my-haskell-toggle-style ()
  "Toggle style of indentation"
  (interactive)
  (let ((n (if (eq haskell-indentation-layout-offset 2) 4 2)))
    (setq tab-width n
	  haskell-indentation-layout-offset n
	  haskell-indentation-left-offset   n
	  haskell-indentation-ifte-offset   n)))


;; ===============================================
;; Syntax highlighting
;; ===============================================
(global-font-lock-mode t)      ; turn on syntax highlighting
(show-paren-mode t)            ; Highlight parenthesis
(setq show-paren-style 'mixed) ; Highlight whole expression if it's not fully visible



;; ===============================================
;; Indentation
;; ===============================================
; Could be set with c-set-style
(defconst my-c-tab-style
  '( "bsd"
	(indent-tabs-mode . t)
	(tab-width        . 4)
	(c-basic-offset   . 4)
 	(c-echo-syntactic-information-p . t)
 	)
  )
(defconst my-c-ws-style
  '( "bsd"
	(indent-tabs-mode . nil)
	(tab-width        . 4)
	(c-basic-offset   . 4)
 	(c-echo-syntactic-information-p . t)
 	)
  )

(c-add-style "bsd-tab" my-c-tab-style)
(c-add-style "bsd-ws"  my-c-ws-style)
(setq c-default-style "bsd-ws")



;; ========================================================
;; Define hooks
;; =========================================================

(defun my-c-indent-hook()
  "Allow to change indentation "
  (local-set-key (kbd "C-c C-<tab>") (lambda () (interactive)
				       (c-set-style "bsd-tab")))
  (local-set-key (kbd "C-c C-SPC")   (lambda () (interactive)
				       (c-set-style "bsd-ws")))
  )

(defun my-indent-hook()
  "Make new lines indented"
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun my-folding-hooks()
  "Hook for code folding"
  (hs-minor-mode t)
  (local-set-key (kbd "C-S-<left>" ) 'hs-hide-block)
  (local-set-key (kbd "C-S-<right>") 'hs-show-block)
  )

(defun my-comment-hooks ()
  "Hooks for commenting"
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  (local-set-key (kbd "C-c C-v") 'my-comment-or-uncomment-line)
  (flyspell-prog-mode)
  )

;; ================================================================
;; Flycheck hooks
;; ================================================================
(add-hook 'flycheck-mode-hook (lambda ()
  #'flycheck-haskell-setup
  ; Redefine flycheck prefix. "C-c !" is insane
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix
              flycheck-command-map)
  ; Increase delay before displaying error
  (set-variable 'flycheck-display-errors-delay 2)
  ))


;; ================================================================
;; Haskell hooks
;; ================================================================
(require 'haskell)
(require 'haskell-mode)
(require 'haskell-cabal)
(require 'haskell-indentation)
(require 'haskell-move-nested)
(require 'haskell-navigate-imports)
(require 'haskell-sort-imports)
(require 'haskell-font-lock)
(add-hook 'haskell-mode-hook (lambda ()
  "Hooks specific to haskell"
  (turn-on-haskell-indentation)
  ; Haskell hooks are run quite frequently by interactive mode so we
  ; disable most of them in temporary buffers
  (when (buffer-file-name)
    (progn
      ;; generic part
      (my-comment-hooks)
      (abbrev-mode t)
      (interactive-haskell-mode)
      ;; Switch between styles
      (local-set-key (kbd "C-c C-j") 'my-haskell-toggle-style)
      ;; Rename buffer on import.
      (let ((nm (haskell-guess-module-name)))
	(if (and nm (not (string-equal nm "")))
	    (rename-buffer (concat "{" nm "}") t)))
      ;; Move nested blocks
      (define-key haskell-mode-map (kbd "M-<left>")
	(lambda ()
	  (interactive)
	  (haskell-move-nested -1)))
      (define-key haskell-mode-map (kbd "M-<right>")
	(lambda ()
	  (interactive)
	  (haskell-move-nested  1)))
      ;; Navigate imports
      (local-set-key (kbd "M-[") 'haskell-navigate-imports)
      (local-set-key (kbd "M-]") 'haskell-navigate-imports-return)
      ;; PRAGMAS
      (local-set-key (kbd "C-c C-s") 'haskell-mode-insert-scc-at-point)
      (local-set-key (kbd "C-c s"  ) 'haskell-mode-kill-scc-at-point)
      (local-set-key (kbd "C-c i"  ) 'my-haskell-insert-inline)
      ;; Flycheck
      (my-try-flycheck)
      (local-set-key (kbd "C-`")     'haskell-interactive-bring)
      (local-set-key (kbd "C-c C-l") 'haskell-process-load-or-reload)
      ))
    ))


;; ================================================================
;; Python hooks
;; ================================================================

(add-hook 'python-mode-hook (lambda ()
  "Python hooks"
  (abbrev-mode t)
  (my-try-flycheck)
  (my-indent-hook)
  (my-folding-hooks)
  (my-comment-hooks)
  ; Python specific hooks
  (setq tab-width        4  )		; Override tab width
  (setq indent-tabs-mode nil)		; Use spaces for indent
  (setq python-indent    4  )		; Python indentation
  ;; Insert shebang into empty files
  (my-insert-if-empty "#!/usr/bin/python\n"
                      "\"\"\"\n"
                      "\"\"\"\n")
  ))


;; ================================================================
;; Set up hooks
;; ================================================================
;; C hooks
(add-hook-list 'c-mode-hook
  '(my-indent-hook
    my-c-indent-hook
    my-make-hook
    my-comment-hooks
    my-folding-hooks))
;; C++ hooks
(add-hook-list 'c++-mode-hook
  '(my-indent-hook
    my-c-indent-hook
    my-make-hook
    my-comment-hooks
    my-folding-hooks))
;; Shell hooks
(add-hook 'sh-mode-hook (lambda ()
  (my-indent-hook)
  (lambda () (my-insert-if-empty "#!/bin/sh\n\n"))
  ))
;; Lisp hooks
(add-hook 'lisp-mode-hook (lambda ()
  (my-indent-hook)
  (my-comment-hooks)
  ))
;; Elisp hooks
(add-hook 'emacs-lisp-mode-hook (lambda ()
  (my-indent-hook)
  (my-comment-hooks)
  ))

(provide 'my-programming)
