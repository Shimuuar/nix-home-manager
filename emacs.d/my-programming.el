;;;
;;; Alexey Khudyakov
;;; Programming enhancement
;;;

;; =========================================================
;; Useful functions 
;; =========================================================
(defun my-insert-guard(title)
  "Insert C/C++ header guards quickly"
  (interactive "sType guard name ")
  (insert "#ifndef " title 
          "\n#define " title 
          "\n\n#endif /* " title " */\n" ))

(defun my-insert-c-template(foo)
  "Insert C program template"
  (interactive "i")
  (insert "\n"
	  "#include <stdlib.h>\n"
	  "#include <stdio.h>\n"
	  "\n"
	  "\n"
	  "int main(int argc, char** argv)\n"
	  "{\n"
	  "\n"
	  "    return 0;\n"
	  "}\n"))

(defun my-insert-c++-template(foo)
  "Insert C program template"
  (interactive "i")
  (insert "\n"
	  "#include <iostream>\n"
	  "\n"
	  "\n"
	  "int main(int argc, char** argv)\n"
	  "{\n"
	  "\n"
	  "    return 0;\n"
	  "}\n"))

(defun my-indent-buffer()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun my-indent-line()
  "indent line and move to next"
  (interactive)
  (indent-according-to-mode)
  (next-line))

(defun my-match-paren (arg)
  "Jump to matching parenthesis"
  (interactive "p")
  (cond ((looking-at "[\({\[]")   (forward-list  1))
        ((looking-back "[]\)}]") (backward-list 1))))

(defun my-comment-or-uncomment-line ()
  "Comment or uncomment line under cursor"
  (interactive)
  (comment-or-uncomment-region (point-at-bol) (point-at-eol)))

(defun my-show-tab
  "Display tab width"
  (interactive) 
  (princ (format "Tab width: %i" tab-width)))

(defun my-settab(wid)
  "Set tab width"
  (interactive "nType tab width ")
  (setq tab-width wid))
;; =================


;; ===============================================
;; Syntax highlighting
;; ===============================================
; turn on syntax highlighting
(global-font-lock-mode t)
; Highlight parensthesis
(show-paren-mode t) 
; Highlight whole expression if it's not fully visible
(setq show-paren-style 'mixed) 
;; =================


;; ===============================================
;; Indentation
;; ===============================================
(defconst my-c-style
  '( "bsd"
	(indent-tabs-mode . t)
	(tab-width        . 4)
	(c-basic-offset   . 4)
	; Make public:/private: indented 
;	(c-offsets-alist  . ((inclass       . ++ )))
 	(c-echo-syntactic-information-p . t)
 	)
  )

(c-add-style "my-style" my-c-style)
(setq c-default-style "my-style")
;; =================


;; ========================================================
;; Define hooks 
;; =========================================================

(defun my-indent-hook()
  "Make new lines indented"
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun my-folding-hooks()
  "Hook for code folding"
  (hs-minor-mode t)
  (local-set-key (kbd "C-S-<left>") 'hs-hide-block)
  (local-set-key (kbd "C-S-<right>") 'hs-show-block)
  )

(defun my-comment-hooks ()
  "Hooks for commenting"
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  (local-set-key (kbd "C-c C-v") 'my-comment-or-uncomment-line)
  )

(defun my-python-hooks ()
  "Hooks specific to python"
  (setq tab-width 4) ; Override tab width
  (abbrev-mode t)    ; Set abberviation mode
  ; Insert shebang into empty files
  (my-insert-if-empty "#!/usr/bin/python\n"
                      "\"\"\"\n"
                      "\"\"\"\n")
  )

(defun my-c-hooks ()
  )
    

; C hooks 
(add-hook 'c-mode-hook          'my-indent-hooks)
(add-hook 'c-mode-hook          'my-comment-hooks)
(add-hook 'c-mode-hook          'my-folding-hooks)
(add-hook 'c-mode-hook          'my-c-hooks)
; C++ hooks 
(add-hook 'c++-mode-hook        'my-indent-hooks)
(add-hook 'c++-mode-hook        'my-comment-hooks)
(add-hook 'c++-mode-hook        'my-folding-hooks)
(add-hook 'c++-mode-hook        'my-c-hooks)
; Python hooks 
(add-hook 'python-mode-hook     'my-indent-hooks)
(add-hook 'python-mode-hook     'my-folding-hooks)
(add-hook 'python-mode-hook     'my-python-hooks)
; Shell hooks 
(add-hook 'sh-mode-hook         'my-indent-hooks)
; Lisp hooks 
(add-hook 'lisp-mode-hook       'my-indent-hooks)
(add-hook 'lisp-mode-hook       'my-comment-hooks)
; Elisp hooks
(add-hook 'emacs-lisp-mode-hook 'my-indent-hooks)
(add-hook 'emacs-lisp-mode-hook 'my-comment-hooks)
; Haskell hooks
(add-hook 'haskell-mode-hook    'my-comment-hooks)
;; =================

(provide 'my-programming)
