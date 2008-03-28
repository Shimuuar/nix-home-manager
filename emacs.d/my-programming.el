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
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq c-default-style "bsd" c-basic-offset 4)
;; =================


;; ========================================================
;; Define hooks 
;; =========================================================
(defun my-ret-hook()
  "Make new lines indented"
  (local-set-key (kbd "RET") 'newline-and-indent)
  )

(defun my-python-hooks ()
  "Hooks specific to python"
  (setq tab-width 4) ; Override tab width
  (abbrev-mode t)    ; Set abberviation mode
  ; Insert shebang into empty files
  (my-insert-if-empty "#!/usr/bin/python\n"
                      "\"\"\"\n"
                      "\"\"\"\n")
  ; Code folding for python with hs-minormode
  (hs-minor-mode t)
  (local-set-key (kbd "C-S-<left>") 'hs-hide-block)
  (local-set-key (kbd "C-S-<right>") 'hs-show-block)
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  )

(defun my-c-or-c++-hooks ()
  "Hooks either for C or C++"
  (local-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
  (local-set-key (kbd "C-c C-v") 'my-comment-or-uncomment-line)
  )

; C hooks 
(add-hook 'c-mode-hook          'my-ret-hook)
(add-hook 'c-mode-hook          'my-c-or-c++-hooks)
; C++ hooks 
(add-hook 'c++-mode-hook        'my-ret-hook)
(add-hook 'c++-mode-hook        'my-c-or-c++-hooks)
; Python hooks 
(add-hook 'python-mode-hook     'my-ret-hook)
(add-hook 'python-mode-hook     'my-python-hooks)
; Shell hooks 
(add-hook 'sh-mode-hook         'my-ret-hook)
; Lisp hooks 
(add-hook 'lisp-mode-hook       'my-ret-hook)
(add-hook 'emacs-lisp-mode-hook 'my-ret-hook)
;; =================

(provide 'my-programming)
