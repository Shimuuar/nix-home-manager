;;;
;;; Alexey Khudyakov
;;;
;;; Generic hooks for various modes


(defun my/hook/indent()
  "RET inserts newline and indents"
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun my/hook/comment()
  "Commenting/uncommenting fullr regions of code"
  (local-set-key (kbd "C-c C-v") 'my/comment-or-uncomment)
  )

(defun my/hook/folding()
  "Hook for code folding"
  (hs-minor-mode t)
  (local-set-key (kbd "C-S-<left>" ) 'hs-hide-block)
  (local-set-key (kbd "C-S-<right>") 'hs-show-block)
  )

(defun my/hook/make()
  "Add quick binding for running make"
  (local-set-key [f8] (lambda () (interactive)
			(compile "make -k")))
  )

(defun my/hook/base-text()
  "Common hooks for text mode"
  (auto-fill-mode  1 )
  (set-fill-column 80)
  (flyspell-mode   t )
  )

(provide 'my-hooks)
