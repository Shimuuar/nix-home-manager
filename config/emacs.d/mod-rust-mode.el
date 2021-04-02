;;; Alexey Khudyakov
;;;
;;; Tweaks for rust-mode

(when (require 'rust-mode nil t)
  ; Indent with spaces
  (setq indent-tabs-mode nil)
  ;
  (add-hook 'rust-mode-hook (lambda ()
    (my/hook/comment)
    ))
  )

(provide 'mod-rust-mode)
