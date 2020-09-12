;;; Alexey Khudyakov
;;;
;;; Tweaks for sql-mode
;;;

(add-hook 'sql-mode-hook (lambda ()
  (my/hook/comment)
  ))

(provide 'mod-sql-mode)
