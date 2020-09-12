;;; Alexey Khudyakov
;;;
;;; Tweaks for hledger-mode
;;;
(when (require 'hledger-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))
  )


(provide 'mod-hledger-mode)
