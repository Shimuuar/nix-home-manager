(when (require 'haskell nil t)
  (setq deft-extensions '("org" "md" "txt" "tex"))
  (setq deft-directory "~/data/zettel")
  ;; List all files recursively
  (setq deft-recursive t)

  ;; bring up deft quickly
  (global-set-key [f8] 'deft)

  ;;
  (setq deft-use-filter-string-for-filename t)

  ;; Autosave interval. Default is 1s which is too aggressive
  (setq deft-auto-save-interval 60)
  )

(provide 'mod-deft)
