;; Configuration for ebib

(when (require 'ebib nil t)
  (setq ebib-preload-bib-files '("hep.bib"))
  (setq ebib-bib-search-dirs '("/home/alexey/data/zettel/bib"))
  )

(provide 'my-ebib)

