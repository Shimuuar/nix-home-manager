;;; Alexey Khudyakov
;;;
;;; Tweaks for latex-mode & friends


(defun my/tex-insert-template()
  "Insert base LaTeX template"
  (interactive)
  (insert
"\\documentclass[a4paper]{article}

\\usepackage[russian]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage{fullpage}
\\usepackage{indentfirst}
% \\usepackage{graphicx}

\\begin{document}
\\end{document}
"))

(defun my/tex-switch-quotes ()
  "Hack to replace TeX english `` and '' quotes with << >>"
  (interactive)
  (if (string-equal tex-open-quote  "``")
    (set-variable 'tex-open-quote   "<<")
    (set-variable 'tex-open-quote   "``"))
  (if (string-equal tex-close-quote "''")
    (set-variable 'tex-close-quote  ">>")
    (set-variable 'tex-close-quote  "''")))



;; ================================================================
;; Hooks
;; ================================================================

(defun my/tex-hooks()
  "Hooks for TeX mode"
  (my/hook/base-text)
  (my/hook/make)
  (local-set-key (kbd "C-c q") 'my/tex-switch-quotes)
  (local-set-key (kbd "C-c SPC") (lambda ()
    (interactive)
    (insert "~")
    ))
  (local-set-key (kbd "C-c -") (lambda ()
    (interactive)
    (insert "~---")
    ))
  ; Add more known blocks (esp. for beamer)
  (set-variable 'latex-standard-block-names
    (append '("frame"
	      "block"
	      "exampleblock"
	      "alertblock"
              "columns"
	      "column"
	      "align"
	      "aligned"
              )
            latex-standard-block-names
            ))
  (set-variable 'latex-block-args-alist
    (append '(("column"       nil ?\{ (skeleton-read "Width: ") ?\})
	      ("block"        nil ?\{ (skeleton-read "Title: ") ?\})
	      ("exampleblock" nil ?\{ (skeleton-read "Title: ") ?\})
	      ("alertblock"   nil ?\{ (skeleton-read "Title: ") ?\})
	      )
	    latex-block-args-alist))
  )

(add-hook 'latex-mode-hook 'my/tex-hooks)
(add-hook 'tex-mode-hook   'my/tex-hooks)

(provide 'mod-latex-mode)
