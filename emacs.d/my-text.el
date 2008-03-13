;;;
;;; Alexey Khudyakov
;;; Text editing customization
;;;

(defun my-switch-tex-quotes ()
  "Hack to replace TeX english `` and '' quotes with << >>"
  (interactive)
  (if (string-equal tex-open-quote  "``")
    (set-variable 'tex-open-quote   "<<")
    (set-variable 'tex-open-quote   "``"))
  (if (string-equal tex-close-quote "''")
    (set-variable 'tex-close-quote  ">>")
    (set-variable 'tex-close-quote  "''")))

(defun my-recode-buffer-safe (target-coding-system)
  "* Recode buffer as if it were encoded with `target-coding-system'.
  If current buffer is write-protected (`buffer-read-only'), do nothing."
  (interactive "zEnter target coding system: ")
  (unless buffer-read-only
    (encode-coding-region (point-min)
                          (point-max)
                          buffer-file-coding-system)
    (decode-coding-region (point-min)
                          (point-max)
                          target-coding-system)
    (set-buffer-file-coding-system target-coding-system)))

(defun my-change-encoding (actual-encoding) 
  "Sets file encoding
  I'm not sure that it would work as intended everywhere, but it work at least"
  (interactive "zEnter file encoding: ")
  (recode-region (point-min) (point-max)
                 actual-encoding buffer-file-coding-system)
  (set-buffer-file-coding-system actual-encoding))


;; =========================================================
;; Hooks
;; =========================================================
; Basic text hook
(defun my-base-text-hooks()
  (auto-fill-mode 1)
  (set-fill-column 80)
  (flyspell-mode t)
  )

; Hooks for LaTeX
(defun my-tex-hooks()
  (my-base-text-hooks)
  ; Function to insert TeX solid space '~' which is inaccessible in russian layout.
  (local-set-key "\C-c " 
                 '(lambda () (interactive) (insert "~")) )
  ; switch TeX quotes
  (local-set-key "\C-cq" 'my-switch-tex-quotes)
  ; TeX template 
  (my-insert-if-empty "\\documentclass[a4paper]{article}\n"
                      "\n"
                      "\\usepackage[russian]{babel}\n"
                      "\\usepackage[utf8]{inputenc}\n"
                      "\\usepackage{epsfig}\n"
                      "\n"
                      "% margins\n"
                      "%\\oddsidemargin=\n"
                      "%\\textwidth=\n"
                      "%\\topmargin=\n"
                      "%\\textheight=\n"
                      "\n"
                      "\\begin{document}\n"
                      "\\end{document}\n")
  )

(add-hook 'latex-mode-hook 'my-base-text-hooks)
(add-hook 'latex-mode-hook 'my-tex-hooks)
(add-hook 'text-mode-hook  'my-base-text-hooks)

; orphography check
(setq         ispell-dictionary "ru")
(setq-default ispell-program-name "aspell")

(provide 'my-text)