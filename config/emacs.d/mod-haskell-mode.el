;;; Alexey Khudyakov
;;;
;;; Tweaks for haskell-mode

;; =========================================================
;; Useful functions
;; =========================================================
(defun my/haskell-insert-inline ()
  "Insert INLINE pragma. It inserts pragma in directly above the
line."
  (interactive)
  ; Find out indentation and identifier name
  (if (save-excursion
	(move-beginning-of-line 1)
	(looking-at "^\\([ \t]*\\)\\([a-zA-Z][a-zA-Z_1-9]*\\)")
	)
    (let ((spaces (buffer-substring (match-beginning 1) (match-end 1))) ; Indentation
	  (ident  (buffer-substring (match-beginning 2) (match-end 2))) ; Binding name
	  )
      ; Insert pragma
      (save-excursion
	(move-beginning-of-line 1)
	(insert (concat spaces "{-# INLINE " ident " #-}\n")))
      )
    (error "No identifier here!"))
  )

(defun my/haskell-insert-language-pragma()
  "Insert LANGUAGE pragmas at top of the file"
  (interactive)
  (atomic-change-group
    (save-excursion
      (beginning-of-buffer)
      (search-forward "{-# LANGUAGE" nil t)
      (move-beginning-of-line nil)
      (insert "{-# LANGUAGE ")
      (insert (completing-read "Language extension: " my/haskell-language-pragmas))
      (insert " #-}\n")
      )))

(defun my/haskell-find-pragma-region(str)
  "Find region which contains LANGUAGE pragmas"
  (save-match-data
    (let ((p1 (string-match "\\(^ *{-# +LANGUAGE +\\([a-zA-Z0-9]+\\) +#-} *\n\\)+" str))
	  (p2 (match-end 0))
	  )
      (if p1 (cons p1 p2) nil))))

(defun my/haskell-align-language-pragmas()
  "Align and sort language pragmas"
  (interactive)
  (pcase (my/haskell-find-pragma-region (buffer-string))
    (`(,p1 . ,p2)
     (save-excursion
       (save-restriction
	 (narrow-to-region (if (= 0 p1) 1 p1) p2)
	 (replace-regexp "^ +" "")
	 (align-regexp           (point-min) (point-max) "\\(\\s-*\\)#-}")
	 (sort-lines nil         (point-min) (point-max))
	 (delete-duplicate-lines (point-min) (point-max))
	 )))
    ))

(defun my/haskell-insert-module-stub()
  "Insert stub declaration for haskell module"
  (interactive)
  (insert "-- |\n")
  (insert (concat "module " (haskell-guess-module-name) " where\n"))
  )

;; =========================================================
;; Haskell abbreviations
;; =========================================================
(define-skeleton my/skel-haskell-LANG
  "Skeleton for haskell LANGUAGE pragma"
  ""
  "{-# LANGUAGE "
  (completing-read "Language extension: " my/haskell-language-pragmas)
  " #-}" 
  )
(define-skeleton my/skel-haskell-OPTGCH
  "Skeleton for haskell OPTIONS_GHC pragma"
  ""
  "{-# OPTIONS_GHC "
  (completing-read "Command line: " my/haskell-ghc-options)
  " #-}" 
  )


;; ================================================================
;; Haskell hooks
;; ================================================================

(when (require 'haskell nil t)
  (require 'haskell-mode)
  (require 'haskell-cabal)
  (require 'haskell-indentation)
  (require 'haskell-move-nested)
  (require 'haskell-navigate-imports)
  (require 'haskell-sort-imports)
  (require 'haskell-font-lock)
  ;;
  (setq auto-mode-alist
	(append
	 '(("cabal.project" . haskell-cabal-mode)
	   ) auto-mode-alist))

  ;; Generate TAGS
  (custom-set-variables '(haskell-tags-on-save t))
  ;; ----------------------------------------------------------------
  ;; Change keybindings
  (define-key haskell-mode-map (kbd "C-c C-v") 'my/comment-or-uncomment)
  (define-key haskell-mode-map (kbd "C-c v")   'haskell-cabal-visit-file)
  (define-key haskell-mode-map (kbd "C-c u")
    (lambda () (interactive)
      (insert "undefined")))
  ;; Move nested blocks
  (define-key haskell-mode-map (kbd "M-<left>")
    (lambda () (interactive)
      (haskell-move-nested -1)))
  (define-key haskell-mode-map (kbd "M-<right>")
    (lambda () (interactive)
      (haskell-move-nested  1)))
  ;; Comments
  (define-key haskell-mode-map (kbd "M-[") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "M-]") 'haskell-navigate-imports-return)
  ;; PRAGMAS
  (define-key haskell-mode-map (kbd "C-c i i") 'my/haskell-insert-inline)
  (define-key haskell-mode-map (kbd "C-c i l")
    (lambda () (interactive)
      (progn
	(atomic-change-group
	  (my/haskell-insert-language-pragma)
	  (my/haskell-align-language-pragmas)))))
  (define-key haskell-mode-map (kbd "C-c i a") 'my/haskell-align-language-pragmas)
  (define-key haskell-mode-map (kbd "C-c i m") 'my/haskell-insert-module-stub)
  ;; Remove unneeded keybindings
  (define-key interactive-haskell-mode-map (kbd "C-c C-v") nil)

  ;; Constants definition
  (defcustom my/haskell-language-pragmas
    (split-string (shell-command-to-string "ghc --supported-extensions"))
    "List of language pragmas supported by the installed version of GHC."
    :group 'my/haskell
    :type  '(repeat string))
  (defcustom my/haskell-ghc-options
    (split-string (shell-command-to-string "ghc --show-options"))
    "List of all GHC's command line options"
    :group 'my/haskell
    :type  '(repeat string))

  (add-hook 'haskell-mode-hook (lambda ()
    "Hooks specific to haskell"
    (turn-on-haskell-indentation)
    ;; generic part
    (my/hook/comment)
    (abbrev-mode t)
    (flyspell-prog-mode)		; Spellcheck in comments
    (interactive-haskell-mode)
    ;; Rename buffer on import.
    (when (buffer-file-name)
      (let ((nm (haskell-guess-module-name)))
	(if (and nm (not (string-equal nm "")))
	    (rename-buffer (concat "{" nm "}") t)))
      (my/try-flycheck)
      )
    ))
  )


(provide 'mod-haskell-mode)
