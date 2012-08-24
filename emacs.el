;;;
;;; Alexey Khudyakov
;;; Main emacs configuration file 
;;;

;; Load paths
(defun add-load-path (path)
  (add-to-list 'load-path (expand-file-name path)))
; Now add lists
(mapcar 'add-load-path
	(list "~/.emacs.d"
	      "~/.emacs.d/lisp"
	      "~/.emacs.d/lisp-personal"
	      "~/.emacs.d/haskell-mode"))
; try to load GHC-mod
(let ((pathes (file-expand-wildcards "~/.cabal/share/ghc-mod-*")))
  (if (> (length pathes) 1)
    (error "Multiple ghc-mods. Do something!!!")
    (mapcar 'add-load-path pathes)))

; Require local modification (if any)
(require 'my-local "my-local.el" t)

; Load files
(require 'my-generic)
(require 'my-text)
(require 'my-programming)
(require 'my-abbrevs)

(require 'my-bindings)
(require 'my-appearance)
