;;;
;;; Alexey Khudyakov
;;; Main emacs configuration file 
;;;

;; Load paths
(defun add-load-path (path)
  (add-to-list 'load-path (expand-file-name path)))
; Now add default pathes
(mapcar 'add-load-path
	(list "~/.emacs.d"
	      "~/.emacs.d/lisp"
	      "~/.emacs.d/lisp-personal"
	      "~/.emacs.d/haskell-mode"))
; Now we need to add path to GHC-mod. If there's more than one
; complain loudly.
(let ((pathes (file-expand-wildcards "~/.cabal/share/*/ghc-mod-*")))
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
