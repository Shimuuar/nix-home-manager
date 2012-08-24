;;;
;;; Alexey Khudyakov
;;; Main emacs configuration file 
;;;

;; Load paths
(mapcar (lambda (path) (add-to-list 'load-path (expand-file-name path)))
	(list "~/.emacs.d"
	      "~/.emacs.d/lisp"
	      "~/.emacs.d/lisp-personal"
	      "~/.emacs.d/haskell-mode"))

; Require local modification (if any)
(require 'my-local "my-local.el" t)

; Load files
(require 'my-generic)
(require 'my-text)
(require 'my-programming)
(require 'my-abbrevs)

(require 'my-bindings)
(require 'my-appearance)
(require 'my-ghc-core)
