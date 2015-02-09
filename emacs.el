;;;
;;; Alexey Khudyakov
;;; Main emacs configuration file 
;;;

;; Load paths
(defun add-load-path (path)
  (add-to-list 'load-path (expand-file-name path)))
; Now add default pathes
(mapcar 'add-load-path
	(list "~/.emacs.d/lisp"
	      "~/.emacs.d/lisp-personal"
	      "~/.emacs.d/haskell-mode"
	      "~/.emacs.d/haskell-flycheck"))
; Add MELPA to list of packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

; Require local modification (if any)
(require 'my-local "my-local.el" t)

; Load files
(require 'my-generic)
(require 'my-text)
(require 'my-programming)
(require 'my-abbrevs)

(require 'my-bindings)
(require 'my-appearance)
