;;;
;;; Alexey Khudyakov
;;; Main emacs configuration file 
;;;

;; Load paths
(defun add-load-path (path)
  (add-to-list 'load-path (expand-file-name path)))
;; Now add default pathes
(add-load-path "~/.emacs.d/lisp-personal")
(mapcar 'add-load-path
	(seq-filter 'file-directory-p
		    (file-expand-wildcards (expand-file-name "~/.emacs.d/lisp/*"))))

; Load files
(require 'my-generic)
(require 'my-hooks)
(require 'my-text)
(require 'my-programming)
(require 'my-bindings)
(require 'my-appearance)
; Mode tweaks
(require 'mod-hledger-mode)
(require 'mod-org-mode)
(require 'mod-c-mode)
(require 'mod-python-mode)
(require 'mod-haskell-mode)
(require 'mod-latex-mode)
(require 'mod-sql-mode)
(require 'mod-rust-mode)
(require 'mod-deft)
;(require 'my-ebib)
; Require local modification (if any)
(require 'my-local "my-local.el" t)
(require 'my-extra "my-extra.el" t)
