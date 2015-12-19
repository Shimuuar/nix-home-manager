;;;
;;; Alexey Khudyakov
;;; Main emacs configuration file 
;;;

;; Load paths
(defun add-load-path (path)
  (add-to-list 'load-path (expand-file-name path)))
;; Now add default pathes
(mapcar 'add-load-path
	(list "~/.emacs.d/lisp"
	      "~/.emacs.d/lisp-personal"
	      "~/.emacs.d/haskell-mode"
	      "~/.emacs.d/haskell-flycheck"))
;; Load necessary packages
;(require 'cl)


; Define packages 
(when (>= emacs-major-version 24)
  (require 'package)
  ; Add MELPA to list of packages
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.org/packages/") t)
  (package-initialize)

  ; Check that all required packages are installed
  (defvar prelude-packages
    '( haskell-mode
       flycheck
       flycheck-haskell
       browse-kill-ring
       undo-tree
       )
    "List of required packages")
  (dolist (p prelude-packages)
    (unless (package-installed-p p)
      (message "WARNING: `%s' is not installed" p)))

)

; Require local modification (if any)
(require 'my-local "my-local.el" t)

; Load files
(require 'my-generic)
(require 'my-text)
(require 'my-programming)
(require 'my-abbrevs)
(require 'my-bindings)
(require 'my-appearance)
;; FIXME: is this a right way?
(custom-set-variables
  '(safe-local-variable-values
   (quote
    ((org-todo-keyword-faces
      ("BLOCKED" . "red")
      ("RC" . "yellow"))))))
