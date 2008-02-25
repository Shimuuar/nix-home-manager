;;;
;;; Alexey Khudyakov
;;; Main emacs configuration file 
;;;

;; Enable a backtrace when problems occur
;; (setq debug-on-error t)

;; Load paths
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))

(require 'my-appearance)
(require 'my-generic)
(require 'my-text)
(require 'my-programming)
(require 'my-abbervs)
(require 'my-bindings)
