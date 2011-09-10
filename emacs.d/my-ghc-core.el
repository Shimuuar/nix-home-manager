;;; ghc-core.el --- Syntax highlighting module for GHC Core

;; Copyright (C) 2010  Johan Tibell

;; Author: Johan Tibell <johan.tibell@gmail.com>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Purpose:
;;
;; To make it easier to read GHC Core output by providing highlighting
;; and removal of commonly ignored annotations.

;;; Code:

(require 'haskell-mode)
(require 'haskell-font-lock)

(defun my-ghc-core-prune-regex (regex &optional nchar)
  "Remove all strings matched by regex in current region
   Optionally remove n characters after regexp."
  (goto-char (point-min))
  (while (search-forward-regexp regex nil t)
    (progn (replace-match "" nil t)
	   (if (and (integerp nchar) (> nchar 0))
	       (delete-char nchar)
	   ))
    ))

(defun my-ghc-core-flush-line-regex (regex)
  "Remove line matched by regex"
  (goto-char (point-min))
  (while (flush-lines regex nil))
  )

(defun my-ghc-core-clean-region (start end)
  "Remove commonly ignored annotations and namespace
prefixes in the given region."
  (interactive "r")
  (save-restriction 
    (narrow-to-region start end)
    ; GHC.Whatever prefixes
    (my-ghc-core-prune-regex "GHC\.[A-Za-z]+\.")
    ; Main module prefix
    (my-ghc-core-prune-regex "Main\.")
    ))

(defun my-ghc-core-clean-buffer ()
  "Remove commonly ignored annotations and namespace
prefixes in the current buffer."
  (interactive)
  (my-ghc-core-clean-region (point-min) (point-max)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . my-ghc-core-mode))

;;;###autoload
(define-derived-mode my-ghc-core-mode haskell-mode "GHC-Core"
  "Major mode for GHC Core files.")

(provide 'my-ghc-core)
;;; my-ghc-core.el ends here

