;;; Alexey Khudyakov
;;;
;;; Tweaks for c-mode & c++-mode
;;;

;; =========================================================
;; Functions
;; =========================================================

(defun my/insert-guard(title)
  "Insert C/C++ header guards quickly"
  (interactive "sType guard name: ")
  (insert "#ifndef " title
          "\n#define " title
          "\n\n#endif /* " title " */\n" ))


(defun my/insert-c-template(foo)
  "Insert C program template"
  (interactive "i")
  (insert
"
#include <stdlib.h>
#include <stdio.h>


int main(int argc, char** argv)
{
    return 0;
}
"))


(defun my/insert-c++-template(foo)
  "Insert C program template"
  (interactive "i")
  (insert
"
#include <iostream>


int main(int argc, char** argv)
{
    return 0;
}
"))


;; =========================================================
;; C/C++ abbreviations
;; =========================================================
(define-skeleton my/skel-reintepret-cast
  "Skeleton for reinterpret cast"
  "Typename: "
  "reinterpret_cast<" str ">(" _ ")")
(define-skeleton my/skel-static-cast
  "Skeleton for static cast"
  "Typename: "
  "static_cast<" str ">(" _ ")")
(define-skeleton my/skel-printf
  "Skeleton for printf" ""
  "printf(\"" _ "\");" )

(define-abbrev-table 'c-mode-abbrev-table
  '(
    ; Preprocessor directives
    ("inc" "#include"  nil 0)
    ("def" "#define"   nil 0)
    ; Control structures
    ("sw"   "switch"   nil 0)
    ("ca"   "case"     nil 0)
    ("br"   "break"    nil 0)
    ("wh"   "while"    nil 0)
    ("ret"  "return"   nil 0)
    ("cont" "continue" nil 0)
    ; Types and modifiers
    ("co" "const"      nil 0)
    ("vo" "void"       nil 0)
    ("bo" "bool"       nil 0)
    ; printf
    ("pr" ""           my/skel-printf 0)
    ))
(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ; Preprocessor directives
    ("inc" "#include"  nil 0)
    ("def" "#define"   nil 0)
    ; Control structures
    ("sw"   "switch"   nil 0)
    ("ca"   "case"     nil 0)
    ("br"   "break"    nil 0)
    ("wh"   "while"    nil 0)
    ("ret"  "return"   nil 0)
    ("cont" "continue" nil 0)
    ; Types and modifiers
    ("co" "const"      nil 0)
    ("vo" "void"       nil 0)
    ("bo" "bool"       nil 0)
    ; Classes
    ("pub"  "public"   nil 0)
    ("priv" "private"  nil 0)
    ; printf
    ("pr" ""           my/skel-printf 0)
    ; Casts
    ("rcast" ""       my/skel-reintepret-cast 0)
    ("scast" ""       my/skel-static-cast 0)
    ; STL
    ("cout"  "std::cout" nil 0)
    ("cerr"  "std::cerr" nil 0)
    ("endl"  "std::endl" nil 0)
    ))



;; ===============================================
;; Indentation
;; ===============================================

; Could be set with c-set-style
(defconst my/c-tab-style
  '( "bsd"
	(indent-tabs-mode . t)
	(tab-width        . 4)
	(c-basic-offset   . 4)
 	(c-echo-syntactic-information-p . t)
 	)
  )
(defconst my/c-ws-style
  '( "bsd"
	(indent-tabs-mode . nil)
	(tab-width        . 4)
	(c-basic-offset   . 4)
 	(c-echo-syntactic-information-p . t)
 	)
  )

(c-add-style "bsd-tab" my/c-tab-style)
(c-add-style "bsd-ws"  my/c-ws-style)
(setq c-default-style "bsd-ws")


;; ===============================================
;; Hooks
;; ===============================================

;; C hooks
(my/add-hook-list 'c-mode-hook
  '(my/hook/indent
    my/hook/comment
    my/hook/make
    my/hook/folding
    ))
;; C++ hooks
(my/add-hook-list 'c++-mode-hook
  '(my/hook/indent
    my/hook/comment
    my/hook/make
    my/hook/folding
    ))

(provide 'mod-c-mode)
