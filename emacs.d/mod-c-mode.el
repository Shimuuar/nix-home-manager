;;; Alexey Khudyakov
;;;
;;; Tweaks for c-mode & c++-mode
;;;

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
