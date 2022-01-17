;; Small script for starting org-mode agenda in a way it should
(find-file "~/data/tracker/work.org")
(find-file "~/data/tracker/todo.org")
(find-file "~/data/tracker/open-source.org")
(find-file "~/data/tracker/regular.org")
(org-agenda nil "a")
;; Set up layout correctly. Very clunky
(delete-other-windows)
(split-window-right)
(switch-to-buffer "todo.org")
