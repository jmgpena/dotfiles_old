;; org-mode configuration
(require 'org-habit)
(require 'org-special-blocks)

(setq org-completion-use-ido t)
(setq org-startup-indented t)

(global-set-key (kbd "C-'") 'org-cycle-agenda-files)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

(when (file-exists-p "~/Dropbox/org")
  (setq org-directory "~/Dropbox/org")
  (setq org-default-notes-file (concat org-directory "/wip.org"))
  (define-key global-map (kbd "C-c c") 'org-capture)

  (setq org-capture-templates
	'(("t" "Todo" entry (file+headline "~/Dropbox/org/wip.org" "inbox")
	   "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:")
	  ;; ("d" "Data entry" table-line (file+headline "~/Dropbox/org/track.data.org" "Personal data")
	  ;;  "| %^T | %^{What} | %^{Qtd|1} | %^{Unit|-} | %^{Notes|-}" :prepend t)
	  ("j" "Journal" entry (file+datetree "~/Dropbox/org/dailylog.org")
	   "* %U :journal:\n\n%?")
	  ("n" "Note" entry (file+headline "~/Dropbox/org/wip.org" "inbox")
	   "* %? :note:\nEntered on %U\n%i\n")
	  ;; ("l" "Log Time" entry (file+datetree "~/Dropbox/org/dailylog.org")
	  ;;  "* %U - %^{Activity}  :TIME:")
	  ))

  (setq org-refile-targets '((org-agenda-files :maxlevel . 9)
			     (nil :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-log-refile nil)

  (setq org-log-done 'time)
  (setq org-todo-keywords
	'((sequence
	   "TODO(t)" "WIP(p!)" "WAIT(w@/!)" "SDAY" "|" "DONE(d@)")
	  ))

  (setq org-tag-alist '(
			("bdy" . ?b)
			("mnd" . ?m)
			("fmy" . ?f)
			("hck" . ?h)
			("msc" . ?s)
			("org" . ?o)
			("rlx" . ?r)
			("mop" . ?p)
			))

  (setq org-fontify-done-headline t)
  (custom-set-faces
   '(org-agenda-structure )
   '(org-done ((t (:foreground "grey" :weight normal :strike-through t)))) 
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "grey" :strike-through t)))))

  (setq org-agenda-files '("~/Dropbox/org/wip.org"
			   "~/Dropbox/org/mop.org"
			   "~/Dropbox/org/ref.org"
			   "~/Dropbox/org/dailylog.org"))

  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-todo-ignore-deadlines nil)
  (setq org-agenda-todo-ignore-timestamp nil)
  (setq org-agenda-todo-ignore-with-date nil)
  (setq org-agenda-tags-todo-honor-ignore-options t)
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies nil)
  (setq org-hierachical-todo-statistics t)
  (setq org-log-into-drawer t)
  (setq org-agenda-sorting-strategy
	(quote ((agenda time-up priority-down habit-up
			effort-up category-up)
		(todo priority-down category-up)
		(tags priority-down category-up))))

  (setq org-agenda-custom-commands
	'(("g" "Global agenda and tasks"
	   ((agenda)
	    (tags-todo "mop")
	    (tags-todo "+hck-mop")
	    (tags-todo "mnd")
	    (tags-todo "msc")
	    (tags-todo "fmy")
	    (tags-todo "org")
	    (tags-todo "bdy")
	    (tags-todo "rlx")
	    ))
	  ))

  (setq org-clock-persist t)
  (setq org-clock-persist-file "~/Dropbox/org/org-clock-save.el")

  (org-clock-persistence-insinuate)
  (setq org-clock-into-drawer t)
  (setq org-clock-remove-zero-time-clocks t)
  (setq org-clock-out-when-done t)
  ;; effort
  (setq org-global-properties
	'(("Effort_ALL"."0 0:05 0:10 0:20 0:30 1:00 2:00 3:00")))

  (setq org-archive-save-context-info '(time file ltags itags category))
  (require 'org-datetree)
  (setq org-archive-location "~/Dropbox/org/dailylog.org::date-tree")
  (defadvice org-archive-subtree
    (around org-archive-subtree-to-data-tree activate)
    "org-archive-subtree to date-tree"
    (if
	(string= "date-tree"
		 (org-extract-archive-heading
		  (org-get-local-archive-location)))
	(let* ((dct (decode-time
		     (org-time-string-to-time
		      (cdar (org-entry-properties nil 'special "CLOSED")))))
	       (y (nth 5 dct))
	       (m (nth 4 dct))
	       (d (nth 3 dct))
	       (this-buffer (current-buffer))
	       (location (org-get-local-archive-location))
	       (afile (org-extract-archive-file location))
	       (org-archive-location
		(format "%s::*** %04d-%02d-%02d %s" afile y m d
			(format-time-string "%A" (encode-time 0 0 0 d m y)))))
	  (message "afile=%s" afile)
	  (unless afile
	    (error "Invalid `org-archive-location'"))
	  (save-excursion
	    (switch-to-buffer (find-file-noselect afile))
	    (org-datetree-find-year-create y)
	    (org-datetree-find-month-create y m)
	    (org-datetree-find-day-create y m d)
	    (widen)
	    (switch-to-buffer this-buffer))
	  ad-do-it)
      ad-do-it))

  ;; Org Mobile
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (setq org-mobile-inbox-for-pull "~/Dropbox/org/inbox.org")
  (setq org-mobile-files '("~/Dropbox/org/inbox.org"
                           "~/Dropbox/org/wip.org"
                           "~/Dropbox/org/ref.org"
                           ))

  ;; Org Babel
  (setq org-ditaa-jar-path "~/Dropbox/config/emacs/ditaa.jar")
  (setq org-plantuml-jar-path "~/Dropbox/config/emacs/plantuml.jar")
  ;; active Babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (plantuml . t)
     (emacs-lisp . t)
     ))
  (setq org-confirm-babel-evaluate nil))

(setq org-latex-listings 'minted)
(unless (boundp 'org-latex-default-packages-alist)
  (setq org-latex-default-packages-alist nil))
(add-to-list 'org-latex-default-packages-alist '("" "minted"))
(setq org-latex-hyperref-format "\\ref{%s}")
(setq org-latex-minted-options
           '(("frame" "lines")
             ("fontsize" "\\scriptsize")
             ("linenos" "false")))
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("report"
               "\\documentclass{report}"
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ))
(setq org-latex-to-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
