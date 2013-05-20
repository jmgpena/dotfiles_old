; jmgpena emacs config

;; disable interface elements as soon as possible
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-message t) ; no splash screen

;; user configuration options
(setq user-full-name "Jorge Pena"
      user-mail-address "jorge@jmgpena.net")

;; locale settings and coding
(setq system-time-locale "C") ; system time in coherent format
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(set-language-environment "utf-8")

;; defaults
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default indicate-empty-lines t)
(setq visible-bell t)
(setq mouse-yank-at-point t)
(setq-default ediff-window-setup-function 'ediff-setup-windows-plain)
(line-number-mode 1)   ; show line numberes on modeline
(column-number-mode 1) ; and also columns
(global-hl-line-mode) ; highlight current line
;;(global-linum-mode 1) ; show line numbers on the left
;; if there are no unsaved changes revert buffers automagically
(global-auto-revert-mode 1)

;; navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; simplify prompts
(defalias 'yes-or-no-p 'y-or-n-p)

;; Backup files
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; graphical interface specific settings
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

; whitespace settings
(require 'whitespace)
(setq whitespace-style '(face spaces tabs newline space-mark tab-mark
                              newline-mark lines-tail trailing))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46])
        (newline-mark 10 [182 10])
        (tab-mark 9 [9655 9] [92 9])))
(setq whitespace-line-column 80)
(global-whitespace-mode t) ; use everywhere
(setq whitespace-global-modes '(c-mode c++-mode emacs-lisp-mode))

;; utility functions
(defun fullpath-relative-to-current-file (file-relative-path)
  "Returns the full path of FILE-RELATIVE-PATH, relative to file location where
this function is called.

Example: If you have this line
 (fullpath-relative-to-current-file \"../xyz.el\")
in the file at
 /home/mary/emacs/emacs_lib.el
then the return value is
 /home/mary/xyz.el
Regardless how or where emacs_lib.el is called.

This function solves 2 problems.

 ① If you have file A, that calls the `load' on a file at B, and
B calls “load” on file C using a relative path, then Emacs will
complain about unable to find C. Because, emacs does not switch
current directory with “load”.

 To solve this problem, when your code only knows the relative
path of another file C, you can use the variable `load-file-name'
to get the current file's full path, then use that with the
relative path to get a full path of the file you are interested.

 ② To know the current file's full path, emacs has 2 ways:
`load-file-name' and `buffer-file-name'. If the file is loaded
by “load”, then load-file-name works but buffer-file-name
doesn't. If the file is called by `eval-buffer', then
load-file-name is nil. You want to be able to get the current
file's full path regardless the file is run by “load” or
interactively by “eval-buffer”.

By Xah Lee"
  (concat (file-name-directory (or load-file-name buffer-file-name))
          file-relative-path))

;; el-get configuration
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(setq el-get-user-package-directory
      (fullpath-relative-to-current-file "init-pkg"))

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(setq el-get-sources
      '((:name el-get)
        (:name dash
               :description "A modern list api for Emacs. No 'cl required."
               :type github
               :pkgname "magnars/dash.el")
        (:name smartparens
               :description "Autoinsert pairs of defined brackets and wrap regions"
               :type github
               :pkgname "Fuco1/smartparens"
               :depends dash)
        (:name smex)
        (:name org-plus-contrib
               :type elpa :repo ("org" . "http://orgmode.org/elpa/"))
        (:name magit :type elpa)
        (:name sass-mode)
        (:name php-mode)
        (:name less-css-mode
               :type github
               :pkgname "purcell/less-css-mode")
        (:name lilypond-mode
               :type github
               :pkgname "jmgpena/lilypond-mode"
               :prepare (load "lilypond-init"))
        (:name solarized-theme
               :type github
               :pkgname "sellout/emacs-color-theme-solarized"
               :description "Solarized themes for Emacs"
               :prepare (add-to-list 'custom-theme-load-path default-directory))
        ))

(setq my:el-get-packages (mapcar 'el-get-source-name el-get-sources))

(el-get 'sync my:el-get-packages)

;; ido mode
(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last")
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point 'guess)
(setq ido-show-dot-for-dired t)
(setq ido-use-virtual-buffers t)
(setq ido-handle-duplicate-virtual-buffers 2)
(setq ido-max-prospects 10)

; switch buffer
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; system specific configs
(setq jmgpena-sysinit-file
      (concat (fullpath-relative-to-current-file "opt/")
              (symbol-name system-type) ".el"))
(when (file-exists-p jmgpena-sysinit-file)
  (load jmgpena-sysinit-file))
