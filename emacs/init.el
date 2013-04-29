; jmgpena emacs config
; aims to be modular and fast

;; disable interface elements as soon as possible
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; general configuration options
(setq user-full-name "Jorge Pena"
      user-mail-address "jorge@jmgpena.net")
(setq add-log-mailing-address "jorge@jmgpena.net")

;; locale settings and coding
(setq system-time-locale "C") ; system time in coherent format
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

;; Who has time to type so many letters?
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

;; defaults
(set-default 'indent-tabs-mode nil)
(setq tab-width 4)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

;; other defaults
(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face spaces tabs newline space-mark tab-mark
                              newline-mark lines-tail trailing)
      whitespace-display-mappings '(
                                    (space-mark 32 [183] [46])
                                    (newline-mark 10 [182 10])
                                    (tab-mark 9 [9655 9] [92 9])
                                    )
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain)
