; jmgpena emacs config
; aims to be modular and fast

;; disable interface elements as soon as possible
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; general configuration options
(setq user-full-name "Jorge Pena"
      user-mail-address "jorge@jmgpena.net")
(setq add-log-mailing-address "jorge@jmgpena.net")

;; locale settings
(setq system-time-locale "C") ; system time in coherent format

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
