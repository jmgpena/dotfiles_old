;;; darwin.el --- osx specific config
;;; commentary:
;;; code:
(setenv "TMPDIR" "/tmp")
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'none)
(setq ns-function-modifier 'hyper)
(setq mac-allow-anti-aliasing t)
;; server settings
(setq server-socket-dir (format "~/.emacs.d/server"))

;; This sets the Emacs "PATH" environment variable and the `exec-path`
;; variable to the same value your login shell sees. The reason this
;; is necessary is because of this:
;;
;; http://developer.apple.com/library/mac/#qa/qa1067/_index.html
;;
;; Basically apps launched from Finder inherit their environment from
;; a .plist file rather than the shell environment.

(defun set-exec-path-from-shell-PATH ()
  "Sets the exec-path to the same value used by the user shell"
  (let ((path-from-shell
         (replace-regexp-in-string
          "[[:space:]\n]*$" ""
          (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; call function now
(set-exec-path-from-shell-PATH)

;;; darwin.el ends here
