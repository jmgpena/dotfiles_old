;; php code sniffer for flymake
(setq flymake-phpcs-command "~/.emacs.d/el-get/flymake-phpcs/bin/flymake-phpcs")
(setq flymake-phpcs-standard "~/work/webdev/phpcs-psr")

(setq flymake-phpcs-show-rule t)

(require 'flymake-phpcs)
