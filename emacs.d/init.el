(require 'package)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(dolist (package '(
		   better-defaults
		   ))
  (when (not (package-installed-p package))
    (package-install package)))

(global-set-key "\C-z" 'undo)

(setq kill-whole-line t)
