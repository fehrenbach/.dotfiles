(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; (require 'diminish)
(require 'bind-key)

;; (menu-bar-mode -1) ; Set by ~/.Xresource
;; (tool-bar-mode -1) ; same
(scroll-bar-mode -1)

(ido-mode t)
(setq ido-enable-flex-matching t)

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key "\C-z" 'undo)
(global-unset-key "\C-x\C-z")

(setq kill-whole-line t)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell t
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
					       "backups"))))


(use-package psc-ide
  :ensure t
  :init
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode))))

(use-package purescript-mode
  :mode "\\.purs\\'"
  :ensure t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "simp" :slant normal :weight normal :height 113 :width normal)))))
