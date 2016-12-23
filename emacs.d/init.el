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
(save-place-mode 1)

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

(use-package stuff
  :ensure nil
  :load-path "~/.emacs.d/lisp"
  :bind ("C-c c" . comment-or-uncomment-region-or-line))

(use-package auctex
  :ensure t
  :mode ("\\.tex\\'" . latex-mode)
  :init (add-hook 'LaTeX-mode-hook #'turn-on-reftex))

(use-package reftex
  :commands turn-on-reftex
  :init (setq reftex-plug-into-AUCTeX t))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook #'global-company-mode))

(use-package flycheck
  :ensure t
  :commands (global-flycheck-mode)
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook 'psc-ide-mode))

(use-package purescript-mode
  :mode ("\\.purs\\'" . purescript-mode)
  :ensure t
  :config
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package merlin
  :ensure t
  :init
  (add-hook 'tuareg-mode-hook
            'merlin-mode))

(use-package tuareg
  :mode ("\\.ml[liy]?\\'" . tuareg-mode)
  :ensure t)

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode))

(use-package links-mode
  :ensure nil
  :load-path "~/src/links/"
  :mode ("\\.links\\'" . links-mode))

(use-package magit
  :ensure t
  :bind ("C-c s" . magit-status))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;;; ProofGeneral ;; proofgeneral package on AUR
(load "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site")
;;; TODO move to use-package?

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Okular")
     (output-html "xdg-open"))))
 '(completion-ignored-extensions
   (quote
    (".agdai .run.xml" ".bcf" ".hi" ".cmti" ".cmt" ".annot" ".cmi" ".cmxa" ".cma" ".cmx" ".cmo" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".log" ".out" ".synctex.gz" ".pdf" ".fdb_latexmk" ".dvi" ".fls" ".spl" ".glob" ".v.d" ".vo")))
 '(inhibit-startup-screen t)
 '(package-selected-packages (quote (magit use-package)))
 '(safe-local-variable-values (quote ((TeX-master . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "simp" :slant normal :weight normal :height 113 :width normal))))
 '(font-lock-comment-face ((t (:foreground "dark gray")))))
