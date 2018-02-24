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
  :init (progn
          (setq TeX-source-correlate-method 'synctex
                TeX-PDF-mode t
                TeX-view-program-selection '((output-pdf "Okular"))
                TeX-view-program-list '(("Okular" "okular --unique --noraise %u")))
          (add-hook 'LaTeX-mode-hook (lambda ()
                                       (add-to-list 'TeX-expand-list
                                                    '("%u" (lambda ()
                                                             (concat
                                                              "file://"
                                                              (expand-file-name (funcall file (TeX-output-extension) t)
                                                                                (file-name-directory (TeX-master-file)))
                                                              "#src:"
                                                              (TeX-current-line)
                                                              (expand-file-name (TeX-master-directory))
                                                              "./"
                                                              (TeX-current-file-name-master-relative)))))))
          (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
          (add-hook 'LaTeX-mode-hook #'server-start)))

(use-package reftex
  :commands turn-on-reftex
  :init (setq reftex-plug-into-AUCTeX t))

(use-package company
  :ensure t
  :init (add-hook 'after-init-hook #'global-company-mode))

(use-package flycheck
  :ensure t
  ;; :commands (global-flycheck-mode)
  ;; :init (add-hook 'after-init-hook #'global-flycheck-mode)
  )

;; Used by psc-ide to suggest project root directory
(use-package projectile
  :ensure t)

(use-package purescript-mode
  :mode ("\\.purs\\'" . purescript-mode)
  :ensure t
  :config
  (add-hook 'purescript-mode-hook 'turn-on-purescript-indentation))

(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook 'psc-ide-mode))

(use-package merlin
  :ensure t
  :init
  (add-hook 'tuareg-mode-hook
            'merlin-mode))

(use-package tuareg
  :mode ("\\.ml[liy]?\\'" . tuareg-mode)
  :ensure t)

(use-package intero
  :ensure t
  :commands intero-mode)

(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'" . haskell-mode)
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

(use-package idris-mode
  :ensure t
  :mode ("\\.idr\\'" . idris-mode))

(use-package links-mode
  :ensure nil
  :load-path "~/src/links/"
  :mode ("\\.links\\'" . links-mode))

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger\\'" . ledger-mode))

(use-package magit
  :ensure t
  :bind ("C-c s" . magit-status))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

;;; ProofGeneral ;; proofgeneral package on AUR
(load "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site")
;;; TODO move to use-package?

(use-package company-coq
  :ensure t
  :defer t ;; not sure this is harmful, needed, or useful..
  :init (add-hook 'coq-mode-hook #'company-coq-mode))

(use-package yaml-mode
  :ensure t
  :mode "\\.yaml\\'")

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(LaTeX-verbatim-environments (quote ("verbatim" "verbatim*" "lstlisting")))
 '(TeX-master nil)
 '(TeX-save-query nil)
 '(completion-ignored-extensions
   (quote
    (".agdai .run.xml" ".bcf" ".hi" ".cb" ".cb2" ".cmti" ".cmt" ".annot" ".cmi" ".cmxa" ".cma" ".cmx" ".cmo" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".log" ".out" ".synctex.gz" ".pdf" ".fdb_latexmk" ".dvi" ".fls" ".spl" ".glob" ".v.d" ".vo" ".ibc")))
 '(ido-enable-flex-matching t)
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/Documents/journal.org")))
 '(package-selected-packages
   (quote
    (intero yaml-mode web-mode use-package tuareg purescript-mode psc-ide projectile multiple-cursors merlin markdown-mode magit ledger-mode idris-mode hledger-mode haskell-mode company-coq auctex)))
 '(proof-splash-enable nil)
 '(safe-local-variable-values
   (quote
    ((reftex-default-bibliography "bib.bib")
     (TeX-master)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "simp" :slant normal :weight normal :height 113 :width normal))))
 '(font-lock-comment-face ((t (:foreground "dark gray")))))
