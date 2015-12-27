;; ESS is not in MELPA or any of the other repos.
;; Install AUR package `emacs-ess`.
;(setq load-path (cons "/usr/share/emacs/site-lisp/ess" load-path))
;(require 'ess-site)

;;; Proofgeneral from AUR package proofgeneral
(load-file "/usr/share/emacs/site-lisp/ProofGeneral/generic/proof-site.el")

;; directory for emacs lisp not in any repository
(add-to-list 'load-path "~/.emacs.d/local/")


(add-to-list 'custom-theme-load-path "~/.emacs.d/local/emacs-color-theme-solarized/")
(load-theme 'solarized t)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
;(package-list-packages)
(package-initialize)

(dolist (package '(ace-jump-mode
                   aggressive-indent
                   auctex
                   better-defaults
                   cider ;; buggy?!
                   circe
                   clojure-mode
                   company
                   ghc
                   haskell-mode
                   helm ;; buggy?!
                   hi2 ;; haskell indentation 2nd try
                   idris-mode
                   magit ;; buggy?!
                   markdown-mode
                   multiple-cursors
                   paredit
                   tuareg ; ocaml mode

                   ;; dependencies of lean-mode
                   dash-functional
                   f
                   lua-mode
                   flycheck
                   ))
  (when (not (package-installed-p package))
    (package-install package)))

(global-set-key "\C-z" 'undo)
(global-unset-key "\C-x\C-z")

(setq kill-whole-line t)

;;(global-visual-line-mode 1)
(define-key global-map (kbd "C-c C-SPC") 'ace-jump-mode)

;(add-hook 'after-init-hook 'global-company-mode)


(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  ;; stolen from http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key (kbd "C-a")
                'smarter-move-beginning-of-line)

;; Stolen from here
;; http://stackoverflow.com/a/9697222
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)
        (next-logical-line)))

(global-set-key (kbd "C-c c") 'comment-or-uncomment-region-or-line)
(global-set-key (kbd "C-c s") 'magit-status)

(setq magit-last-seen-setup-instructions "1.4.0")

;;; flyspell
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

;; ;;; helm
(require 'helm-config)
(helm-mode 1)

;;; org-mode
(global-set-key (kbd "C-c a") 'org-agenda)


;;; mu4e
(require 'mu4e)
(setq
 mu4e-change-filenames-when-moving t
 mu4e-drafts-folder "/Drafts"
 mu4e-headers-include-related t
 mu4e-headers-skip-duplicates t
 mu4e-maildir "~/Mail"
 ;; mu4e-refile-folder "/All Mail" ; see below
 mu4e-sent-folder "/Sent Mail"
 mu4e-trash-folder "/Bin"
 mu4e-update-interval 300
 mu4e-user-mail-address-list '("stefan.fehrenbach@gmail.com" "stefan.fehrenbach@ed.ac.uk" "s1437649@sms.ed.ac.uk"))

(add-to-list 'mu4e-bookmarks
             '("maildir:/INBOX" "Inbox" ?i))

(setq mu4e-refile-folder
      (lambda (msg)
        (cond
         ;; messages sent by me go to the sent folder
         ((find-if (lambda (addr) (mu4e-message-contact-field-matches msg :from addr))
                   mu4e-user-mail-address-list)
          mu4e-sent-folder)
         ;; everything else goes to /archive
         ;; important to have a catch-all at the end!
         (t "/All Mail"))))

(global-set-key (kbd "C-c m") 'mu4e)

;;; Circe
(setq circe-network-options
      '(("Freenode"
         :nick "fehrenbach"
         :channels ("#clojure" "#haskell" "#idris"))
        ("mozilla"
         :host "irc.mozilla.org"
         :port 6697
         :nickserv-mask "^NickServ!NickServ@services\\.$"
         :tls t
         :nick "fehrenbach"
         :channels ("#rust"))))

(setq circe-reduce-lurker-spam t)

(setq
 lui-time-stamp-position 'right-margin
 lui-time-stamp-format "%H:%M"
 lui-fill-type nil)

(add-hook 'lui-mode-hook 'my-lui-setup)
(defun my-lui-setup ()
  (setq
   fringes-outside-margins t
   right-margin-width 5
   word-wrap t
   wrap-prefix "    "))

(require 'circe-color-nicks)
(enable-circe-color-nicks)


;;; AUCTeX
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'variable-pitch-mode)
;; SyncTeX stuff: http://www.kevindemarco.com/2013/04/24/emacs-auctex-synctex-okular-on-ubuntu-12-04/
(setq TeX-view-program-selection
 '((output-pdf "Okular")))
(setq TeX-view-program-list
 '(("Okular" "okular --unique %o#src:%n%b")))
;; In Okular set editor to: emacsclient -a emacs --no-wait +%l %f
;; Use shift + left click in Okular to jump to the correct line in Emacs.
(add-hook 'LaTeX-mode-hook 'server-start)
;; Enable synctex correlation
(setq TeX-source-correlate-method 'synctex)

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)


;;; Agda
(when (executable-find "agda-mode")
  (load-file (let ((coding-system-for-read 'utf-8))
                  (shell-command-to-string "agda-mode locate"))))


;;; Clojure
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)


;;; OCaml
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'merlin)
(require 'company)

(add-hook 'tuareg-mode-hook 'merlin-mode t)
(add-to-list 'company-backends 'merlin-company-backend)
(add-hook 'merlin-mode-hook 'company-mode)
;; Enable auto-complete
(setq merlin-use-auto-complete-mode 'easy)
;; Use opam switch to lookup ocamlmerlin binary
(setq merlin-command 'opam)


;;; Haskell

;; structured haskell mode
;; (installed in cabal sandbox in ~/opt, binary sym-linked to ~/bin)
;(add-to-list 'load-path "/home/stefan/opt/structured-haskell-mode/elisp")
;(require 'shm)

;(set-face-background 'shm-current-face "#02b1f2")

;(add-hook 'haskell-mode-hook 'structured-haskell-mode)



;; haskell-mode
;; (eval-after-load 'haskell-mode '(progn
;;   (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;;   (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;;   (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
;;   (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))
;; (eval-after-load 'haskell-cabal '(progn
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
;; ;; ghc-mod
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (add-hook 'haskell-mode-hook 'turn-on-hi2)

;;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode))
                              auto-mode-alist))

;;; Links
(add-to-list 'load-path "~/src/links/")
(require 'links-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-command "latex -synctex=1")
 '(TeX-command-extra-options "-shell-escape")
 '(clojure-defun-indents (quote (match)))
 '(colon-double-space nil)
 '(compilation-scroll-output (quote first-error))
 '(completion-ignored-extensions
   (quote
    (".agdai .run.xml" ".bcf" ".hi" ".cmti" ".cmt" ".annot" ".cmi" ".cmxa" ".cma" ".cmx" ".cmo" ".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo" ".log" ".out" ".synctex.gz" ".pdf")))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(inhibit-startup-screen t)
 '(links-cli-arguments "--config=config")
 '(org-agenda-files (quote ("~/org/uni.org" "~/org/india2015.org")))
 '(sentence-end-double-space nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(visual-line-fringe-indicators (quote (nil right-curly-arrow))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
