(let ((path (expand-file-name "~/.emacs.d/elisp")))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path t)))


;و;(when (>= emacs-major-version 24)
(require 'package)

;; ;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;; ;;  )

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/"))
      tls-checktrust t
      tls-program '("gnutls-cli --x509cafile %t -p %p %h")
      gnutls-verify-error t)
(package-initialize)

(setq use-package-always-ensure nil)

(unless (require 'use-package nil t)
  (if (not (yes-or-no-p (concat "Refresh packages, install use-package and"
				" other packages used by init file? ")))
      (error "you need to install use-package first")
    (package-refresh-contents)
    (package-install 'use-package)
    (require 'use-package)
    (setq use-package-always-ensure t)))


;;; ### Workaround for security vulnerability in Emacs >= 21.1 and < 25.3 ###
;;;
;;;  See [Changes in Emacs 25.3](https://www.gnu.org/software/emacs/news/NEWS.25.3)

(eval-after-load "enriched"
    '(defun enriched-decode-display-prop (start end &optional param)
       (list start end)))

(use-package uuidgen
  :demand)

(use-package helm-descbinds
  :demand
  :config
  (helm-descbinds-mode))

;;; List of personal key bindings

(global-set-key (kbd "C-c h b") 'describe-personal-keybindings)

;;; Install [ripgrep](https://github.com/BurntSushi/ripgrep) (rg) and
;;; add

;; (use-package helm-rg
;;   :bind
;;   (("C-c r" . helm-rg)))

;;; File explorer sidebar

(use-package treemacs
  :bind
  (("C-c t" . treemacs)
   ("s-a" . treemacs)))

;;; Cycle through buffers' history

(use-package buffer-flip
  :bind
  (("s-o" . buffer-flip)
   :map buffer-flip-map
   ("s-o" . buffer-flip-forward)
   ("s-O" . buffer-flip-backward)
   ("C-g" . buffer-flip-abort)))

;;; [fzf](https://github.com/junegunn/fzf) and
;;; [lcd](https://github.com/lukpank/lcd) for finding files and
;;; directories

(use-package fzf
  :commands fzf/start
  :bind
  (("C-c f" . fzf)
   ("C-c D" . my-lcd)))

(defun my-lcd ()
  (interactive)
  (fzf/start default-directory
             (fzf/grep-cmd "lcd" "-l %s")))

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (use-package dired+
;;              :load-path "~/.emacs.d/elisp/dired+")

;; (when (memq window-system '(mac ns))
;; ;; NB: the shell path is the same one you get opening a shell, i.e. in the case of bash it runs ~/.bash_profile etc.
;;   (message (make-string "window-system:" window-system))
;;   (exec-path-from-shell-initialize)
;; )

  ;; (message (make-string "window-system:" window-system))
  (exec-path-from-shell-initialize)


;; (setq exec-path
;;       '(
;; 	"/Users/gar/bin"
;; 	"/usr/local/bin"
;; 	"/usr/bin"
;; 	"/bin"
;; 	"/Users/gar/.cabal/bin"
;; 	"/usr/texbin"
;; 	"/Applications/Julia/Contents/Resources/julia/bin"))

	;; "/usr/X11R6/bin" ;; ???
	;; "/opt/local/sbin")) ;; got rid of MacPorts

;; (message (getenv "PATH"))

;; (load "my-abbrevs")

;; dart
;; (add-to-list 'auto-mode-alist '("\.dart$" . java-mode))

;; c++ templates
(add-to-list 'auto-mode-alist '("\.tcc$" . c++-mode))

;; (load "hava-mode.el")
;; (add-to-list 'auto-mode-alist '("\.hava$" . hava-mode))

;; ocaml - https://dev.realworldocaml.org/install.html
;;(add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

;; (load "edn-mode.el")
(require 'edn)
(add-to-list 'auto-mode-alist '("\.edn$" . edn-mode))

;; xql
;; (load "xquery-mode.el")
(require 'xquery-mode)
(add-to-list 'auto-mode-alist '("\.xql$" . xquery-mode))
(add-to-list 'auto-mode-alist '("\.xqm$" . xquery-mode))
(add-to-list 'auto-mode-alist '("\.xq$" . xquery-mode))

;; bazel
(load "bazel-mode.el")
;; bazel-mode doesn't do indenting; use python mode
(add-to-list 'auto-mode-alist '("\\.bazelrc" . bazel-mode))
(add-to-list 'auto-mode-alist '("BUILD$" . bazel-mode))
(add-to-list 'auto-mode-alist '("/BUILD\\(\\..*\\)?\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE$" . bazel-mode))
(add-to-list 'auto-mode-alist '("/WORKSPACE\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("\\.\\(BUILD\\|WORKSPACE\\|bzl\\)\\'" . bazel-mode))


;; Remove all annoying modes from auto mode lists

(defun replace-alist-mode (alist oldmode newmode)
  (dolist (aitem alist)
    (if (eq (cdr aitem) oldmode)
    (setcdr aitem newmode))))

;; (add-to-list 'load-path "~/.emacs.d/ProofGeneral/generic/")
;; (add-to-list 'load-path "~/.emacs.d/elpa/proof-general-4.4/generic/")
;; (replace-alist-mode auto-mode-alist 'verilog-mode 'proof-general-mode)

;; PROOF GENERAL stuff
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives
               (cons "melpa" (concat proto "://melpa.org/packages/")) t))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))

;; Install ProofGeneral
(use-package proof-general
  :ensure t
  :mode ("\\.v\\'" . coq-mode)
  :init
  (setq coq-prog-name "coqtop")         ; or "C:/Coq/bin/coqtop.exe"…
)

;; Install company-coq
(use-package company-coq
  :ensure t
  :hook
  (coq-mode . company-coq-mode)
  :init
  (setq company-coq-disabled-features '(hello prettify-symbols)))

;; (load "~/.emacs.d/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 4)
 '(change-log-default-name "CHANGELOG.md")
 '(cider-boot-parameters "")
 '(latex-run-command "lualatex")
 '(magit-commit-arguments (quote ("--signoff")))
 '(package-selected-packages
   (quote
    (dune rust-mode go-mode bazel-mode utop tuareg iedit merlin proof-general uuidgen buffer-flip fzf helm-rg treemacs helm-descbinds use-package kotlin-mode company-coq helm-ls-git helm-make helm-system-packages helm-unicode xquery-tool ess-smart-equals ess-smart-underscore yaml-mode yang-mode expand-region rnc-mode helm-gtags helm-cider helm ggtags dart-mode yas-jit xcscope solarized-theme smartparens rainbow-delimiters paredit mustache-mode markdown-mode magit ido-vertical-mode ido-ubiquitous ido-at-point groovy-mode gradle-mode exec-path-from-shell ess edn ctags company-math color-theme-solarized color-theme-sanityinc-solarized clojurescript-mode clojure-snippets clojure-quick-repls auto-complete-nxml adoc-mode)))
 '(rng-schema-locating-files
   (quote
    ("schemas.xml" "/Applications/Emacs.app/Contents/Resources/etc/schema/schemas.xml" "/usr/local/xml/schema/dita/schemas.xml"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (setq font-lock-maximum-size 1000000)
(setq visible-bell 1)
(setq shell-file-name "bash")
(setq shell-command-switch "-c")
;(setq case-fold-search nil)
(setq explicit-shell-file-name shell-file-name)
(setenv "SHELL" shell-file-name)
(setq explicit-sh-args '("-login" "-i"))
(if (boundp 'w32-quote-process-args)
    (setq w32-quote-process-args ?\")) ;; Include only for MS Windows. "

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(show-paren-mode t)
;;(setq show-paren-style 'parenthesis) ; highlight just brackets
(setq show-paren-style 'expression) ; highlight entire bracket expression

(defadvice show-paren-function
  (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
	 (matching-text (and cb
			     (char-equal (char-syntax cb) ?\) )
			     (blink-matching-open))))
    (when matching-text (message matching-text))))

;; Align with spaces only
(setq-default indent-tabs-mode nil)
(defadvice align-regexp (around align-regexp-with-spaces)
  "Never use tabs for alignment."
  (let ((indent-tabs-mode nil))
    ad-do-it))
(ad-activate 'align-regexp)

(setq-default show-trailing-whitespace t)
(setq transient-mark-mode t)
(global-hl-line-mode 1)
;; To customize the background color
;; (set-face-background 'hl-line "#677")
(global-font-lock-mode 1)

(setq user-full-name "Gregg Reynolds")
;(setq user-mail-address "gar@mobileink.com")

(defalias 'qrr 'query-replace-regexp)

(setq auto-save-interval 125)
(setq make-backup-files         t ;backup my files
      backup-by-copying         t ;don't clobber symlinks
      delete-old-versions       t
      kept-new-versions         6
      kept-old-versions         2
      version-control           t ;use versioned backups
      vc-make-backup-files      t ;make backups for cvs projects
      vc-follow-symlinks        t)
;; Backups to central location
;(setq backup-directory-alist `(("." ,@(concat user-emacs-directory "backups"))))


(setq kept-old-versions 1000000)
;; (toggle-menu-bar-mode-from-frame)
;; (defun toggle-fullscreen ()
;;   (interactive)
;;   (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
;;                                            nil
;;                                            'fullboth)))
;; (global-set-key [(meta return)] 'toggle-fullscreen)

;; (require 'xcscope)
;; (cscope-setup)

(setq-default c-basic-offset 4)

(require 'recentf)

;; enable recent files mode.
(recentf-mode t)
;; 50 files ought to be enough.
(setq recentf-max-saved-items 50)

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
        (message "Aborting")))

;; magit
(global-set-key (kbd "C-c g") 'magit-status)

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(global-set-key (kbd "C-c h") 'helm-mini)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

; Force unix and utf-8
(if (> emacs-major-version 21)
    (progn
      (set-language-environment "utf-8")
      ;; (set-input-method "latin-1-postfix")
      (prefer-coding-system 'utf-8)
      (setq locale-coding-system 'utf-8)
      (set-terminal-coding-system 'utf-8)
      (set-keyboard-coding-system 'utf-8)
      (set-selection-coding-system 'utf-8)
      (setq file-name-coding-system 'utf-8)
      (setq coding-system-for-read 'utf-8)
      (setq coding-system-for-write 'utf-8)
      (modify-coding-system-alist 'file "\\.txt\\'" 'utf-8)
      (modify-coding-system-alist 'file "\\.el\\'" 'utf-8)
      (require 'un-define "un-define" t)
      (set-buffer-file-coding-system 'utf-8 'utf-8-unix)
      (set-default buffer-file-coding-system 'utf-8-unix)
      (set-default-coding-systems 'utf-8-unix)
      (prefer-coding-system 'utf-8-unix)
      (set-default buffer-file-coding-system 'utf-8-unix)
      ))

(setq inhibit-eol-conversion t)

;; Uncomment if using abbreviations
;; (abbrev-mode t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;(autoload 'toggle-show-trailing-whitespace "utils" "utils" t)
(defun toggle-show-trailing-whitespace ()
  (interactive)
  (if show-trailing-whitespace
      (setq show-trailing-whitespace nil)
    (setq show-trailing-whitespace t))
  (redraw-display))

(defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
        (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))


(setenv "JAVA_HOME"
        "/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.0.2/Contents/Home")

 (add-hook 'c-mode-common-hook
	   (lambda ()
	     (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	       (ggtags-mode 1))))

(add-hook 'c-mode-hook (lambda () comment-style (quote multi-line)))

;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
;; (color-theme-solarized-dark)

;; http://www.emacswiki.org/emacs/RainbowDelimiters
;;(require 'rainbow-delimiters)
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; (global-rainbow-delimiters-mode)

(require 'tramp)
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   CUSTOMIZATIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://ergoemacs.org/emacs/emacs_buffer_management.html
(defalias 'list-buffers 'ibuffer)
;; make buffer switch command show suggestions
;; http://ergoemacs.org/emacs/emacs_buffer_switching.html
;(require 'ido)
(ido-mode t)
;; http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html
;;;;;todo ...


(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program nil)

(add-hook 'dired-load-hook
	  (lambda ()
	    (load "dired-x")
	    ;; Set dired-x global variables here.  For example:
	    ;; (setq dired-guess-shell-gnutar "gtar")
	    ;; (setq dired-x-hands-off-my-keys nil)
	    (setq dired-omit-files
		  (concat dired-omit-files "\\|^\\..+$"))
	    ))
(add-hook 'dired-mode-hook
	  (lambda ()
	    ;; Set dired-x buffer-local variables here.  For example:
	    (dired-omit-mode 1)
	    (print dired-omit-extensions)
	    (delete ".pdf" dired-omit-extensions)))
;;note: Info file is wrong:    (setq dired-omit-files-p t)

;;(require 'dired+)
;; (require 'color-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   MODES SETUP
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Tex/Latex
;;

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(add-hook 'BibTeX-mode-hook (lambda () (setq fill-prefix nil)))

;; (require 'smartparens-config)

;;  Clojure
;;(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.cljd$" . clojure-mode))
;;(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

;; (load (expand-file-name "~/.emacs.d/elisp/migae"))
;; (add-hook 'clojure-mode-hook
;;       (lambda ()
;;         (set (make-local-variable 'parse-sexp-ignore-comments) t) ))
;; (add-hook 'clojure-mode-hook
;; 	  (lambda ()
;; 	    (message "clojure-mode-hook migae: %s" (buffer-file-name))
;; 	    (define-key clojure-mode-map (kbd "C-x C-s") 'migae-save-clj-buffer)))

;; (add-hook 'js-mode-hook
;; 	  (lambda ()
;; 	    (message "js-mode-hook migae: %s" (buffer-file-name))
;; 	    (define-key js-mode-map (kbd "C-x C-s") 'migae-save-js-buffer)))

;; (add-hook 'nxml-mode-hook
;; 	  (lambda ()
;; 	    (message "nxml-mode-hook migae: %s" (buffer-file-name))
;; 	    (define-key nxml-mode-map (kbd "C-x C-s") 'migae-save-nxml-buffer)))

;; (add-hook 'html-mode-hook
;; 	  (lambda ()
;; 	    (message "html-mode-hook migae: %s" (buffer-file-name))
;; 	    (define-key html-mode-map (kbd "C-x C-s") 'migae-save-html-buffer)))

;;(add-hook 'ruby-mode-hook (lambda () (local-set-key 'f1 'ri)))

;; (define-clojure-indent
;;   (defroutes 'defun)
;;   (GET 2)
;;   (POST 2)
;;   (PUT 2)
;;   (DELETE 2)
;;   (HEAD 2)
;;   (ANY 2)
;;   (context 2))

;;
(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)
;;(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;(setq nrepl-hide-special-buffers t)

(add-to-list 'load-path "~/.emacs.d/snippets")
(require 'yasnippet)
(yas-global-mode 1)

;; (require 'arb-minor-mode)

;; ;; Remove Yasnippet's default tab key binding
;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; ;; Set Yasnippet's key binding to shift+tab
;; (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
;; ;; Alternatively use Control-c + tab
;; (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)

;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)
;; (global-auto-complete-mode t)
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")

;;(add-to-list 'load-path "~/.emacs.d/elisp/doc-mode-1.1")
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))
(autoload 'adoc-mode "adoc-mode")
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode nil)))

;;(load "ess-autoloads")
;;(require 'ess-r-mode)
(require 'ess-site)

;;
;; ESS julia language
;; https://github.com/emacs-ess/ESS/wiki/Julia
;; excecutable file
;; (setq inferior-julia-program-name
;;       "/Applications/Julia-0.2.1.app/Contents/Resources/julia/bin/julia-basic")
;; nope:  "/Applications/Julia/Contents/Resources/julia/bin/julia")
;; put julia/bin on PATH instead?

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   nxml schemata

(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))

(defun set-nxml-schemas ()
  (setq rng-schema-locating-files-default
	(cons
	 "/usr/local/xml/schema/dita/schemas.xml"
	 rng-schema-locating-files-default)))

;; (setq rng-schema-locating-files
;;       (cons (car rng-schema-locating-files-default)
;; 	    (append '("~/.nvdl.d/schemas.xml")
;; 		    (cdr rng-schema-locating-files-default))))

(add-hook 'nxml-mode-hook 'set-nxml-schemas)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  dita
(add-to-list 'auto-mode-alist '("\\.ditamap$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.dita$" . nxml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   netrexx
(autoload 'netrexx-mode "netrexx-mode" "NETREXX mode" nil t)
(setq auto-mode-alist
      (append
       (list (cons "\\.nrx$"  'netrexx-mode)
	     (cons "\\.nry$"  'netrexx-mode)
	     )
       auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

(load "mykeys")

;;(require 'proof-site)

;; (setenv "ATSHOME" "/usr/local/lib/ats2-postiats-0.1.2")
;; (setenv "PATSHOME" "/usr/local/lib/ats2-postiats-0.1.2")

;; (require 'ats2-flymake)


(set-face-attribute 'default nil :foundry "apple" :family "Source Code Pro")

(setq default-directory (expand-file-name "~/"))
(setq command-line-default-directory "~/")
(setq inhibit-splash-screen t)

;; (add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))

(when (eq system-type 'darwin)
  (message "system-type: darwin")

  ;; default Latin font (e.g. Consolas)
  (set-face-attribute 'default t :family "Kawkab Mono")

  (set-frame-font "Kawkab Mono")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly. 
  (set-face-attribute 'default nil :height 155)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  ;; (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; you may want to add different for other charset in this way.
  )

(setq initial-frame-alist
       '((left . 80)
	 (top . 50)
	 (height . 30)
	 (width . 160)))

(setq default-frame-alist
       '((left . 80)
	 (top . 50)
	 (height . 30)
	 (width . 160)))

(split-window-right)

;; for nshap codebooks
;; (defun wrap-qtext (b e) ;; txt)
;;   "simple wrapper"
;;   (interactive "r")
;;   (save-restriction
;;     (narrow-to-region b e)
;;     (goto-char (point-min))
;;     ;; (insert txt)
;;     (insert "<qtext>")
;;     (goto-char (point-max))
;;     (insert "</qtext>")))

;; (global-set-key (kbd "C-c M-q") 'wrap-qtext)

;; (defun wrap-desc (b e) ;; txt)
;;   "wrap description"
;;   (interactive "r")
;;   (save-restriction
;;     (narrow-to-region b e)
;;     (goto-char (point-min))
;;     ;; (insert txt)
;;     (insert "<desc>")
;;     (goto-char (point-max))
;;     (insert "</desc>")))

;; (global-set-key (kbd "C-c M-d") 'wrap-desc)

;; (defun wrap-note (b e) ;; txt)
;;   "wrap note"
;;   (interactive "r")
;;   (save-restriction
;;     (narrow-to-region b e)
;;     (goto-char (point-min))
;;     ;; (insert txt)
;;     (insert "<note>")
;;     (goto-char (point-max))
;;     (insert "</note>")))

;; (global-set-key (kbd "C-c M-n") 'wrap-note)

;; (defun wrap-skip (b e) ;; txt)
;;   "wrap skip note"
;;   (interactive "r")
;;   (save-restriction
;;     (narrow-to-region b e)
;;     (goto-char (point-min))
;;     ;; (insert txt)
;;     (insert "<skip-if>")
;;     (goto-char (point-max))
;;     (insert "</skip-if>")))

;; (global-set-key (kbd "C-c M-k") 'wrap-skip)

;; (defun quex-loc ;; (b e)
;;   "insert quex loc elt"
;;   (interactive "r")
;;   (save-restriction
;;     ;; (narrow-to-region b e)
;;     ;; (goto-char (point-min))
;;     ;; (insert txt)
;;     (insert "<quex id='' list-label=''>")))

(global-set-key (kbd "C-c M-k") 'wrap-skip)

(global-set-key (kbd "C-c M-k") 'yas-insert-snippet)

(defun unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the reverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000))
    (fill-paragraph nil)))
