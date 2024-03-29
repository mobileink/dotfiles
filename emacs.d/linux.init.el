;; https://so.nwalsh.com/2020/02/29/dot-emacs

;; lexical binding: https://nullprogram.com/blog/2016/12/22/
;; emacs 27.1: lexical-binding is the default
;;; init.el --- -*- lexical-binding: t -*-


(when (and (equal emacs-version "27.2")
           (eql system-type 'darwin))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq gc-cons-threshold 100000000)

(setq package-enable-at-startup nil)
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
(setq site-run-file nil)

(let ((path (expand-file-name "~/.emacs.d/elisp")))
  (if (file-accessible-directory-p path)
      (add-to-list 'load-path path t)))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;  Effectively replace use-package with straight-use-package
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#integration-with-use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Tell straight to use ssh by default, instead of https.
;; (setq straight-vc-git-default-protocol 'ssh)

(setq inhibit-startup-message t)
;; WTF? If I don't define this, I get weird warnings when byte compiling
(setq warning-suppress-types nil)


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


;; Updates from [[https://github.com/alhassy/emacs.d][alhassy]].
;; Move auto save and backup files out of the way
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; (setq auto-save-file-name-transforms
;;       ((".*" ~/.emacs.d/backups t)))
;; Silently delete execess backup versions
(setq delete-old-versions t)
;; Only keep the last 100 backups of a file.
(setq kept-old-versions 100)
;; Even version controlled files get to be backed up.
(setq vc-make-backup-files t)
;; Use version numbers for backup files.
(setq version-control t)

;; Save the place in files
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")

;; Make some buffers immortal
(defun ndw-immortal-buffers ()
  (if (or (eq (current-buffer) (get-buffer "*scratch*"))
          (eq (current-buffer) (get-buffer "*Messages*")))
      (progn (bury-buffer)
             nil)
    t))

(add-hook 'kill-buffer-query-functions 'ndw-immortal-buffers)

(require 'whitespace)
;; 17 October 2019, removed "face" here because it makes colored backgrounds
;; in other modes, such as Org and Markdown, ugly and distracting.
;; 3 November 2019, removed trailing because too many menus have it
(setq whitespace-style
      '(tabs trailing spaces space-before-tab newline space-after-tab space-mark tab-mark))
(setq whitespace-display-mappings
      '(
;        (space-mark   ?\    [?\x2423]   [?·] [?\ ])  ; space
        (space-mark   ?\xA0 [?\xA4]     [?_])        ; hard space
        ))
(global-whitespace-mode)
(setq-default show-trailing-whitespace nil)

(setq-default fill-column 70)
(setq sentence-end-double-space nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

;; Force unix and utf-8
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

(setq inhibit-eol-conversion t)

;; Uncomment if using abbreviations
;; (abbrev-mode t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

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

;; (load "~/.emacs.d/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(change-log-default-name "CHANGELOG.md")
 '(cider-boot-parameters "")
 '(custom-safe-themes
   '("2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "5516001c13a43f1f5be2d7df8035445f8d7c73179f637e40c1503afb184d98f2" default))
 '(latex-run-command "lualatex")
 '(magit-commit-arguments '("--signoff")))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )

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

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(straight-use-package 'auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(use-package smartparens)
;; (require 'smartparens-config)

(use-package rainbow-mode
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode))

;; http://www.emacswiki.org/emacs/RainbowDelimiters
;;(require 'rainbow-delimiters)
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;; (global-rainbow-delimiters-mode)

(use-package rainbow-delimiters)
(use-package paredit)

(require 'tramp)
(setq tramp-default-method "ssh")


(use-package bind-key)

(use-package key-chord)
(key-chord-mode +1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  Dired/Buffer Mgmt
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

(use-package dired+)
;; (use-package dired-aux)

(use-package diredfl)
(setq diredfl-global-mode 1)

(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable
(setq-default dired-dotfiles-show-p nil) ; Buffer-local variable
(setq dired-omit-files
      (concat dired-omit-files
              "\\|^\\..+$\\|^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"
              "\\|^bazel-.*"
              "\\|GPATH"
              "\\|GRTAGS"
              "\\|GTAGS"
              ))

(add-hook 'dired-mode-hook
          (lambda ()
            ;; Set dired-x buffer-local variables here.  For example:
            (setq dired-omit-mode 1)
            ))

(use-package dired-subtree)
(use-package dired-subtree
  :config
  (setq dired-subtree-use-backgrounds nil)
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)))

(use-package quick-preview)
(use-package quick-preview
  :init
  (global-set-key (kbd "C-c q") 'quick-preview-at-point)
  (define-key dired-mode-map (kbd "Q") 'quick-preview-at-point))

;; (add-hook 'dired-mode-hook
;; 	  (lambda ()
;; 	    ;; Set dired-x buffer-local variables here.  For example:
;; 	    (dired-omit-mode 1)
;; 	    (print dired-omit-extensions)
;; 	    ;; (delete ".pdf" dired-omit-extensions)
;; 	    ))
;;note: Info file is wrong:    (setq dired-omit-files-p t)

;;(require 'dired+)

;; expand selected region by semantic units:
(use-package expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ;;;;;;;;;;;;;;;; Data Formats ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package yaml-mode)

(use-package edn
  :straight t)
(load "edn-mode.el")
;; (require 'edn)
(add-to-list 'auto-mode-alist '("\.edn$" . edn-mode))

;;;;;; ASCIIdoc
(use-package adoc-mode)
;;(add-to-list 'load-path "~/.emacs.d/elisp/doc-mode-1.1")
(add-to-list 'auto-mode-alist '("\\.adoc$" . adoc-mode))
(autoload 'adoc-mode "adoc-mode")

(defun adoc-buffer-face-mode-fixed ()
   "Sets a fixed width (monospace) font in current buffer"
   (interactive)
   (setq buffer-face-mode-face '(:family "Inconsolata" :height 180))
   (buffer-face-mode))

;; (add-hook 'adoc-mode-hook (lambda() (buffer-face-mode nil)))
(add-hook 'adoc-mode-hook 'adoc-buffer-face-mode-fixed)

;;;;;;   NXML
(use-package auto-complete-nxml)

(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))

(setq rng-schema-locating-files
   (quote
    ("schemas.xml" "/Applications/Emacs.app/Contents/Resources/etc/schema/schemas.xml" "/usr/local/xml/schema/dita/schemas.xml")))

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

;;;;;;  DITA
(add-to-list 'auto-mode-alist '("\\.ditamap$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.dita$" . nxml-mode))

;; YANG files - https://tools.ietf.org/html/rfc6020
;; (use-package yang-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;     ;;;;;;;;;;;;;;;; Version Control ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; magit
(defvar tramp-ssh-controlmaster-options)
(defvar magit-last-seen-setup-instructions)
(use-package magit
  :init
  (setq tramp-ssh-controlmaster-options "")
  (setq magit-last-seen-setup-instructions "1.4.0"))
  ;; :bind
  ;; (("C-c t" . treemacs)
  ;;  ("s-a" . treemacs))

(use-package magit-find-file)

(use-package git-gutter
  :init
  (setq git-gutter:deleted-sign "▁")
  (setq git-gutter:added-sign "▌")
  (setq git-gutter:modified-sign "▌")
  :config
  (global-git-gutter-mode +1))

(global-set-key (kbd "C-c g") 'magit-status)
(magit-add-section-hook
   'magit-status-sections-hook
   'magit-insert-tracked-files
   'magit-insert-modules
   nil
   'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    ;;;;;;;; Programming Langauge Modes ;;;;;;;;;;;;;;;;    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq scheme-program-name "mibl")

(add-to-list 'auto-mode-alist '("dune$" . scheme-mode))
(add-to-list 'auto-mode-alist '("mibl$" . scheme-mode))

(use-package lua-mode)

(require 'lemon-mode)
(add-to-list 'auto-mode-alist '("\\.y$" . lemon-mode))

;;;;;; Clojure ;;;;;;
;; clojurescript-mode clojure-snippets clojure-quick-repls
;; edn
;;  mustache-mode
;;  Clojure
;;(require 'clojure-mode)
;; (use-package clojurescript-mode)
;; (use-package clojurescript-mode)
(use-package clojure-snippets)
(use-package clojure-quick-repls)
(add-to-list 'auto-mode-alist '("\\.cljd$" . clojure-mode))
;; (use-package helm-cider)

;;(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
;; (define-clojure-indent
;;   (defroutes 'defun)
;;   (GET 2)
;;   (POST 2)
;;   (PUT 2)
;;   (DELETE 2)
;;   (HEAD 2)
;;   (ANY 2)
;;   (context 2))
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

;;;;;; C/C++ ;;;;;;
(setq-default c-basic-offset 4)

;; c++ templates
(add-to-list 'auto-mode-alist '("\.tcc$" . c++-mode))





;;;;;; OCaml ;;;;;;
(use-package tuareg)
(use-package utop)

(use-package merlin)
;; gopcaml-mode requires ocaml >= 4.09.0
;; (use-package gopcaml-mode)

;; opam
;; Add opam emacs directory to your load-path by appending this to your .emacs:
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; (autoload 'gopcaml-mode "gopcaml-mode" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; (setq auto-mode-alist
    ;;       (append '(("\\.ml[ily]?$" . gopcaml-mode)
    ;;                 ("\\.topml$" . gopcaml-mode))
    ;;               auto-mode-alist))
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

;; (add-hook 'tuareg-mode-hook 'tuareg-imenu-set-imenu)
(setq auto-mode-alist
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))

(autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
(add-hook 'tuareg-mode-hook 'utop-minor-mode)

(add-hook 'tuareg-mode-hook 'merlin-mode)
(setq merlin-use-auto-complete-mode t)
(setq merlin-error-after-save nil)

;;;;;; Dart ;;;;;;
(use-package dart-mode)
;; dart
;; (add-to-list 'auto-mode-alist '("\.dart$" . java-mode))


;;;;;; Coq ;;;;;;
(use-package proof-general
  ;; :ensure t
  :mode ("\\.v\\'" . coq-mode)
  :init
  (setq coq-prog-name "coqtop")         ; or "C:/Coq/bin/coqtop.exe"…
)

;; (add-to-list 'load-path "~/.emacs.d/ProofGeneral/generic/")
(add-to-list 'load-path "~/.emacs.d/elpa/proof-general-4.4/generic/")
;; (replace-alist-mode auto-mode-alist 'verilog-mode 'proof-general-mode)

;; PROOF GENERAL stuff
;; (require 'package)
;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (add-to-list 'package-archives
;;                (cons "melpa" (concat proto "://melpa.org/packages/")) t))
;; (package-initialize)

;; (use-package company-coq
;;   :ensure t
;;   :hook
;;   (coq-mode . company-coq-mode)
;;   :init
;;   (setq company-coq-disabled-features '(hello prettify-symbols)))

;;;;;; Rust ;;;;;;
(use-package rust-mode)

;;;;;; Go ;;;;;;
(use-package go-mode)

;;;;;; Kotlin ;;;;;;
(use-package kotlin-mode)

;;;;;; Java ;;;;;;
(setenv "JAVA_HOME"  ;; FIXME: versioning
        "/Library/Java/JavaVirtualMachines/graalvm-ce-java11-19.3.0.2/Contents/Home")

 (add-hook 'c-mode-common-hook
	   (lambda ()
	     (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
	       (ggtags-mode 1))))

(add-hook 'c-mode-hook (lambda () comment-style (quote multi-line)))

(add-hook 'prog-mode-hook #'hs-minor-mode)

;;;;;; Ruby ;;;;;;
;;(add-hook 'ruby-mode-hook (lambda () (local-set-key 'f1 'ri)))

;;;;;; NetRexx ;;;;;;
(autoload 'netrexx-mode "netrexx-mode" "NETREXX mode" nil t)
(setq auto-mode-alist
      (append
       (list (cons "\\.nrx$"  'netrexx-mode)
	     (cons "\\.nry$"  'netrexx-mode)
	     )
       auto-mode-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  Build Tools  ;;;;;;;;
;; Bazel
(require 'bazel)
;; (use-package bazel-mode)
;; (load "bazel-mode.el")
;; bazel-mode doesn't do indenting; use python mode
(add-to-list 'auto-mode-alist '("\\.bazelrc" . bazel-mode))
(add-to-list 'auto-mode-alist '("BUILD$" . bazel-mode))
(add-to-list 'auto-mode-alist '(".bazel$" . bazel-mode))
;; (add-to-list 'auto-mode-alist '("/BUILD\\(\\..*\\)?\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE$" . bazel-mode))
(add-to-list 'auto-mode-alist '("/WORKSPACE\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("\\.\\(BUILD\\|WORKSPACE\\|bzl\\)\\'" . bazel-mode))

;; Groovy
;; (use-package groovy-mode)

;; Gradle
;; (use-package gradle-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;  Misc Tools  ;;;;;;;;
(use-package iedit)
;; (define-key iedit-mode-occurrence-keymap (kbd "RET") 'iedit-mode)
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (narrow-to-defun)
          (iedit-start (current-word) (point-min) (point-max)))))))

(global-set-key (kbd "C-;") 'iedit-dwim)

(use-package buffer-flip)
;; (use-package buffer-flip
;;   ;; :ensure t
;;   :chords (("u8" . buffer-flip))
;;   :bind  (:map buffer-flip-map
;;                ( "8" .   buffer-flip-forward)
;;                ( "*" .   buffer-flip-backward)
;;                ( "C-g" . buffer-flip-abort))
;;   :config
;;   (setq buffer-flip-skip-patterns
;;         '("^\\*helm\\b"
;;           "^\\*swiper\\*$")))

;; (use-package avy
;;   :defer t
;;   :bind
;;   (("M-z c" . avy-goto-char-timer)
;;    ("M-z l" . avy-goto-line))
;;   :custom
;;   (avy-timeout-seconds 0.3)
;;   (avy-style 'pre)
;;   :custom-face
;;   (avy-lead-face ((t (:background "#51afef" :foreground "#870000" :weight bold)))))

(use-package fzf)
(use-package helm-descbinds)
;; (use-package helm-ls-git)
;; (use-package helm-make)
(use-package helm-system-packages)
(use-package helm-unicode)
(use-package xquery-tool)

;;;;;;;; Statistics ;;;;;;;;
;; (use-package ess)
;; (use-package ess-smart-equals)
;; (use-package ess-smart-underscore)
;;(require 'ess-r-mode)
;; (require 'ess-site)
;;
;; ESS julia language
;; https://github.com/emacs-ess/ESS/wiki/Julia
;; excecutable file
;; (setq inferior-julia-program-name
;;       "/Applications/Julia-0.2.1.app/Contents/Resources/julia/bin/julia-basic")
;; nope:  "/Applications/Julia/Contents/Resources/julia/bin/julia")
;; put julia/bin on PATH instead?


(use-package rnc-mode
  :init
  (setq rnc-indent-level 3))

(use-package helm-gtags)
;; (use-package yas-jit)
(use-package xcscope)
(use-package solarized-theme)
(use-package mustache-mode)
(use-package markdown-mode)
(use-package ido-vertical-mode)
;; (use-package ido-ubiquitous)
(use-package ido-at-point)

(use-package ctags)
(use-package company-math)
(use-package color-theme-modern)


;; (eval-after-load "enriched"
;;     '(defun enriched-decode-display-prop (start end &optional param)
;;        (list start end)))

(use-package uuidgen)
(use-package uuidgen
  :demand)

(use-package helm)
(use-package helm-descbinds)
(use-package helm-descbinds
  :demand
  :config
  (helm-descbinds-mode))

;;; List of personal key bindings

;;; Install [ripgrep](https://github.com/BurntSushi/ripgrep) (rg) and
;;; add

(use-package helm-rg
  :commands helm-rg
  :init
  (bind-key "C-c r" 'helm-rg)
  ;; :bind
  ;; (("C-c r" . helm-rg))
  )

;;; File explorer sidebar

(use-package treemacs)
(use-package treemacs
  ;; :bind
  ;; (("C-c t" . treemacs)
  ;;  ("s-a" . treemacs))
  )

;; (global-set-key (kbd "C-c h b") 'describe-personal-keybindings)

;;; Cycle through buffers' history

;; (use-package buffer-flip
;;   :bind
;;   (("s-o" . buffer-flip)
;;    :map buffer-flip-map
;;    ("s-o" . buffer-flip-forward)
;;    ("s-O" . buffer-flip-backward)
;;    ("C-g" . buffer-flip-abort)))

;;; [fzf](https://github.com/junegunn/fzf) and
;;; [lcd](https://github.com/lukpank/lcd) for finding files and
;;; directories

;; (use-package fzf
;;   :commands fzf/start
;;   :bind
;;   (("C-c f" . fzf)
;;    ("C-c D" . my-lcd)))

;; (defun my-lcd ()
;;   (interactive)
;;   (fzf/start default-directory
;;              (fzf/grep-cmd "lcd" "-l %s")))

;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; (when (memq window-system '(mac ns))
;; ;; NB: the shell path is the same one you get opening a shell, i.e. in the case of bash it runs ~/.bash_profile etc.
;;   (message (make-string "window-system:" window-system))
;;   (exec-path-from-shell-initialize)
;; )

(use-package exec-path-from-shell)
  ;; (message (make-string "window-system:" window-system))
(exec-path-from-shell-initialize)


;; (load "my-abbrevs")

;; (load "hava-mode.el")
;; (add-to-list 'auto-mode-alist '("\.hava$" . hava-mode))

;; ocaml - https://dev.realworldocaml.org/install.html

(use-package imenu)
;; xql
;; (load "xquery-mode.el")
;; (require 'xquery-mode)
;; (add-to-list 'auto-mode-alist '("\.xql$" . xquery-mode))
;; (add-to-list 'auto-mode-alist '("\.xqm$" . xquery-mode))
;; (add-to-list 'auto-mode-alist '("\.xq$" . xquery-mode))

;; Remove all annoying modes from auto mode lists

(defun replace-alist-mode (alist oldmode newmode)
  (dolist (aitem alist)
    (if (eq (cdr aitem) oldmode)
    (setcdr aitem newmode))))

(setq user-full-name "Gregg Reynolds")
;(setq user-mail-address "gar@mobileink.com")

(defalias 'qrr 'query-replace-regexp)

(toggle-menu-bar-mode-from-frame)
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                           'fullboth)))
(global-set-key [(meta return)] 'toggle-fullscreen)

;; (require 'xcscope)
;; (cscope-setup)

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

;; get rid of `find-file-read-only' and replace it with something
;; more useful.
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(global-set-key (kbd "C-c h") 'helm-mini)

(global-set-key (kbd "C-x C-f") 'helm-find-files)


(use-package ggtags)

;; (load-theme 'solarized-dark t)
;; (load-theme 'solarized-light t)
;; (color-theme-solarized-dark)

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

;;
(add-hook 'cider-mode-hook 'turn-on-eldoc-mode)
;;(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;(setq nrepl-hide-special-buffers t)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/snippets"))
(use-package yasnippet)
;; (require 'yasnippet)
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

(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
;; (setq initial-frame-alist
;;        '((left . 80)
;; 	 (top . 50)
;; 	 (height . 30)
;; 	 (width . 160)))

;; (setq default-frame-alist
;;        '((left . 80)
;; 	 (top . 50)
;; 	 (height . 30)
;; 	 (width . 160)))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(adoc-align ((t (:inherit bold-italic))))
 '(markup-internal-reference-face ((t (:inherit markup-code-face :underline t))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray50" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1.0 :width normal :foundry "unknown" :family "Monospace"))))
 '(markup-reference-face ((t (:inherit markup-title-3-face :underline nil)))))
(put 'upcase-region 'disabled nil)
