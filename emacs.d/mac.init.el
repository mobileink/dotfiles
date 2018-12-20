;و;(when (>= emacs-major-version 24)
(require 'package)
(package-initialize)
;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
;;(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
;;  )

(when (not package-archive-contents)
  (package-refresh-contents))


(when (memq window-system '(mac ns))
;; NB: the shell path is the same one you get opening a shell, i.e. in the case of bash it runs ~/.bash_profile etc.
  (message "window-system:" window-system)
  (exec-path-from-shell-initialize)
)

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

(add-to-list 'load-path "~/.emacs.d/elisp")
;; (load "my-abbrevs")

;; (add-to-list 'load-path "~/.emacs.d/ProofGeneral/generic/")

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
    (ggtags helm cider dart-mode yas-jit xcscope solarized-theme smartparens rainbow-delimiters paredit mustache-mode markdown-mode magit ido-vertical-mode ido-ubiquitous ido-at-point groovy-mode gradle-mode exec-path-from-shell ess edn ctags company-math color-theme-solarized color-theme-sanityinc-solarized clojurescript-mode clojure-snippets clojure-quick-repls auto-complete-nxml adoc-mode)))
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

(setq initial-frame-alist
       '((left . 100)
	 (top . 50)
	 (height . 50)
	 (width . 215)))

(setq default-frame-alist
       '((left . 100)
	 (top . 50)
	 (height . 50)
	 (width . 215)))

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


;; (require 'smartparens-config)

;;  Clojure
;;(require 'clojure-mode)

;;(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
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
(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode f)))

;; bazel
(add-to-list 'auto-mode-alist '("BUILD$" . python-mode))


;(require 'ess-site)

;;
;; ESS julia language
;; https://github.com/emacs-ess/ESS/wiki/Julia
;; excecutable file
(setq inferior-julia-program-name
      "/Applications/Julia-0.2.1.app/Contents/Resources/julia/bin/julia-basic")
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

;; (load-file "dir/generic/proof-site.el")
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
  (set-face-attribute 'default t :family "Monaco")

  (set-default-font "Monaco")

  ;; default font size (point * 10)
  ;;
  ;; WARNING!  Depending on the default font,
  ;; if the size is not supported very well, the frame will be clipped
  ;; so that the beginning of the buffer may not be visible correctly. 
  (set-face-attribute 'default nil :height 135)

  ;; use specific font for Korean charset.
  ;; if you want to use different font size for specific charset,
  ;; add :size POINT-SIZE in the font-spec.
  ;; (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

  ;; you may want to add different for other charset in this way.
  )

(split-window-right)
