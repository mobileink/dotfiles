(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t))

(when (memq window-system '(mac ns))
;; NB: the shell path is the same one you get opening a shell, i.e. in the case of bash it runs ~/.bash_profile etc.
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

(add-to-list 'load-path "~/.emacs.d/ProofGeneral/generic/")

;; (load "~/.emacs.d/init.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(latex-run-command "lualatex")
 '(rng-schema-locating-files
   (quote
    ("schemas.xml" "/Applications/Emacs.app/Contents/Resources/etc/schema/schemas.xml" "/usr/local/xml/schema/dita/schemas.xml")))
 '(safe-local-variable-values
   (quote
    ((warroot . "/Users/gar/sparky/sparky-rest/target/sparky-1.0-SNAPSHOT/WEB-INF")
     (srcroot . "/Users/gar/sparky/sparky-rest/src/main/clojure")
     (app . "sparky-rest")
     (warroot . "/Users/gar/sparky/hello/target/hello-1.0-SNAPSHOT/WEB-INF/classes")
     (srcroot . "/Users/gar/sparky/hello/src/main/clojure")
     (warroot . "/Users/gar/dev/aama-rest/war/WEB-INF/classes")
     (warroot . "/Users/gar/dev/aama-rest/war")
     (srcroot . "/Users/gar/dev/aama-rest/src")
     (app . "aama")))))
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

(setq-default show-trailing-whitespace t)
(setq transient-mark-mode t)
(global-hl-line-mode 1)
;; To customize the background color
;; (set-face-background 'hl-line "#677")
(global-font-lock-mode 1)

(setq user-full-name "Gregg Reynolds")
(setq user-mail-address "gar@mobileink.com")

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

(global-set-key (kbd "C-c h") 'helm-mini)

(global-set-key (kbd "C-x C-f") 'helm-find-files)

; Force unix and utf-8
(if (> emacs-major-version 21)
    (progn
      (set-language-environment "utf-8")
      (set-input-method "latin-1-postfix")
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
	 (width . 175)))

(setq default-frame-alist
       '((left . 100)
	 (top . 50)
	 (height . 50)
	 (width . 175)))

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


;; (load-theme 'solarized-dark t)
(load-theme 'solarized-light t)

;; http://www.emacswiki.org/emacs/RainbowDelimiters
(require 'rainbow-delimiters)
;; (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(global-rainbow-delimiters-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;   CUSTOMIZATIONS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; http://ergoemacs.org/emacs/emacs_buffer_management.html
(defalias 'list-buffers 'ibuffer)
;; make buffer switch command show suggestions
;; http://ergoemacs.org/emacs/emacs_buffer_switching.html
(require 'ido)
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


;;(require 'smartparens-config)

;;  Clojure
;;(require 'clojure-mode)

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))

(load (expand-file-name "~/.emacs.d/elisp/migae"))
(add-hook 'clojure-mode-hook
      (lambda ()
        (set (make-local-variable 'parse-sexp-ignore-comments) t) ))
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (message "clojure-mode-hook migae: %s" (buffer-file-name))
	    (define-key clojure-mode-map (kbd "C-x C-s") 'migae-save-clj-buffer)))
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
;(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
;(add-hook 'cider-repl-mode-hook 'paredit-mode)
;(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
;(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;(setq nrepl-hide-special-buffers t)


(require 'yasnippet)
(yas-global-mode 1)

(require 'ess-site)

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
(require 'proof-site)

(setenv "ATSHOME" "/usr/local/lib/ats2-postiats-0.1.2")
(setenv "PATSHOME" "/usr/local/lib/ats2-postiats-0.1.2")

(require 'ats2-flymake)


(set-face-attribute 'default nil :foundry "apple" :family "Source Code Pro")

(setq default-directory (expand-file-name "~/"))
(setq command-line-default-directory "~/")
(setq inhibit-splash-screen t)

(add-to-list 'auto-mode-alist '("\\.g4\\'" . antlr-mode))

(load-file (let ((coding-system-for-read 'utf-8))
                (shell-command-to-string "agda-mode locate")))
