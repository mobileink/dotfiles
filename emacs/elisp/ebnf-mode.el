;; define several class of keywords
;; (setq ebnf-keywords '("break" "default" "do" "else" "for" "if" "return" "state" "while") )
;; (setq ebnf-types '("float" "integer" "key" "list" "rotation" "string" "vector"))
;; (setq ebnf-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
;; (setq ebnf-events '("at_rot_target" "at_target" "attach"))
;; (setq ebnf-functions '("llAbs" "llAcos" "llAddToLandBanList" "llAddToLandPassList"))

(setq ebnf-symbols '("ZCHARS"
		     "="
		     ","
		     ";"
		     "|"
		     "[" "]" "{" "}" "(" ")"
		     "?" "-" "'"
		     ;; "(*" "*)")
      ))

(setq ebnf-symbols-regexp (regexp-opt ebnf-symbols t))

(setq ebnf-font-lock-keywords
      `(
	(,ebnf-symbols-regexp . font-lock-constant-face)
	))

;; Create mode-specific table variables.
(defvar ebnf-mode-syntax-table nil "")
;; (defvar emacs-lisp-mode-syntax-table nil "")
;; (defvar lisp-mode-abbrev-table nil "")

(if (not ebnf-mode-syntax-table) ; Do not change the table
                                       ;   if it is already set.
    (let ((i 0))
      (setq ebnf-mode-syntax-table (make-syntax-table))
    ;; comment: “(* … *)”
      (modify-syntax-entry ?\( ". 1" ebnf-mode-syntax-table)
      (modify-syntax-entry ?\) ". 4" ebnf-mode-syntax-table)
      (modify-syntax-entry ?* ". 23" ebnf-mode-syntax-table))
  )


;; command to comment/uncomment text
(defun ebnf-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let (
	(comment-start "(*") (comment-end "*)")
        )
    (comment-dwim arg)))

(defun ebnf-indent-line ()
  "Indent current line as EBNF code."
;  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0)		   ; First line is always non-indented
    ;; (let ((not-indented t) cur-indent)
    (cond ((looking-at "^[ \t]*[A-Z]") ; begin Production Rule
	   (indent-line-to 0))
	  ((looking-at "^[ \t]*|")
	   (indent-line-to 4))
	  ((looking-at "^[ \t]*;")
	   (indent-line-to 4))
	  ((looking-at "^[ \t]*(\*") ; begin Production Rule
	   (indent-line-to 0))
	  (t (indent-line-to 4)))))

      ;; (if (looking-at "^[ \t]*;") ; If the line we are looking at is the end of a block, then decrease the indentation
      ;; 	  (progn
      ;; 	    (save-excursion
      ;; 	      (forward-line -1)
      ;; 	      (setq cur-indent (- (current-indentation) tab-width)))
      ;; 	    (if (< cur-indent 0) ; We can't indent past the left margin
      ;; 		(setq cur-indent 0)))
	;; (save-excursion
	;;   (while not-indented ; Iterate backwards until we find an indentation hint
	;;     (forward-line -1)
	;;     (if (looking-at "^[ \t]*|") ; This hint indicates that we need to indent at the level of the END_ token
	;; 	(progn
	;; 	  (setq cur-indent (current-indentation))
	;; 	  (setq not-indented nil))
	;;       (if (looking-at "^[ \t]*|")
	;; 	  (progn		; indent an extra level
	;; 	    (setq cur-indent (+ (current-indentation) default-tab-width))
	;; 	    (setq not-indented nil))
	;; 	(if (bobp)
	;; 	    (setq not-indented nil))))))
	;; )
      ;; else
      ;; (if cur-indent
      ;; 	  (indent-line-to cur-indent)
      ;; 	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

;; define the mode
(define-derived-mode ebnf-mode fundamental-mode
  "ebnf"
  "Major mode for editing ebnf"
  :syntax-table ebnf-mode-syntax-table

  (setq-local comment-start "(* ")
  (setq-local comment-end " *)")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
	      '(sample-font-lock-keywords))
  (setq-local indent-line-function 'ebnf-indent-line)

  (setq font-lock-defaults '((ebnf-font-lock-keywords)))

  ;; ;; clear memory
  ;; (setq ebnf-keywords-regexp nil)
  ;; (setq ebnf-types-regexp nil)
  ;; (setq ebnf-constants-regexp nil)
  ;; (setq ebnf-events-regexp nil)
  ;; (setq ebnf-functions-regexp nil)

  (setq ebnf-syntax-regexp nil)

  (setq tab-width 4)

  (run-hooks 'ebnf-mode-hook))	      ; This permits the user to use a
					;   hook to customize the mode.

(add-to-list 'auto-mode-alist '("\\.ebnf\\'" . ebnf-mode))

(provide 'ebnf-mode)
