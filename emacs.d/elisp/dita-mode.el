
(defvar hava-awzan
  '("faʕluŋ" "faʕlaŧuŋ"
    "fuʕluŋ" "fuʕlaŧuŋ"
    "fiʕluŋ" "fiʕlaŧuŋ"
    "fiʕāluŋ" "fiʕālaŧuŋ"
    "fāʕiluŋ" "mifʕaluŋ"
    "ąfʕuluŋ" "ąfʕāluŋ"
    "fuʕūluŋ"
    )
  "Arabic noun forms.")
;; create the regex string for each class of keywords
(defvar hava-awzan-regexp (regexp-opt hava-awzan 'words))


;; Create mode-specific tables.
;;(defvar hava-mode-syntax-table nil 
;;  "Syntax table used while in text mode.")

;; (if hava-mode-syntax-table
;;     ()              ; Do not change the table if it is already set up.
;;   (setq hava-mode-syntax-table (make-syntax-table))
;;   (modify-syntax-entry ?\" ".   " hava-mode-syntax-table)
;;   (modify-syntax-entry ?\\ ".   " hava-mode-syntax-table)
;;   (modify-syntax-entry ?' "w   " hava-mode-syntax-table))

;;{{{ hava-indent-line

;; (defun hava-indent-line ()
;;   "Indent current line for Lua mode.
;; Return the amount the indentation changed by."
;;   (let ((indent (max 0 (- (hava-calculate-indentation nil)
;; 			  (hava-calculate-indentation-left-shift))))
;; 	beg shift-amt
;; 	(case-fold-search nil)
;; 	(pos (- (point-max) (point))))
;;     (beginning-of-line)
;;     (setq beg (point))
;;     (skip-chars-forward hava-indent-whitespace)
;;     (setq shift-amt (- indent (current-column)))
;;     (when (not (zerop shift-amt))
;;       (delete-region beg (point))
;;       (indent-to indent))
;;     ;; If initial point was within line's indentation,
;;     ;; position after the indentation.  Else stay at same point in text.
;;     (if (> (- (point-max) pos) (point))
;; 	(goto-char (- (point-max) pos)))
;;     shift-amt
;;     indent))

;;}}}

(defvar hava-mode-abbrev-table nil
  "Abbrev table used while in hava mode.")
(define-abbrev-table 'hava-mode-abbrev-table

  '(("b" "<bi/>")
    ("bh" "<bi-hi/>")
    ("bha" "<bi-hā/>")
    ("bhm" "<bi-him/>")
    ("l" "<li/>")
    ("al-" "<al-")
    ("h" "<hu/>")
    ("hu" "<hu/>")
    ("ha" "<hā/>")
    ("hm" "<hum/>")
    ("ala" "<ʕala/>")
    ("alh" "<ʕalayhi/>")
    ("alha" "<ʕalayhā/>")
    ("ån" "<ʕan/>")
    ("bynhm" "<baynahum/>")

    ("xun" "<xun/>")
    ("xan" "<xan/>")
    ("xin" "<xin/>")
    ("xu" "<xu/>")
    ("xa" "<xa/>")
    ("xi" "<xi/>")
    ("lxu" "<lxu/>")
    ("lxa" "<lxa/>")
    ("lxi" "<lxi/>")

    ("xxu" "<xxu/>")
    ("xxa" "<xxa/>")
    ("xxi" "<xxi/>")
    ("xxun" "<xxun/>")
    ("xxan" "<xxan/>")
    ("xxin" "<xxin/>")
    ("lxxu" "<lxxu/>")
    ("lxxa" "<lxxa/>")
    ("lxxi" "<lxxi/>")


    ("1ap" "fāʕiluŋ")
    ("1apf" "fāʕilaŧuŋ")
    ("Ai" "fāʕiluŋ")
    ("Aip" "fāʕilaŧuŋ")

    ("fa" "faʕluŋ")
    ("fap" "faʕlaŧuŋ")
    ("fi" "fiʕluŋ")
    ("fip" "fiʕlaŧuŋ")
    ("fu" "fuʕluŋ")
    ("fup" "fuʕlaŧuŋ")
    ("ff" "faʕʕaluŋ")
    ("ffp" "faʕʕalaŧuŋ")
    ("fafa" "faʕʕaluŋ")
    ("fafap" "faʕʕalaŧuŋ")
    ("FF" "faʕʕāluŋ")
    ("FFp" "faʕʕālaŧuŋ")
    ("fāfā" "faʕʕāluŋ")
    ("fāfāp" "faʕʕālaŧuŋ")

    ("aa" "faʕaluŋ")
    ("ai" "faʕiluŋ")
    ("aap" "faʕalaŧuŋ")
    ("aA" "faʕāluŋ")
    ("aAp" "faʕālaŧuŋ")

    ("au" "faʕuluŋ")
    ("aup" "faʕulaŧuŋ")
    ("aU" "faʕūluŋ")
    ("aUp" "faʕūlaŧuŋ")

    ("uA" "fuʕāluŋ")

    ("ia" "fiʕaluŋ")
    ("iap" "fiʕalaŧuŋ")
    ("iA" "fiʕāluŋ")
    ("iAp" "fiʕālaŧuŋ")

    ("iI" "faʕīluŋ")
    ("iIp" "faʕīlaŧuŋ")

    ("'i" "ąfāʕilu")
    ("'I" "ąfāʕīlu")
    ("'A" "ąfʕāluŋ")
    ("'p" "ąfʕilaŧuŋ")
    ("'u" "ųfʕuluŋ")

    ("mai" "mafʕiluŋ")
    ("maip" "mafʕilaŧuŋ")
    ("mia" "mifʕaluŋ")
    ("miap" "mifʕalaŧuŋ")
    ("mii" "mafāʕilu")
    ("mII" "mafāʕīlu")

    ("Fi" "faʕāįlu")
    ("FI" "faʕāįylu")
    ("wi" "fawāɁilu")
    ("wI" "fawāɁīlu")

    ("'i" "ąfāɁilu")
    ("'I" "ąfāɁīlu")

    ("faa" "fāʕiluŋ")

    ("kdh" "kaðā")
    ("lwjh" "<li/><wajhihi/>")
    ("wjhh" "<wajhahu/>")
    ("shu" "<al_šayˀu/>")
    ("sha" "<al_šayˀa/>")
    ("ash" "<ʕan/><al_šayˀi/>")
    ("flnu" "<fulānuŋ")
    ("flna" "<fulānaŋ")
    ("zbu" "<al_ẓabyu")
    ("zba" "<al_ẓabya")

    ;; ("" "mifāʕīlu")

    ;; ("b" "<bi/>" nxml-indent-line 0)
    ))

;(defvar hava-mode-map nil   ; Create a mode-specific keymap.
;  "Keymap for hava mode.")

(defun hava-setup-keymap ()
  "Set up keymap for hava mode."
  (define-key hava-mode-map (kbd "C-c w") 'hava-wzn-mode)
  (define-key hava-mode-map (kbd "C-c 4") 'toggle-input-method)
  )
;; ;  (define-key hava-mode-map (kbd "C-c e") '(yas/insert-snippet hava-entry))
;;   ;; (define-key hava-mode-map "u,," 'insert-ʕu)
;;   ;; (define-key hava-mode-map "u." 'insert-ʕi)
;;   ;; (define-key hava-mode-map "uu" 'insert-ʕʕ)
;;   )

;;;###autoload
 (define-derived-mode hava-mode nxml-mode "hava"
   "A major mode for editing hava files
 Special commands: \\{hava-mode-map}
Turning on hava-mode runs the hook `hava-mode-hook'."
;   :syntax-table hava-mode-syntax-table
   ;; code for syntax highlighting
;   (setq font-lock-defaults '((hava-font-lock-keywords)))
   ;; clear memory
   (font-lock-add-keywords nil `((,hava-awzan-regexp . 'font-lock-warning-face)))
;   (setq hava-awzan-regexp nil)
   (setq local-abbrev-table hava-mode-abbrev-table)
   (abbrev-mode 1)
   (hava-setup-keymap)
   (setq case-replace nil)
   (setq case-fold-search nil)
   (set-input-method 'latin-4-postfix))
   ;; (load-file "~/.emacs.d/elisp/hava.el"))

(provide 'hava-mode)
