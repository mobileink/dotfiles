;;(require 'derived)

;;; Local Variables:
;;; eval: (arb-minor-mode)
;;; End:

;; setting font for input method:
;; https://emacs.stackexchange.com/questions/5519/how-to-assign-a-certain-font-for-each-input-method-language-in-emacs-24

(defconst arb-fatha ?\u064E) ;; "fatha"
(defconst arb-damma ?\u064F) ;; "damma"
(defconst arb-kasra ?\u0650) ;; "kasra"

(defconst arb-shadda ?\u0651)
(defconst maybe-shadda (string arb-shadda ?\?))

(defconst arb-sukun ?\u0652)
(defconst maybe-sukun (string arb-sukun ?\?))

(defconst arb-haraka (string arb-fatha arb-damma arb-kasra))
    ;; \u064B ;; fathatan
    ;; \u064C ;; dammatan
    ;; \u064D ;; kasratan
    ;; ))
(defconst maybe-haraka (concat "[" arb-haraka "]?"))

(defconst arb-fathatan ?\u064B) ;; "fathatan"
(defconst arb-dammatan ?\u064C) ;; "dammatan"
(defconst arb-kasratan ?\u064D) ;; "kasratan"
(defconst arb-tanween (string arb-fathatan arb-dammatan arb-kasratan))

(defconst arb-tashkeel
  (concat arb-haraka arb-tanween
          (string arb-shadda arb-sukun)
          (string ?\u0653 ;; "madda above"
                  ?\u0654 ;; "hamza above"
                  ?\u0655 ;; "hamza below"
                  )))
(defconst maybe-tashkeel (concat "[" arb-tashkeel "]?"))

(defconst arb-hamza            ?\u0621)
(defconst alif-madda-above ?\u0622)
(defconst alif-hamza-above ?\u0623)
(defconst waw-hamza-above  ?\u0624)
(defconst alif-hamza-below ?\u0625)
(defconst ya-hamza-below   ?\u0626)
(defconst arb-hamz (string arb-hamza alif-madda-above
                       alif-hamza-above alif-hamza-below
                       waw-hamza-above ya-hamza-below))

;; 1.  find word with root, ignoring tashkeel
;; 2.  check spelling
;; 3.  parse/categorize

(defun sib-root ()
  "Find words with root ROOT in buffer."
  (interactive) ;; "MsRoot? ")
  (let ((ROOT) (RADS) (RXP))
    (setq ROOT
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word t t)))
    (if ROOT
        (setq ROOT (replace-regexp-in-string " " "_" ROOT))
      (setq ROOT (read-string "Enter root:" nil nil nil t)))
    (setq RADS (vconcat ROOT))
    (setq RXP (concat (string (aref ROOT 0))
                      maybe-tashkeel
                      (string (aref ROOT 1))
                      maybe-tashkeel
                      ;; "\\("
                      ;; maybe-shadda
                      ;; maybe-haraka
                      ;; "\\|"
                      ;; maybe-sukun
                      ;; "\\)"
                      (string (aref ROOT 2))
                      maybe-tashkeel
                      ))
    (re-search-forward RXP) ;; nil nil 5)
    (message "Root %s, RXP %s: " ROOT RXP)
    ))

(defun sib-maany ()
  "Look up the word under cursor al almaany.com.
If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (browse-url (concat
                 ;; "http://en.wikipedia.org/wiki/"
                 "https://www.almaany.com/ar/dict/ar-en/"
                 word))
    ;; (eww myUrl) ; emacs's own browser
    ))

(defun sib-lex ()
  "Look up the word under cursor in lex.sibawayhi.
If there is a text selection (a phrase), use that.

This command switches to browser."
  (interactive)
  (let (word)
    (setq word
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (current-word)))
    (setq word (replace-regexp-in-string " " "_" word))
    (setq word (replace-regexp-in-string (string
                                          ?[?\u064E ;; fatha
                                            ?\u064F ;; damma
                                            ?\u0650 ;; kasra
                                            ?\u0651 ;; shadda
                                            ?\u0652 ;; sukun
                                            ?\u0627 ;; alif
                                         ?])
                                         ""
                                         word))
    (browse-url (concat
                 "http://lex.sibawayhi.org/html/pi/"
                 word))
    ;; (eww myUrl) ; emacs's own browser
    ))

(defun arb:q (qry)
  (interactive "P")
  ;; (goto-char (point-min))
  (perform-replace "^ *\\(.+\\)$" ;; match non-space char at beginning of line
                   "\"\\1\""
                   t   ;; query-flag
                   t   ;; regexp-flag
                   nil ;; delimited-flag
                   nil ;; repeat-count
                   nil ;; map
                   (region-beginning) ;; start
                   (region-end) ;; end
                   )
  ;; (perform-replace " *$" ;; match non-space char at beginning of line
  ;;                  "\""
  ;;                  t   ;; query-flag
  ;;                  t   ;; regexp-flag
  ;;                  nil ;; delimited-flag
  ;;                  nil ;; repeat-count
  ;;                  nil ;; map
  ;;                  (region-beginning) ;; start
  ;;                  (region-end) ;; end
  ;;                  )
)

(defun arb:sp (min max)
  (interactive "r")
  (perform-replace
   (concat "< هَارُونَ مُجَلَّدَ = ?\" "
           "\\([١٢٣٤]+\\) ?\""
           "  *صَفْحَةَ ?= ?\" ?"
           "\\([٠١٢٣٤٥٦٧٨٩]+\\)"
           " *\""
           )
   "<هارون مجلد=\"\\1\" صفحة=\"\\2\""
                   t t nil nil nil min max)
  ;; (perform-replace "صَفْحَةَ =\" "
  ;;                  "صفحة=\""
  ;;                  t t nil nil nil min max)
  (perform-replace ""
                   "هارون"
                   t t nil nil nil min max)
  (perform-replace "بنَات"
                   "بَنَات"
                   t t nil nil nil min max)
  (perform-replace "الأرُبَّعَة"
                   "الْأَرْبَعَة"
                   t t nil nil nil min max)
  (perform-replace "ذ ٰ لِكَ"
                   "ذٰلِكَ"
                   t t nil nil nil min max)
  (perform-replace "الصَّفَّة"
                   "الصِّفَة"
                   t t nil nil nil min max)
  (perform-replace "الْاِسْم" "الِاسْم"
                   t t nil nil nil
                   min max)
  )

(add-hook 'arb-minor-mode-hook
          (lambda () (set-input-method "arabic")))

(add-hook 'arb-minor-mode-hook
          #'(lambda ()
              (message "running arb-minor-mode-hook")
              (yas-activate-extra-mode 'arb-minor-mode))
          )


(define-minor-mode arb-minor-mode
  "Minor mode for editing arabic files"
  :lighter " arb"
  :keymap (current-local-map)
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c q") 'arb:q)
            (define-key map (kbd "C-c s") 'arb:sp)
            (define-key map (kbd "C-c n") 'rng-next-error)
            map)
  )

  ;; :after-hook (yas-activate-extra-mode 'arb-minor-mode))

;; (setq case-fold-search nil)

(provide 'arb-minor-mode)
