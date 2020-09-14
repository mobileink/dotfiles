(require 'derived)

(define-derived-mode hava-mode clojure-mode "hava"
  "Major mode for editing hava
Special commands:
\\{hava-mode-map}"
  )

(setq case-fold-search nil)

(defun hava:q (qry)
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

(provide 'hava)

