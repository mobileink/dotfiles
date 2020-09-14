;; (defun insert-p-tag ()
;;   "Insert <p></p> at cursor point."
;;   (interactive)
;;   (insert "<p></p>")
;;   (backward-char 4))

;; (defun replace-greek-region ()
;;   "Replace “alpha” to “α” and other greek letters in current region."
;;   (interactive)
;;   (let (
;;         (p1 (region-beginning))
;;         (p2 (region-end)))
;;     (save-restriction
;;       (narrow-to-region p1 p2)
;;       (goto-char (point-min))
;;       (while (search-forward " alpha" nil t)
;;         (replace-match " α" nil t))
;;       (goto-char (point-min))
;;       (while (search-forward " beta" nil t)
;;         (replace-match " β" nil t))
;;       (goto-char (point-min))
;;       (while (search-forward " gamma" nil t)
;;         (replace-match " γ" nil t)))))

(defun qstage1 ()
  ""
  (interactive)
  (let ((case-fold-search t)) ; or nil

    ;; this must go first:
    (goto-char (point-min))
    (while (search-forward-regexp "\C-L" nil t)
      (replace-match ""))


    (goto-char (point-min))
    (while (search-forward-regexp
            "^\\([A-Z0-9_]+\\): +\\(.*\\)$" nil t)
      (replace-match "</item>

<item qid=\"\\1\" nshapid=\"\\1\" desc=\"\\2\">"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *- *\\([0-9]+\\) *- *$"
            nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (search-forward-regexp
            "Based upon \\([0-9,]+\\) valid cases out of \\([0-9,]+\\) total cases."
            nil t)
      (replace-match "<cases valid=\"\\1\" total=\"\\2\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "• *Mean: *\\([0-9.]+\\) *$"
            nil t)
      (replace-match "<stat mean=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "• *Median: *\\([0-9.]+\\) *$"
            nil t)
      (replace-match "<stat median=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "• *Mode: *\\([0-9.]+\\) *$"
            nil t)
      (replace-match "<stat mode=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "• *Minimum: *\\([0-9.]+\\) *$"
            nil t)
      (replace-match "<stat min=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "• *Maximum: *\\([0-9.]+\\) *$"
            nil t)
      (replace-match "<stat max=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "• *Standard Deviation: *\\([0-9.]+\\) *$"
            nil t)
      (replace-match "<stat SD=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Location: *\\([0-9]+\\) *- *\\([0-9]+\\) *(width: *\\([0-9]+\\); *decimal: \\([0-9]+\\))"
            nil t)
      (replace-match "<locn cols=\"\\1 \\2\" precision=\"\\4\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Variable Type: *\\(.*\\)$"
            nil t)
      (replace-match "<vtype type=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "(Range of) Missing Values: *\\(.*\\)$"
            nil t)
      (replace-match "<missing range=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Value *Label *Unweighted *%
 *Frequency
"
            nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Missing Data *
"
            nil t)
      (replace-match ""))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Total *\\([,0-9,]+\\) *\\(.*\\)$"
            nil t)
      (replace-match "<total freq=\"\\1\" pct=\"\\1\"/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *\\([-0-9\\s.]+\\)\\s-+*\\([()\\/ a-z,/-()A-Z'-]+\\)\\s-+\\([0-9]+\\) *\\(.*\\)$"
            nil t)
      (replace-match "<r val=\"\\1\" lbl=\"\\2\" f=\"\\3\" pct=\"\\4\"/>"))

    ))

(defun qdays ()
  ""
  (interactive)
  (let ((case-fold-search t)) ; or nil

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Days *$"
            nil t)
      (replace-match "<days/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Day *$"
            nil t)
      (replace-match "<day/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Weeks *$"
            nil t)
      (replace-match "<weeks/>"))
    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Week *$"
            nil t)
      (replace-match "<week/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Months *$"
            nil t)
      (replace-match "<months/>"))
    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Month *$"
            nil t)
      (replace-match "<month/>"))
    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Years *$"
            nil t)
      (replace-match "<years/>"))
    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Year *$"
            nil t)
      (replace-match "<year/>"))))

(defun qtags ()
  ""
  (interactive)
  (let ((case-fold-search t)) ; or nil

    (goto-char (point-min))
    (while (search-forward-regexp
            "Please note that only the first 50 response categories are displayed in the PDF codebook. To view all response categories, please
 *analyze the data file in the statistical package of your choice (SAS, SPSS, Stata, R)."
            nil t)
      (replace-match "<note.first50/>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "Skip\\(.*\\)$"
            nil t)
      (replace-match "<skip>\\1</skip>"))

    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *Note:\\(.*\\)$"
            nil t)
      (replace-match "<note>\\1</note>"))

;;     (goto-char (point-min))
;;     (while (search-forward-regexp
;;             "(\\([^)]+\\))"
;;             nil t)
;;       (replace-match "
;; <context>\\1</context>
;; "))

    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward-regexp
              "Note\\(.*\\)$"
              nil t)
        (replace-match "<note>\\1</note>")))

    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward-regexp
              "\\[HAND CARD\\([^]]*\\)\\]"
              nil t)
        (replace-match "<hand-card>\\1</hand-card>")))

    (goto-char (point-min))
    (let ((case-fold-search nil))
      (while (search-forward-regexp
              "^ *Days *$"
              nil t)
        (replace-match "<days/>")))

    ))

(defun qfirst50 ()
  ""
  (interactive)
  (let ((case-fold-search t)) ; or nil

    (goto-char (point-min))
    (while (search-forward-regexp
            "Please note that only the first 50 response categories are displayed in the PDF codebook. To view all response categories, please
 *analyze the data file in the statistical package of your choice (SAS, SPSS, Stata, R)."
            nil t)
      (replace-match "<note.first50/>"))))

(defun qresp ()
  ""
  (interactive)
  (let ((case-fold-search t)) ; or nil
    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *\\([-0-9md\\s.]+\\)\\s-+*\\([()\\/ a-z,/-()A-Z'-]+\\)\\s-+\\([0-9]+\\) *\\(.*\\)$"
            nil t)
      (replace-match "<r val=\"\\1\" lbl=\"\\2\" f=\"\\3\" pct=\"\\4\"/>"))))

(defun qresp2 ()
  ""
  (interactive)
  (let ((case-fold-search t)) ; or nil
    (goto-char (point-min))
    (while (search-forward-regexp
            "^ *\\([0-9 :\\s.]+\\)\\s-+*\\([\(\)\\/ a-z,/-()A-Z'-]+\\)\\s-+\\([0-9]+\\) *\\(.*\\)$"
            nil t)
      (replace-match "<r val=\"\\1\" lbl=\"\\2\" f=\"\\3\" pct=\"\\4\"/>"))))


(defun qxr ()
  ""
  (interactive)
  (let ((case-fold-search t)) ; or nil

    ;; idiom for string replacement within a region
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))

      (goto-char (point-min))
      (while (search-forward-regexp
              "^ *\\([0-9]+\\)\\s-+*\\([0-9]\\)\\([\\(\)\\/ a-z,/-()A-Z'-]+\\)\\s-+\\([0-9]+\\) *\\(.*\\)$"
              nil t)
        (replace-match "<r val=\"\\1\" lbl=\"\\2\\3\" f=\"\\4\" pct=\"\\5\"/>")))))

