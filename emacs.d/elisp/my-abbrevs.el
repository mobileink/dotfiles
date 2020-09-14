;; -*- coding: utf-8; lexical-binding: t; -*-
;; sample use of abbrev

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(

    ;; net abbrev
    ("afaik" "as far as i know" )
    ("atm" "at the moment" )
    ;;;
    ))

;; define abbrev for specific major mode
;; the first part of the name should be the value of the variable major-mode of that mode
;; e.g. for go-mode, name should be go-mode-abbrev-table
(define-abbrev-table 'nxml-mode-abbrev-table
  '(
    ("g3" "package main
import \"fmt\"
func main() {
        fmt.Println(\"3\")
}")

    ("فم"
     "<فم>")
    ("if" "if x < 0 { 3 }")
    ("r" "return")
    ("ps" "+")
    ("eq" "==")
    ("pt" "fmt.Println(3)")
    ("fu" "func(x int) int { return 1 }")
    ("v" "var = 3")

    ;;

    ))

(set-default 'abbrev-mode t)

(setq save-abbrevs nil)
