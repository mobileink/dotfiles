;;; keys.el --- key mappings

;; Copyright (C) 2006

;; Author:  <gar@arabink.com>
;; Keywords:

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;

;;; Code:


(define-abbrev-table 'global-abbrev-table '(
    ("logtop" "⊤" nil 0)
    ("logbot" "⊥" nil 0)
    ("logand" "∧" nil 0)
    ("nand"   "⊼" nil 0) ;; 22BC NAND
    ("logor"  "∨" nil 0)
    ("xor"    "⊻" nil 0) ;; 22BB XOR
    ("lognot" "¬" nil 0)

    ("forall"  "∀" nil 0)
    ("thereis" "∃" nil 0) ;; 2203 THERE EXISTS
    ("nexist"  "∄" nil 0) ;; 2204 THERE DOES NOT EXIST

    ;; inferences
    ("derive" "⊢" nil 0) ;; 22A2 RIGHT TACK
    ("conseq" "⊨" nil 0) ;; 22A8 TRUE
    ("models" "⊧" nil 0) ;; 22A7 MODELS

    ;; sets
    ("elt"      "∈" nil 0) ;; 2208 ELEMENT OF
    ("nelt"     "∉" nil 0) ;; 2209 NOT AN ELEMENT OF
    ("containselt" "∋" nil 0) ;; 220B CONTAINS AS ELEMENT
    ("nelt"       "∉" nil 0) ;; 2209 NOT AN ELEMENT OF
    ("intersect"  "∩" nil 0) ;; 2229 INTERSECTION
    ("union"      "∪" nil 0) ;; 222A UNION
    ("supset"     "⊃" nil 0) ;; 2283 SUPERSET OF

    ("compos"     "∘" nil 0) ;; 2218 RING OPERATOR
    ("bullet"     "∙" nil 0) ;; 2219 BULLET OPERATOR
    ("middot"     "·" nil 0) ;; 00B7 MIDDLE DOT

    ;; equality and equivalence
    ("eq3"     "≡" nil 0) ;; 2261 IDENTICAL TO (3 lines)
    ("tilde"   "∼" nil 0) ;; 223C TILDE OPERATOR
    ("sim"     "≈" nil 0) ;; 2248 AMOST EQUAL TO
    ("nsim"    "≉" nil 0) ;; 2249 NOT AMOST EQUAL TO
    ("approx"  "≅" nil 0) ;; 2245 APPROXIMATELY EQUAL TO

    ;; ordering
    ("prec"    "≺" nil 0) ;; 227A PRECEDES
    ("succ"    "≻" nil 0) ;; 227B SUCCEEDS

    ;; delimiters
    ("[["     "⟦" nil 0) ;; 27E6 MATHEMATICAL LEFT WHITE SQUARE BRACKET
    ("]]"     "⟧" nil 0) ;; 27E7 MATHEMATICAL RIGHT WHITE SQUARE BRACKET
    ("langle" "⟨" nil 0) ;; 27E8 MATHEMATICAL LEFT ANGLE BRACKET
    ("rangle" "⟩" nil 0) ;; 27E9 MATHEMATICAL RIGHT ANGLE BRACKET
    ("l<<"    "⟪" nil 0) ;; 27EA MATHEMATICAL LEFT DOUBLE ANGLE BRACKET
    ("r>>"    "⟫" nil 0) ;; 27EB MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET
    ("lquine" "⌜" nil 0) ;; 231C TOP LEFT  CORNER
    ("rquine" "⌝" nil 0) ;; 231D TOP RIGHT CORNER
    ("lceil" "⌈"  nil 0) ;; 2308 LEFT  CEILING
    ("rceil" "⌉"  nil 0) ;; 2309 RIGHT CEILING

    ;; Zed notation
    ("lbind"     "⦉"  nil 0) ;; 2989 Z NOTATION LEFT BINDING BRACKET
    ("rbind"     "⦊"  nil 0) ;; 298A Z NOTATION RIGHT BINDING BRACKET
    ("zcomp"     "⨟"  nil 0) ;; 2A1F Z NOTATION SCHEMA COMPOSITION
    ("zproj"     "⨡"  nil 0) ;; 2A21 Z NOTATION PROJECTION

    ;; arrows
    ("rar"    "→" nil 0)
    ("rrar"   "⇒" nil 0)
    ("lar"    "←" nil 0)
    ("llar"   "⇐" nil 0) ;; left double arrow
    ("lrar"   "↔" nil 0) ;; 2194
    ("llrrar" "⇔" nil 0) ;; left right double arrow

    ("oplus"  "⊕" nil 0) ;; 2295 circled plus
    ("otimes" "⊗" nil 0) ;; 2297 circled times
    ("sqrt"   "√" nil 0) ;; 221A SQUARE ROOT
    ("star"   "⋆" nil 0) ;; 22C5 STAR OPERATOR
    ("bstar"  "★" nil 0) ;; 22605 BLACK STAR
    ("*6"     "✶" nil 0)  ;; 2736 SIX POINTED BLACK STAR
    ("wstar"  "☆" nil 0) ;; 226056 WHITE STAR

    ("possible" "⋄" nil 0) ;; 22C4 modal operator "it is possible that"
    ("necessar" "◻" nil 0) ;; 22FB modal operator "it is necessary that"
    ))

(abbrev-mode 1)

;; (global-set-key [\C-c \C-c] 'comment-region)

(global-set-key (kbd "C-x g") 'magit-status)

;; todo: make this elisp-mode specific
(global-set-key [\C-c\C-r] 'eval-region)

;; (global-set-key [?\C-c ?\C-c] 'comment-region)

(global-set-key [C-M-\\] 'indent-region)
(global-set-key [\C-/] 'undo)

(global-set-key [?\C-h] 'delete-backward-char)
(global-set-key [?\M-?] 'help)

;(global-set-key [f2] 'delete-indentation)
;; (global-set-key [f2] 'biosketch-mode)

(global-set-key [f4] 'enlarge-window)

(global-set-key [f5] 'isearch-forward-regexp)
(global-set-key [C-f5] 'query-replace-regexp)

(global-set-key [f6] 'bookmark-jump)
;; C-f6 reserved for major modes

(global-set-key [f7] 'scroll-down)
(global-set-key [C-f7] 'beginning-of-buffer)

(global-set-key [f8] 'scroll-up)
(global-set-key [C-f8] 'end-of-buffer)

(global-set-key [f9] 'other-window)
(global-set-key [\C-f9] 'delete-other-windows)

(global-set-key [f10] 'toggle-show-trailing-whitespace)
(global-set-key [(C-f10)] 'delete-trailing-whitespace)

(global-set-key [(f11)]        'cycle-buffer-backward)
(global-set-key [(C-f11)]  'cycle-buffer-backward-permissive)

(global-set-key [(f12)]       'cycle-buffer)
(global-set-key [(C-f12)] 'cycle-buffer-permissive)

;; (global-set-key [(S-f11)] 'planner-create-task-from-buffer)

;;; keys.el ends here
