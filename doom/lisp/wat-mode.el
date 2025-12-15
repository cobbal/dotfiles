;;; wat-mode.el --- WAT editing mode    -*- lexical-binding: t; -*-
;; Adapted from scheme.el

;; Copyright (C) 1986-2025 Free Software Foundation, Inc.

;; Author: Bill Rozas <jinx@martigny.ai.mit.edu>
;; Adapted-by: Dave Love <d.love@dl.ac.uk>
;; Keywords: languages, lisp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; The major mode for editing Scheme-type Lisp code, very similar to
;; the Lisp mode documented in the Emacs manual.  `dsssl-mode' is a
;; variant of scheme-mode for editing DSSSL specifications for SGML
;; documents.  [As of Apr 1997, some pointers for DSSSL may be found,
;; for instance, at <URL:https://www.sil.org/sgml/related.html#dsssl>.]
;; All these Lisp-ish modes vary basically in details of the language
;; syntax they highlight/indent/index, but dsssl-mode uses "^;;;" as
;; the page-delimiter since ^L isn't normally a valid SGML character.
;;
;; For interacting with a Scheme interpreter See also `run-scheme' in
;; the `cmuscheme' package and also the implementation-specific
;; `xscheme' package.

;; Here's a recipe to generate a TAGS file for DSSSL, by the way:
;; etags --lang=scheme --regex='/[ \t]*(\(mode\|element\)[ \t
;; ]+\([^ \t(
;; ]+\)/\2/' --regex='/[ \t]*(element[ \t
;; ]*([^)]+[ \t
;; ]+\([^)]+\)[ \t
;; ]*)/\1/' --regex='/(declare[^ \t
;; ]*[ \t
;; ]+\([^ \t
;; ]+\)/\1/' "$@"

;;; Code:

(require 'lisp-mode)
(eval-when-compile 'subr-x)             ;For `named-let'.

(defvar wat-mode-syntax-table
  (let ((st (make-syntax-table))
         (i 0))
    ;; Symbol constituents
    ;; We used to treat chars 128-256 as symbol-constituent, but they
    ;; should be valid word constituents (Bug#8843).  Note that valid
    ;; identifier characters are Wat-implementation dependent.
    (while (< i ?0)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?9))
    (while (< i ?A)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?Z))
    (while (< i ?a)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))
    (setq i (1+ ?z))
    (while (< i 128)
      (modify-syntax-entry i "_   " st)
      (setq i (1+ i)))

    ;; Whitespace
    (modify-syntax-entry ?\t "    " st)
    (modify-syntax-entry ?\n ">   " st)
    (modify-syntax-entry ?\f "    " st)
    (modify-syntax-entry ?\r "    " st)
    (modify-syntax-entry ?\s "    " st)

    ;; These characters are delimiters but otherwise undefined.
    ;; Brackets and braces balance for editing convenience.
    (modify-syntax-entry ?\[ "(]  " st)
    (modify-syntax-entry ?\] ")[  " st)
    (modify-syntax-entry ?{ "(}  " st)
    (modify-syntax-entry ?} "){  " st)
    (modify-syntax-entry ?\| "\" 23bn" st)
    ;; Guile allows #! ... !# comments.
    ;; But SRFI-22 defines the comment as #!...\n instead.
    ;; Also Guile says that the !# should be on a line of its own.
    ;; It's too difficult to get it right, for too little benefit.
    ;; (modify-syntax-entry ?! "_ 2" st)

    ;; Other atom delimiters
    (modify-syntax-entry ?\" "\"   " st)
    (modify-syntax-entry ?' "'   " st)
    (modify-syntax-entry ?` "'   " st)

    ;; Special characters
    (modify-syntax-entry ?, "'   " st)
    (modify-syntax-entry ?@ "'   " st)
    (modify-syntax-entry ?# "' 14" st)
    (modify-syntax-entry ?\\ "\\   " st)

    ;; WAT comments
    (modify-syntax-entry ?\( "() 1nc" st)
    (modify-syntax-entry ?\; "< 23" st)
    (modify-syntax-entry ?\) ")( 4nc" st)
    (modify-syntax-entry ?\n ">" st)

    st))

(defvar wat-mode-abbrev-table nil)
(define-abbrev-table 'wat-mode-abbrev-table ())

(defun wat-mode-variables ()
  (set-syntax-table wat-mode-syntax-table)
  (setq local-abbrev-table wat-mode-abbrev-table)
  (setq-local paragraph-start (concat "$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local paragraph-ignore-fill-prefix t)
  (setq-local fill-paragraph-function 'lisp-fill-paragraph)
  ;; Adaptive fill mode gets in the way of auto-fill,
  ;; and should make no difference for explicit fill
  ;; because lisp-fill-paragraph should do the job.
  (setq-local adaptive-fill-mode nil)
  (setq-local indent-line-function 'lisp-indent-line)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local outline-regexp ";;; \\|(....")
  (setq-local add-log-current-defun-function #'lisp-current-defun-name)
  (setq-local comment-start ";")
  (setq-local comment-add 1)
  (setq-local comment-start-skip ";+[ \t]*")
  (setq-local comment-use-syntax t)
  (setq-local comment-column 40)
  (setq-local lisp-indent-function 'lisp-indent-function)
  (setq mode-line-process '("" wat-mode-line-process))
  (setq-local imenu-case-fold-search t)
  (setq-local imenu-syntax-alist '(("+-*/.<>=?!$%_&~^:" . "w")))
  (setq-local syntax-propertize-function #'wat-syntax-propertize)
  (setq font-lock-defaults
    '((wat-font-lock-keywords
        wat-font-lock-keywords-1 wat-font-lock-keywords-2)
       nil nil (("+-*/.<>=!?$%_&~^:" . "w") (?#. "w 14"))
       beginning-of-defun
       (font-lock-mark-block-function . mark-defun)))
  (setq-local prettify-symbols-alist lisp-prettify-symbols-alist)
  (setq-local lisp-doc-string-elt-property 'wat-doc-string-elt))

(defvar wat-mode-line-process "")

(defvar-keymap wat-mode-map
  :doc "Keymap for Wat mode.
All commands in `lisp-mode-shared-map' are inherited by this map."
  :parent lisp-mode-shared-map)

;;;###autoload
(define-derived-mode wat-mode prog-mode "Wat"
  "Major mode for editing Wat code.
Editing commands are similar to those of `lisp-mode'.

In addition, if an inferior Wat process is running, some additional
commands will be defined, for evaluating expressions and controlling
the interpreter, and the state of the process will be displayed in the
mode line of all Wat buffers.  The names of commands that interact
with the Wat process start with \"xwat-\" if you use the MIT
Wat-specific `xwat' package; for more information see the
documentation for `xwat-interaction-mode'.  Use \\[run-wat] to
start an inferior Wat using the more general `cmuwat' package.

Commands:
Delete converts tabs to spaces as it moves back.
Blank lines separate paragraphs.  Semicolons start comments.
\\{wat-mode-map}"
  (wat-mode-variables))

(defgroup wat nil
  "Editing Wat code."
  :link '(custom-group-link :tag "Font Lock Faces group" font-lock-faces)
  :group 'lisp)

(defcustom wat-mode-hook nil
  "Normal hook run when entering `wat-mode'.
See `run-hooks'."
  :type 'hook)

(defconst wat-font-lock-keywords-1
  (eval-when-compile
      (list
        ;;
        ;; Control structures.
        (cons
          (concat
            "\\<" (regexp-opt
                    '(
                       "func"
                       "import"
                       "param"
                       "result"
                       "memory"
                       "table"
                       "elem"
                       "data"

                       ;; control flow instructions
                       "block"
                       "br"
                       "br_if"
                       "br_table"
                       "call"
                       "drop"
                       "end"
                       "if" "then" "else"
                       "loop"
                       "nop"
                       "return"
                       "select"
                       "unreachable"
                       ) t)
            "\\>") 1)
        ))
  "Subdued expressions to highlight in Wat modes.")

(defconst wat-font-lock-keywords-2
  (append wat-font-lock-keywords-1
    (eval-when-compile
      (list
        ;;
        ;; Control structures.
        (cons
          (concat
            "\\<" (regexp-opt
                    '(
                       "func"
                       "import"
                       "param"
                       "result"
                       "memory"
                       "table"

                       ;; types
                       "i32" "i64" "f32" "f64"

                       ;; control flow instructions
                       "block"
                       "br"
                       "br_if"
                       "br_table"
                       "call" "call_indirect" "return_call" "return_call_indirect"
                       "drop"
                       "end"
                       "if" "then" "else"
                       "loop"
                       "nop"
                       "return"
                       "select"
                       "unreachable"

                       ;; memory instructions
                       "memory.grow"
                       "memory.size"
                       "i32.load" "i64.load" "f32.load" "f64.load"
                       "i32.load8_s" "i32.load8_u" "i32.load16_s" "i32.load16_u"
                       "i64.load8_s" "i64.load8_u" "i64.load16_s" "i64.load16_u" "i64.load32_s" "i64.load32_u"
                       "i32.store" "i64.store" "f32.store" "f64.store"
                       "i32.store8" "i32.store16" "i64.store8"
                       "i64.store16" "i64.store32"
                       "memory.copy"
                       "memory.fill"

                       ;; numeric instructions
                       ;; constants
                       "i32.const" "i64.const" "f32.const" "f64.const"
                       ;; comparison
                       "i32.eqz" "i32.eq" "i64.eqz" "i64.eq" "f32.eq" "f64.eq"
                       "i32.ne" "i64.ne" "f32.ne" "f64.ne"
                       "i32.gt_s" "i32.gt_u" "i64.gt_s" "i64.gt_u" "f32.gt" "f64.gt"
                       "i32.lt_s" "i32.lt_u" "i64.lt_s" "i64.lt_u" "f32.lt" "f64.lt"
                       "i32.ge_s" "i32.ge_u" "i64.ge_s" "i64.ge_u" "f32.ge" "f64.ge"
                       "i32.le_s" "i32.le_u" "i64.le_s" "i64.le_u" "f32.le" "f64.le"
                       ;; arithmetic
                       "i32.add" "i64.add" "f32.add" "f64.add"
                       "i32.sub" "i64.sub" "f32.sub" "f64.sub"
                       "i32.mul" "i64.mul" "f32.mul" "f64.mul"
                       "i32.div_s" "i32.div_u" "i64.div_s" "i64.div_u" "f32.div" "f64.div"
                       "i32.rem_s" "i32.rem_u" "i64.rem_s" "i64.rem_u"
                       ;; conversion
                       "i64.extend_i32_s" "i64.extend_i32_u"
                       "i32.wrap_i64"
                       "f64.promote_f32"
                       "f32.demote_f64"
                       "f32.convert_i32_s" "f32.convert_i32_u" "f32.convert_i64_s" "f32.convert_i64_u"
                       "f64.convert_i32_s" "f64.convert_i32_u" "f64.convert_i64_s" "f64.convert_i64_u"
                       "i32.trunc_f32_s" "i32.trunc_f32_u" "i32.trunc_f64_s" "i32.trunc_f64_u"
                       "i64.trunc_f32_s" "i64.trunc_f32_u" "i64.trunc_f64_s" "i64.trunc_f64_u"
                       "i32.reinterpret_f32" "i64.reinterpret_f64" "f32.reinterpret_i32" "f64.reinterpret_i64"
                       ;; floating point
                       ;; TODO: https://developer.mozilla.org/en-US/docs/WebAssembly/Reference/Numeric#floating_point_specific_instructions
                       ;; bitwise
                       "i32.and" "i64.and"
                       "i32.or" "i64.or"
                       "i32.xor" "i64.xor"
                       "i32.shl" "i64.shl"
                       "i32.shr_s" "i32.shr_u" "i64.shr_s" "i64.shr_u"
                       "i32.rotl" "i64.rotl"
                       "i32.rotr" "i64.rotr"
                       "i32.clz" "i64.clz"
                       "i32.ctz" "i64.ctz"
                       "i32.popcnt" "i64.popcnt"

                       ;; variable instructions
                       "local" "local.get" "local.set" "local.tee"
                       "global" "global.get" "global.set"
                       ) t)
            "\\>") 1)
        )))
  "Gaudy expressions to highlight in Wat modes.")

(defvar wat-font-lock-keywords wat-font-lock-keywords-1
  "Default expressions to highlight in Wat modes.")

(defun wat-syntax-propertize (beg end)
  (goto-char beg)
  ;; (wat-syntax-propertize-sexp-comment end)
  ;;(wat-syntax-propertize-regexp end)
  (funcall
    (syntax-propertize-rules
      ("\\(#\\);" (1 (prog1 "< cn"
                       (wat-syntax-propertize-sexp-comment end))))
      ("\\(#\\)/" (1 (when (null (nth 8 (save-excursion
                                          (syntax-ppss (match-beginning 0)))))
                       (put-text-property
                         (match-beginning 1)
                         (match-end 1)
                         'syntax-table (string-to-syntax "|"))
                       ;; (wat-syntax-propertize-regexp end)
                       nil))))
    (point) end))

(provide 'wat-mode)

;;; wat-mode.el ends here
