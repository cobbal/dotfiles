;; acl2r.el   Basic Proof General instance for ACL2r
;;
;; Copyright (C) 2000 LFCS Edinburgh.
;; License:   GPL (GNU GENERAL PUBLIC LICENSE)
;; Author:    David Aspinall <David.Aspinall@ed.ac.uk>
;;
;; $Id$
;;
;; Needs improvement!
;;
;; See the README file in this directory for information.


(require 'proof-easy-config)            ; easy configure mechanism
(require 'proof-syntax)			; functions for making regexps

(proof-easy-config  'acl2r "ACL2r"
 proof-assistant-home-page       "http://www.cs.utexas.edu/users/moore/acl2"
 proof-prog-name		 "acl2r"

 proof-script-sexp-commands	 t
 proof-script-comment-start	";"

 proof-non-undoables-regexp "^:.*"

 proof-shell-annotated-prompt-regexp "ACL2S?(r)[ !]*>+"

 ;; proof-save-command-regexp	 "(def\\w+\\s "
 proof-save-command-regexp	 "(ISUREHOPETHISNEVERHAPPENS"
 proof-save-with-hole-regexp	 "(ISUREHOPETHISNEVERHAPPENS\\(\\)"
 ;; proof-goal-command-regexp       "(def\\w+\\s "
 ;; proof-save-with-hole-regexp     "(def\\w+[ \t\n]+\\(\\w+\\)"
 ;; proof-save-with-hole-result	 1
 ;; proof-goal-with-hole-regexp     "(def\\w+[ \t\n]+\\(\\w+\\)"
 ;; proof-goal-with-hole-result	 1
 proof-shell-error-regexp
 "^Error: \\|Error in TOP-LEVEL: \\|\\*+ FAILED \\*+"
 proof-shell-interrupt-regexp    "Correctable error: Console interrupt."

 proof-shell-quit-cmd            "(good-bye)"
 proof-shell-restart-cmd	 ":q\n:q\n:q\n(lp)\n"    ;; FIXME: maybe not?
 proof-info-command		 ":help"
 proof-undo-n-times-cmd		 ":ubt %s"  ;; shouldn't give errors
 proof-forget-id-command	 ":ubt *%s*"  ;; so use ubt not ubt!
 proof-context-command		 ":pbt :max"
 ;; proof-showproof-cmd		 ":pbt :here"

 proof-script-preprocess #'acl2-script-preprocess

 proof-shell-truncate-before-error nil

 proof-arbitrary-undo-positions t

 proof-shell-strip-crs-from-input nil

 ;; proof-completed-proof-behaviour 'closeany
 ;; proof-shell-proof-completed-regexp ""

 ;;
 ;; Syntax table entries for proof scripts  (FIXME: incomplete)
 ;;
 proof-script-syntax-table-entries
 '(?\[ "(]  "
   ?\] "([  "
   ?\( "()  "
   ?\) ")(  "
   ?.  "w   "
   ?_  "w   "
   ?-  "w   "
   ?>  "w   " ;; things treated as names can have > in them
   ?#  "'   "
   ?\' "'    "
   ?`  "'    "
   ?,  "'    "
   ?\| "."
   ?\; "<    "
   ?\n ">    "
   )

 ;; A tiny bit of syntax highlighting
 ;;
 proof-script-font-lock-keywords
 (append
  (list
   (proof-ids-to-regexp '("defthm" "defabbrev" "defaxiom" "defchoose"
			  "defcong" "defconst" "defdoc" "defequiv"
			  "defevaluator" "defpackage" "deflabel" "deftheory"
			  "implies" "equal" "and")))
  (if (boundp 'lisp-font-lock-keywords) ;; wins if font-lock is loaded
      lisp-font-lock-keywords))


 ;; End of easy config.
 )

;; Interrupts and errors enter another loop; break out of it
(add-hook
 'proof-shell-handle-error-or-interrupt-hook
 (lambda () (if (eq proof-shell-error-or-interrupt-seen 'interrupt)
		(proof-shell-insert ":q" nil))))

(defun acl2-pos-to-identifier (start)
 (format "pg-pos-%s"
  (+ start
   (save-excursion
    (goto-char start)
    (skip-chars-forward " \t\n")))))

(defun acl2-script-preprocess (filename start end cmd)
 (list
  (format "(with-output :off :all (defconst *%s* 0))"
   (acl2-pos-to-identifier start))
  cmd))

(defun acl2-add-element-advice (orig-fun idiomsym id span &optional name)
 (funcall orig-fun idiomsym id span
  (or name (acl2-pos-to-identifier (span-start span)))))
(advice-add 'pg-add-element :around #'acl2-add-element-advice)

;(defun pg-add-element (idiomsym id span &optional name)

;; (warn "ACL2 Proof General is incomplete!  Please help improve it!
;; Please add improvements at https://github.com/ProofGeneral/PG")


(provide 'acl2r)
