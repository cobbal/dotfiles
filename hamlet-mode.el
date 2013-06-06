; An emacs mode for editing files written in Hamlet, Yesod's HTML-like templating language.
(defvar hamlet-mode-hook nil)

; tab stops from column 2 to 80. more than that, and you're doing it wrong.
(add-hook 'hamlet-mode-hook '(lambda ()
          (setq tab-stop-list (number-sequence 2 80 2))))


; autoload on .hamlet and .lucius files
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . hamlet-mode))
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . css-mode))

(defconst hamlet-name-regexp "[_:[:alpha:]][-_.:[:alnum:]]*")

(defconst hamlet-font-lock-highlighting
  `(
    ;; tag names
    (,(concat "</?\\(" hamlet-name-regexp "\\)") 1 font-lock-function-name-face)
    ;; attributes; the three groups, in order, are attribute name,
    ;; attribute string value, and .class or #id
    (,(concat "\\(?:^\\|[ \t]\\)\\(?:\\("
              hamlet-name-regexp "\\)=\\(\\sw*\\)\\|\\([.#]"
              hamlet-name-regexp "\\)\\)")
     (1 font-lock-variable-name-face nil t)
     (2 font-lock-string-face nil t)
     (3 font-lock-variable-name-face nil t)
     )
    ;; variable interpolation
    ("\\([@^#]{[^}]+}\\)" 1 font-lock-preprocessor-face t)
    ;; control flow
    ("^[ \t]*\\($\\w+\\)" 1 font-lock-keyword-face)
    )
)

(defvar hamlet-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?\\ "w" st)
    st)
)

(defun hamlet-mode-auto-fill-function ()
  (when (> (current-column) fill-column)
    ;; Split at 2 chars before fill-column. This is so that we can insert a
    ;; space and a hash
    (while (> (current-column) (- fill-column 2))
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((auto-fill-function nil)
          (indent (current-indentation)))
      (insert "#")
      (newline)
      (indent-to indent)
      (insert "\\")
      (end-of-line))))

(defcustom hamlet-basic-offset 2
 "Specities the basic indentation level for `hamlet-indent-line'."
 :type 'integer
 :group 'hamlet)

(defun hamlet-max-indentation ()
 "Calculate max Hamlet indentation for line at point."
 (save-excursion
  (beginning-of-line)
  (if (bobp)
   0
   (previous-line)
   (+ (current-indentation) hamlet-basic-offset))))

;; liberally borrowed from python.el
(defun hamlet-indent-line ()
 (interactive)
 (let ((indent (current-indentation))
       (max-indent (hamlet-max-indentation)))
  (if (and
       (eq this-command 'indent-for-tab-command)
       (eq last-command this-command))
   (if (= 0 max-indent)
    (message "Sole indentation")
    (progn
     (beginning-of-line)
     (delete-horizontal-space)
     (indent-to
      (if (= indent 0)
       max-indent
       (- indent hamlet-basic-offset)))))
   (hamlet-indent-line-1))))

(defun hamlet-indent-line-1 (&optional leave)
 (let ((indent (current-indentation))
       (max-indent (hamlet-max-indentation))
       (pos (- (point-max) (point))))
  (when (or (> indent max-indent) (not leave))
   (beginning-of-line)
   (delete-horizontal-space)
   (indent-to max-indent))))

(define-derived-mode hamlet-mode text-mode "Hamlet"
 "Major mode for editing Hamlet files."
 (kill-local-variable 'normal-auto-fill-function)
 (kill-local-variable 'font-lock-defaults)
 (set (make-local-variable 'font-lock-defaults)
  '(hamlet-font-lock-highlighting))
 (set (make-local-variable 'normal-auto-fill-function)
  'hamlet-mode-auto-fill-function)
 (set (make-local-variable 'indent-line-function) #'hamlet-indent-line))


(provide 'hamlet-mode)
