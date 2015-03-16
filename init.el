;; -*- no-byte-compile: t; lexical-binding: t -*-

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq ns-pop-up-frames nil)
(tool-bar-mode -1)

(blink-cursor-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "/usr/local/opt/coq/lib/emacs/site-lisp")

(setq el-get-notify-type 'message)
(unless (require 'el-get nil 'noerror)
 (add-to-list 'exec-path "/usr/local/bin")
 (with-current-buffer
  (url-retrieve-synchronously
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
  (let (el-get-master-branch)
   (goto-char (point-max))
   (eval-print-last-sexp))))

(require 'package)
(add-to-list 'package-archives
 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
 (el-get-elpa-build-local-recipes))

(el-get 'sync
 '(auto-complete
   coffee-mode
   clojure-mode
   d-mode
   evil
   exec-path-from-shell
   fsharp-mode
   gnu-apl-mode
   go-mode
   graphviz-dot-mode
   haskell-mode
   hy-mode
   markdown-mode
   paredit
   evil-paredit
   php-mode
   racket-mode
   rust-mode
   sml-mode
   swift-mode
   unicode-fonts))

(defun el-get-install-optionals ()
 (interactive)
 (dolist (pkg '(auctex
                ProofGeneral))
  (el-get-install pkg)))

(when (memq window-system '(mac ns))
 (setq frame-resize-pixelwise t)
 (add-hook 'window-setup-hook
  (lambda () (modify-frame-parameters nil '((fullscreen . maximized)))))
 (exec-path-from-shell-initialize))

(setq racket-mode-pretty-lambda nil)
(setq racket-program "/Applications/Racket/bin/racket")
(add-hook 'racket-mode-hook
 (lambda ()
  (setq-local eldoc-documentation-function nil)))

(require 'evil)
(evil-mode 1)
(define-key evil-motion-state-map [down-mouse-1] #'mouse-drag-region)
(setq evil-echo-state nil)
(setq-default evil-symbol-word-search t)
(setq evil-mode-line-format '(before . mode-line-frame-identification))
(global-undo-tree-mode -1)

(prefer-coding-system           'utf-8)
(set-default-coding-systems     'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(setq buffer-file-coding-system 'utf-8)

(setq evil-cross-lines t)
(setq undo-limit (round (* 1 1024 1024 1024)))
(setq undo-strong-limit (round (* 1.5 1024 1024 1024)))
(setq-default transient-mark-mode nil)
(setq vc-follow-symlinks t)
(setq visible-bell t)
(setq apropos-do-all t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq-default fill-column 80)
(setq-default tab-width 4)
(setq coffee-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default py-indent-offset 4)
(setq sgml-basic-offset 1)
(setq hamlet-basic-offset 1)
(setq lisp-indent-offset 1)
(setq fill-column 80)
(setq sentence-end-double-space nil)

(show-paren-mode 1)

(add-hook 'c-mode-common-hook
 (lambda ()
  (c-add-style "correct"
   '((c-basic-offset . 4)
     (c-offsets-alist . ((substatement-open . 0)
                         (defun-open . 0)
                         (innamespace . 0)
                         (inextern-lang . 0)
                         (case-label . 4)
                         (statement-case-open . 4)
                         (statement-case-intro . 4)
                         (inline-open . 0)
                         (brace-list-open . 0)))))
  (c-set-style "correct")
  t))

(setq initial-frame-alist '((width . 100) (height . 53) (top . 0) (left . 0)))
(setq default-frame-alist '((width . 100) (height . 53) (top . 0)))

(defun try-set-font (font)
 (ignore-errors (set-frame-font font nil t) t))

(or
 (try-set-font "Menlo 10")
 (when (eq window-system 'w32)
  (try-set-font "DejaVu Sans mono 8"))
 (try-set-font "DejaVu Sans mono 10")
 (try-set-font "Espresso mono 10")
 (try-set-font "Consolas 8"))

;;(define-key global-map [down-mouse-1] nil)
(global-set-key (kbd "C-c \\") "λ")
(global-set-key (kbd "M-u") #'insert-char)
(global-set-key (kbd "C-c s") #'query-replace-regexp)
(global-set-key (kbd "C-c q") #'refill-mode)
(global-set-key (kbd "C-c a") #'auto-complete-mode)
(global-set-key (kbd "C-c w") #'fixup-whitespace)
(global-set-key (kbd "C-c c") #'recompile)
(global-set-key (kbd "C-c C") #'compile)
(global-set-key (kbd "C-c C-c") #'comment-region)
(global-set-key (kbd "C-c u") #'revert-buffer)
(global-set-key (kbd "C-c ;") #'ispell-buffer)
(global-set-key (kbd "M-`") #'ff-find-other-file)
(global-set-key (kbd "M-h") #'ns-do-hide-emacs)
(global-set-key (kbd "RET") #'newline-and-indent)
(global-set-key (kbd "M-s") #'save-buffer)
(global-set-key (kbd " ") (lambda () (interactive) (insert ? ))) ;; nbsp
(global-set-key (kbd "C-c C-/") #'describe-char)

;; bind C-x 5 3 to be same as C-x 5 2
(define-key ctl-x-5-map "3" 'make-frame-command)

;;(eval-after-load "tex-mode"
 ;;'(define-key tex-mode-map (kbd "C-j") #'newline-and-indent))
(define-key evil-motion-state-map (kbd "K") nil)

(add-hook 'LaTeX-mode-hook
 (lambda ()
  (add-to-list 'LaTeX-indent-environment-list '("algorithmic" current-indentation))))

(mapc
 (lambda (x)
  (add-to-list 'load-path (expand-file-name x)))
 '("~/.emacs.d/lisp"
   "~/collab-mode"))

(require 'auto-complete)
(require 'auto-complete-config)

(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
;;(define-key ac-complete-mode-map viper-ESC-key 'viper-intercept-ESC-key)
(add-hook 'objc-mode-hook
 (lambda ()
  (run-at-time ".1 second" nil
   (lambda () (auto-complete-mode 1)))))

(define-key ac-mode-map (kbd "C-M-<tab>") 'auto-complete)

(setq ac-auto-start t)
(setq ac-dwim t)

(global-auto-complete-mode t)

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline-p 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
;;(define-key ac-completing-map (kbd "M-n") 'ac-next)
;;(define-key ac-completing-map (kbd "M-p") 'ac-previous)
(ac-linum-workaround)

(require 'objc-help)
(iphoneize)


;; borrowed from http://www.emacswiki.org/emacs/NxmlMode
(defun nxml-where ()
 "Display the hierarchy of XML elements the point is on as a path."
 (interactive)
 (let ((path nil))
  (save-excursion
   (save-restriction
    (widen)
    (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
            (condition-case nil
             (progn
              (nxml-backward-up-element) ; always returns nil
              t)
             (error nil)))
     (setq path (cons (xmltok-start-tag-local-name) path)))
    (if (called-interactively-p t)
     (message "/%s" (mapconcat 'identity path "/"))
     (format "/%s" (mapconcat 'identity path "/")))))))

(when (boundp 'global-linum-mode)
 (global-linum-mode t)
 (setq linum-format "%d "))
(column-number-mode t)

(require 'autoloaded)

(defun http-find-file (arg url)
 (interactive "P\nsURL: ")
 (message (format "%s %s" arg url))
 (let ((buffer-name (first (last (split-string url "/")))))
  (http-get url nil
   (lambda (proc message)
    (switch-to-buffer (process-buffer proc))
    (http-decode-buffer)
    (set-visited-file-name (concat "/tmp/" buffer-name)))
   nil buffer-name nil)))

(autoload 'glsl-mode "glsl-mode" nil t)

(dolist (a '(("\\.mm\\'" . objc-mode)
             ("\\.h\\'" . c++-mode)
             ("\\.swift\\'" . swift-mode)
             ("\\.\\(v\\|f\\|tc\\|te\\)sh\\'" . glsl-mode)
             ("\\.jsont\\'" . html-mode)
             ("\\.ijs\\'" . j-mode)
             ("\\.j\\'" . objj-mode)
             ("\\.js\\'" . js-mode)
             ("\\.julius\\'" . js-mode)
             ("\\.clj\\'" . clojure-mode)
             ("\\.nu\\'" .  nu-mode)
             ("[Nn]ukefile\\'" . nu-mode)
             ("[Mm]akefile." . makefile-mode)
             ("\\.json\\'" . js-mode)
             ("\\.cs\\'" . csharp-mode)
             ("\\.cl\\'" . lisp-mode)
             ("\\.fscr\\'" . smalltalk-mode)
             ("\\.rkt\\'" . racket-mode)
             ("\\.dart\\'" . dart-mode)
             ("\\.pro\\'" . qmake-mode)
             ("\\.coffee\\'" . coffee-mode)
             ("\\.ly\\'" . LilyPond-mode)
             ("\\.v\\'" . coq-mode)))
 (add-to-list 'auto-mode-alist a))

(add-hook 'js-mode-hook
 (lambda ()
;;; make emacs recognize the error format produced by jslint
  (set (make-local-variable 'compilation-error-regexp-alist)
   '(("^\\([a-zA-Z.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))
  (set (make-local-variable 'compile-command)
   (let ((file (file-name-nondirectory buffer-file-name)))
    (concat "/usr/share/jslint/jslint " file)))))

;; clojure-mode
(require 'clojure-mode)

;; slime
;; Borrowed from http://nklein.com/2010/05/getting-started-with-clojureemacsslime/
;;(require 'slime-autoloads)
;;(slime-setup '(slime-repl))
;;(setq slime-net-coding-system 'utf-8-unix)
;;(setq slime-lisp-implementations
 ;;'((sbcl ("sbcl"))
   ;;(ccl ("ccl"))
   ;;(clisp ("clisp"))))

;;(defmacro defslime-start (name mapping)
 ;;`(defun ,name ()
   ;;(interactive)
   ;;(let ((slime-default-lisp ,mapping))
    ;;(slime))))

;;(defslime-start ccl 'ccl)
;;(defslime-start sbcl 'sbcl)
;;(defslime-start clisp 'clisp)

;;(add-hook 'slime-mode-hook
 ;;(lambda ()
  ;;(setq slime-truncate-lines nil)
  ;;(slime-redirect-inferior-output)))

(require 'ido)
(setq ido-auto-merge-work-directories-length -1)

(defun byte-compile-all-in-emacs-d ()
 (interactive)
 (byte-recompile-directory (expand-file-name "~/.emacs.d") 0))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;(load "~/.emacs.d/el-get/haskell-mode/haskell-site-file")
(require 'hamlet-mode)

(add-hook 'haskell-mode-hook
 (lambda ()
  ;;(local-set-key (kbd "C-c c") #'haskell-compile)
  (require 'haskell-compile)
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (setq compilation-error-regexp-alist haskell-compilation-error-regexp-alist)
  '(add-to-list 'prettify-symbols-alist '("\\" . ?λ))
  '(prettify-symbols-mode t)))
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-hook 'python-mode-hook
 (lambda ()
  '(add-to-list 'prettify-symbols-alist '("lambda" . ?λ))
  '(prettify-symbols-mode t)))

(add-hook 'emacs-lisp-mode-hook
 (lambda ()
  '(add-to-list 'prettify-symbols-alist '("lambda" . ?λ))
  '(prettify-symbols-mode t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'visible-mark)

(global-visible-mark-mode t)

(setq proof-splash-enable nil)
(autoload 'coq-mode "coq" "Major mode for editing Coq vernacular." t)
;;(ignore-errors (load-file "~/.emacs.d/ProofGeneral/generic/proof-site.el"))
(setq coq-prog-args '("-emacs-U" "-I" "/Users/acobb/programs/cpdt/cpdt/src"))

(add-hook 'd-mode-hook
 (lambda ()
  (add-to-list
   'compilation-error-regexp-alist
   '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
     1 2 nil (3 . 4)))))

(setq emdroid-activity-creator "activityCreator.py")
(setq emdroid-tools-dir "/Users/acobb/Desktop/programs/android/tools/")

(eval-after-load "ido"
 '(progn
   (setq ido-create-new-buffer 'always)
   (setq ido-enable-flex-matching t)
   (setq ido-everywhere t)
   (ido-mode 'both)))

(load "~/.emacs.d/el-get/dash/dash.el")
(require 'apl-map)

(setq unicode-fonts-skip-font-groups nil)
(require 'unicode-fonts)
(unicode-fonts-setup)

(setq doc-view-resolution 200)

(defun set-lisp-indent-offset (n)
 (interactive "Nlisp-indent-offset: ")
 (set (make-local-variable 'lisp-indent-offset) n))

(server-start)

(ignore-errors
 (load "~/programs/boxfu/boxfu.el" t))

;; (load-theme 'manoj-dark)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(describe-char-unidata-list
   (quote
    (name old-name general-category decomposition uppercase lowercase)))
 '(graphviz-dot-auto-indent-on-semi nil)
 '(safe-local-variable-values
   (quote
    ((eval visible-mode t)
     (eval auto-fill-mode t)
     (encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
