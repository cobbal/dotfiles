(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq ns-pop-up-frames nil)
(tool-bar-mode -1)

(blink-cursor-mode 1)
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq viper-mode t)
(require 'viper)

(setq transient-mark-mode nil)
(setq vc-follow-symlinks t)
(setq visible-bell t)

(prefer-coding-system           'utf-8)
(set-default-coding-systems     'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(setq buffer-file-coding-system 'utf-8)

(setq apropos-do-all t)
(setq inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq tab-width 4)
(setq coffee-tab-width 4)
(setq viper-shift-width 4)
(setq-default indent-tabs-mode nil)
(setq-default py-indent-offset 4)
(setq sgml-basic-offset 1)
(setq hamlet-basic-offset 1)
(setq lisp-indent-offset 1)
(show-paren-mode 1)

;; Sea lion hack
(when (string= default-directory "/Applications/")
 (setq default-directory (expand-file-name "~/")))

(defconst my-c-style
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

(add-hook 'c-mode-common-hook
 '(lambda ()
   (c-add-style "my" my-c-style t)))

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(setq el-get-notify-type 'message)
(unless (require 'el-get nil 'noerror)
 (with-current-buffer
  (url-retrieve-synchronously
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
  (let (el-get-master-branch)
   (goto-char (point-max))
   (eval-print-last-sexp))))


(el-get 'sync
 '(vimpulse
   haskell-mode
   auto-complete
   coffee-mode
   clojure-mode))

(setq initial-frame-alist '((width . 100) (height . 53) (top . 0) (left . 300)))
(setq default-frame-alist '((width . 100) (height . 53) (top . 0)))

(if (x-display-list)
 (catch 'break
  (dolist (font '("-apple-Espresso mono-medium-r-normal--0-0-0-0-m-0-iso10646-1"
                  "-unknown-DejaVu Sans mono-normal-normal-normal-*-*-*-*-*-*-0-iso10646-1"))
   (when (x-list-fonts font)
    (add-to-list 'default-frame-alist (cons 'font font))
    (throw 'break nil)))))

(define-key global-map [down-mouse-1] nil)
(global-set-key (kbd "<f9>") "λ")
(global-set-key (kbd "M-u") 'ucs-insert)
(global-set-key (kbd "C-c s") 'query-replace-regexp)
(global-set-key (kbd "C-c a") 'auto-complete-mode)
(global-set-key (kbd "C-c w") 'fixup-whitespace)
(global-set-key (kbd "C-c c") 'recompile)
(global-set-key (kbd "C-c C") 'compile)
(global-set-key (kbd "C-c C-c") 'comment-region)
(global-set-key (kbd "C-c u") 'revert-buffer)
(global-set-key (kbd "C-c ;") 'ispell)
(global-set-key (kbd "M-`") 'ff-find-other-file)
(global-set-key (kbd "M-h") 'ns-do-hide-emacs)
(global-set-key (kbd "M-RET") 'ns-toggle-fullscreen)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "<ns-drag-file>")
 (lambda ()
  (interactive)
  (let ((f (car ns-input-file)))
   (setq ns-input-file (cdr ns-input-file))
   (find-file f))))
;; bind C-x 5 3 to be same as C-x 5 2
(define-key ctl-x-5-map "3" 'make-frame-command)

(eval-after-load "tex-mode"
 '(define-key tex-mode-map (kbd "C-j") #'newline-and-indent))

(mapc
 (lambda (x)
  (add-to-list 'load-path (expand-file-name x)))
   '("~/.emacs.d"
     "~/collab-mode"))

(require 'vimpulse)

(require 'auto-complete)
(require 'auto-complete-config)

(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
(define-key ac-complete-mode-map viper-ESC-key 'viper-intercept-ESC-key)
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

;; Function to make brace highlighting like Vim's
;; Contributed by Alessandro Piras
(require 'advice)
'(defadvice show-paren-function (around viper-shop-paren-function activate)
 (let ((close-chars (list ?\) ?\] ?\})))
  (if viper-vi-basic-minor-mode
   (cond
    ((member (char-after (point)) close-chars)
     (save-excursion
      (forward-char)
      ad-do-it))
    ((member (char-after (- (point) 1)) close-chars) nil)
    (t ad-do-it))
   ad-do-it)))

(when (boundp 'global-linum-mode)
 (global-linum-mode t)
 (setq linum-format "%d "))

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

(setq auto-mode-alist
 (append
  '(("\\.mm\\'" . objc-mode)
    ("\\.h\\'" . c++-mode)
    ("\\.[vf]sh\\'" . glsl-mode)
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
    ("\\.rkt\\'" . scheme-mode)
    ("\\.dart\\'" . dart-mode)
    ("\\.pro\\'" . qmake-mode)
    ("\\.coffee\\'" . coffee-mode)
    ("\\.ly\\'" . LilyPond-mode))
  auto-mode-alist))

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

(defun byte-compile-all-in-emacs-d ()
 (interactive)
 (mapcar
  (lambda (file-name)
   (list file-name (byte-compile-file file-name)))
  (split-string (shell-command-to-string "find ~/.emacs.d -name '*.el' -print0") "\0" t)))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(load "~/.emacs.d/el-get/haskell-mode/haskell-site-file")
(require 'hamlet-mode)

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(require 'visible-mark)

(global-visible-mark-mode t)

(server-start)

(setq emdroid-activity-creator "activityCreator.py")
(setq emdroid-tools-dir "/Users/acobb/Desktop/programs/android/tools/")
(setq fill-column 80)
(eval-after-load "ido"
 '(progn
   (setq ido-create-new-buffer 'always)
   (setq ido-enable-flex-matching t)
   (setq ido-everywhere t)
   (ido-mode 'both)))
(setq safe-local-variable-values '((encoding . utf-8)))
