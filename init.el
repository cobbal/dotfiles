;; -*- no-byte-compile: t; lexical-binding: t -*-

;; Do this first to minimize color flash
(load-theme 'manoj-dark)

(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)
(setq ns-pop-up-frames nil)
(tool-bar-mode -1)

(blink-cursor-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)

(dolist (x '("~/.emacs.d/lisp"
             "~/.emacs.d/el-get/el-get"
             "/usr/local/opt/coq/lib/emacs/site-lisp"))
 (add-to-list 'load-path (expand-file-name x)))

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
   dash-at-point
   evil
   exec-path-from-shell
   fsharp-mode
   glsl-mode
   gnu-apl-mode
   go-mode
   graphviz-dot-mode
   haskell-mode
   hy-mode
   markdown-mode
   misc-cmds
   php-mode
   racket-mode
   rust-mode
   sml-mode
   swift-mode
   unicode-fonts))

(package-initialize)

(defun el-get-install-optionals ()
 (interactive)
 (dolist (pkg '(auctex
                ProofGeneral))
  (el-get-install pkg)))

(defun racket-rain-down-judgment ()
 (interactive)
 (goto-char (point-at-eol))
 (let ((col (current-column)))
  (newline-and-indent)
  (insert
   (make-string
    (- col (current-column))
    ?-))))

(setq racket-mode-pretty-lambda nil)
(setq racket-program "/Applications/Racket/bin/racket")
(add-hook 'racket-mode-hook
 (lambda ()
  (setq prettify-symbols-alist '())
  (add-to-list 'prettify-symbols-alist '(")" . ?())
  (add-to-list 'prettify-symbols-alist '("(" . ?)))
  (add-to-list 'prettify-symbols-alist '("{" . ?}))
  (add-to-list 'prettify-symbols-alist '("}" . ?{))
  (add-to-list 'prettify-symbols-alist '("[" . ?]))
  (add-to-list 'prettify-symbols-alist '("]" . ?[))
  ;(prettify-symbols-mode t)
  (setq-local eldoc-documentation-function nil)
  (local-set-key (kbd "C-M-d") #'racket-visit-definition)
  (local-set-key (kbd "C-c C--") #'racket-rain-down-judgment)))

(require 'evil)
(evil-mode 1)
(setq evil-cross-lines t)
(define-key evil-motion-state-map [down-mouse-1] #'mouse-drag-region)
(define-key evil-motion-state-map (kbd "K") nil)
(add-to-list 'evil-intercept-maps '(compilation-mode-map))
(setq evil-echo-state nil)
(setq-default evil-symbol-word-search t)
(setq evil-mode-line-format '(before . mode-line-frame-identification))
(global-undo-tree-mode -1)

(prefer-coding-system           'utf-8)
(set-default-coding-systems     'utf-8)
(set-terminal-coding-system     'utf-8)
(set-keyboard-coding-system     'utf-8)
(setq buffer-file-coding-system 'utf-8)

(setq undo-limit (round (* 1 1024 1024 1024)))
(setq undo-strong-limit (round (* 1.5 1024 1024 1024)))
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

(when (memq window-system '(mac ns))
 (setq frame-resize-pixelwise t)
 (let ((fullscreen-mode 'maximized))
  (when (> (x-display-pixel-width) 1440) ;; crude test for multiple displays
   (setq initial-frame-alist `((left + -2000) . ,initial-frame-alist))
   (setq fullscreen-mode 'fullscreen))
  ;;(setq ns-use-native-fullscreen nil)
  (setq initial-frame-alist
   `((fullscreen . ,fullscreen-mode) . ,initial-frame-alist)))
 (exec-path-from-shell-initialize))


(defun try-set-font (font)
 (ignore-errors (set-frame-font font nil t) t))

(or
 (try-set-font "Menlo 13")
 (try-set-font "Menlo 11")
 (when (eq window-system 'w32)
  (try-set-font "DejaVu Sans mono 8"))
 (try-set-font "DejaVu Sans mono 11")
 (try-set-font "Espresso mono 11")
 (try-set-font "Consolas 10"))

(require 'frame-focus-hints)
(require 'transpose-window-splits)

(defun unload-enabled-themes ()
 (interactive)
 (mapcar #'disable-theme custom-enabled-themes))

(defadvice load-theme (before theme-dont-propagate activate)
 (unload-enabled-themes))

(defun first-error ()
 (interactive)
 (next-error 1 t))

(defun other-window-previous (count)
 (interactive "p")
 (other-window (- count)))

(defun google-chrome-goto-location ()
 (interactive)
 (start-process "launch-browser" nil "osascript"
  "-e"
  (concat
   "tell application \"System Events\" to click menu item \"Open "
   "Location…\" of menu \"File\" of menu bar item \"File\" of "
   "menu bar 1 of process \"Google Chrome\"")
  "-e"
  "tell application \"Google Chrome\" to activate"))

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
(global-set-key (kbd " ") " ") ;; nbsp -> normal space
(global-set-key (kbd "C-c C-/") #'describe-char)
(global-set-key (kbd "M-g M-f") #'first-error)
(global-set-key (kbd "M-k") #'evil-scroll-up)
(global-set-key (kbd "M-j") #'evil-scroll-down)
(global-set-key (kbd "C-x C-o") #'other-window-previous)
(global-set-key (kbd "M-d") #'dash-at-point)
(global-set-key (kbd "<C-return>") #'indent-new-comment-line)
(global-set-key (kbd "M-l") #'google-chrome-goto-location)
(dolist (map (list evil-normal-state-map evil-motion-state-map))
 (define-key map (kbd "C-w ;") #'transpose-window-splits))

;; bind C-x 5 3 to be same as C-x 5 2
(define-key ctl-x-5-map "3" 'make-frame-command)

;; (eval-after-load "tex-mode"
;;  '(define-key tex-mode-map (kbd "C-j") #'newline-and-indent))

(add-hook 'LaTeX-mode-hook
 (lambda ()
  (add-to-list 'LaTeX-indent-environment-list '("algorithmic" current-indentation))))

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

(define-key ac-menu-map (kbd "RET") nil)


(setq ac-auto-start t)
(setq ac-dwim t)
(global-auto-complete-mode t)

(global-hl-line-mode t)
(set-face-foreground 'hl-line nil)

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline-p 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
;;(define-key ac-completing-map (kbd "M-n") 'ac-next)
;;(define-key ac-completing-map (kbd "M-p") 'ac-previous)
(ac-linum-workaround)

;;(require 'objc-help)
;;(iphoneize)

(dolist (l '((racket-mode . "racket")))
 (add-to-list 'dash-at-point-mode-alist l))

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


(require 'ido)
(setq ido-auto-merge-work-directories-length -1)

(defun byte-compile-all-in-emacs-d-lisp ()
 (interactive)
 (byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;(load "~/.emacs.d/el-get/haskell-mode/haskell-site-file")
;;(require 'hamlet-mode)

(add-hook 'haskell-mode-hook
 (lambda ()
  (require 'haskell-compile)
  (local-set-key (kbd "C-c c") #'recompile)
  (set (make-local-variable 'compile-command)
   (format haskell-compile-command
    (file-name-nondirectory buffer-file-name)))
  (turn-on-haskell-doc-mode)
  (turn-on-haskell-indentation)
  (setq compilation-error-regexp-alist haskell-compilation-error-regexp-alist)
  '(add-to-list 'prettify-symbols-alist '("\\" . ?λ))
  '(prettify-symbols-mode t)))
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;; (add-hook 'python-mode-hook
;;  (lambda ()
;;   (add-to-list 'prettify-symbols-alist '("lambda" . ?λ))
;;   (prettify-symbols-mode t)))

;; (add-hook 'emacs-lisp-mode-hook
;;  (lambda ()
;;   (add-to-list 'prettify-symbols-alist '("lambda" . ?λ))
;;   (prettify-symbols-mode t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defface my-visible-mark-face-1
  `((t (:background "plum4" :foreground "white")))
  "Face for the mark."
  :group 'visible-mark)

(setq visible-mark-faces `(my-visible-mark-face-1))

(require 'visible-mark)
(global-visible-mark-mode t)

(setq proof-splash-enable nil)
(ignore-errors (load-file "~/.emacs.d/el-get/ProofGeneral/ProofGeneral/generic/proof-site.el"))
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

(require 'dash)
(require 'apl-map)

(setq unicode-fonts-skip-font-groups nil)
(require 'unicode-fonts)
(unicode-fonts-setup)

(setq doc-view-resolution 200)

(setq graphviz-dot-auto-indent-on-semi nil)

(defun set-lisp-indent-offset (n)
 (interactive "Nlisp-indent-offset: ")
 (set (make-local-variable 'lisp-indent-offset) n))

(server-start)

(require 'boxfu)

;;(evil-transient-mark -1)
;;(transient-mark-mode -1)

(defun create-tags (dir-name)
 "Create tags file."
 (interactive "DDirectory: ")
 (shell-command
  (format "ctags -e -R %s" (directory-file-name dir-name))))

(define-key evil-normal-state-map (kbd "M-.") nil)

(defadvice find-tag (around refresh-etags activate)
 "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
 (let ((extension (file-name-extension (buffer-file-name))))
  (condition-case err
   ad-do-it
   (error (and (buffer-modified-p)
           (not (ding))
           (y-or-n-p "Buffer is modified, save it? ")
           (save-buffer))
    (er-refresh-etags extension)
    ad-do-it))))

(defun er-refresh-etags (&optional extension)
 "Run etags on all peer files in current dir and reload them silently."
 (interactive)
 (shell-command (format "etags *.%s" (or extension "el")))
 (let ((tags-revert-without-query t))  ; don't query, revert silently
  (visit-tags-table default-directory nil)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(describe-char-unidata-list
   (quote
    (name old-name general-category decomposition uppercase lowercase)))
 '(package-selected-packages
   (quote
    (misc-cmds hl-spotlight unicode-enbox racket-mode gnu-apl-mode)))
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
