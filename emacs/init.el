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
(setq ad-redefinition-action 'accept) ;; silence advice warning about redefinition

(dolist (x '("~/.emacs.d/lisp"
             "~/.emacs.d/el-get/el-get"
             "~/.emacs.d/el-get/clang-complete-async"
             "/usr/local/opt/coq/lib/emacs/site-lisp"
             "~/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/"))
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

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

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
   avy
   cmake-mode
   coffee-mode
   clojure-mode
   d-mode
   dash-at-point
   evil
   ;; exec-path-from-shell
   fsharp-mode
   glsl-mode
   gnu-apl-mode
   go-mode
   graphviz-dot-mode
   haskell-mode
   hy-mode
   markdown-mode
   misc-cmds
   nix-mode
   php-mode
   racket-mode
   rust-mode
   scala-mode2
   sexp-rewrite
   sml-mode
   swift-mode
   unicode-fonts))

(package-initialize)

(defun el-get-install-optionals ()
 (interactive)
 (dolist (pkg '(auctex
                ProofGeneral
                clang-complete-async))
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
  (require 'racket-rewrites)
  (local-set-key (kbd "C-c d") sexprw-mode-keymap)
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
(setq inhibit-startup-echo-area-message "acobb")
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq-default tab-width 4)
(setq coffee-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default py-indent-offset 4)
(setq sgml-basic-offset 1)
(setq hamlet-basic-offset 1)
(setq lisp-indent-offset 1)
(setq-default fill-column 80)
(setq fill-column 80)
(setq sentence-end-double-space nil)

(defun my-terminal-visible-bell ()
 "A friendlier visual bell effect."
 (invert-face 'mode-line)
 (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil
 ring-bell-function #'my-terminal-visible-bell)

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
   '(setq fullscreen-mode 'fullscreen))
  ;;(setq ns-use-native-fullscreen nil)
  (setq initial-frame-alist
   `((fullscreen . ,fullscreen-mode) . ,initial-frame-alist))))
 ;; (require 'exec-path-from-shell)
 ;; (push "GOPATH" exec-path-from-shell-variables)
 ;; (exec-path-from-shell-initialize)


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

(defcustom compile-always-comint nil "")
(defun my-compile-advice (orig-fun command &optional mode &rest args)
 (apply orig-fun command (or compile-always-comint mode) args))
(advice-add 'compilation-start :around #'my-compile-advice)

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

(defun inc-char-at-point (n)
 (interactive "p")
 (save-excursion
  (let ((c (char-after (point))))
   (delete-backward-char -1)
   (insert (+ c n)))))

(defun dec-char-at-point (n)
 (interactive "p")
 (inc-char-at-point (- n)))

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
(global-set-key (kbd "C-c C--") #'dec-char-at-point)
(global-set-key (kbd "C-c C-=") #'inc-char-at-point)
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
;; (global-set-key (kbd "M-l") #'google-chrome-goto-location)
(global-set-key (kbd "<C-M-tab>") 'clang-format-region)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-M-e") nil)
(dolist (map (list evil-normal-state-map evil-motion-state-map))
 (define-key map (kbd "C-w ;") #'transpose-window-splits))
(eval-after-load "compile"
 '(define-key compilation-mode-map (kbd "g") nil))

;; bind C-x 5 3 to be same as C-x 5 2
(define-key ctl-x-5-map (kbd "3") 'make-frame-command)

(avy-setup-default)

(add-hook 'LaTeX-mode-hook
 (lambda ()
  (set (make-local-variable 'before-save-hook) nil)
  (add-to-list 'LaTeX-indent-environment-list '("algorithmic" current-indentation))))

(require 'auto-complete)
(require 'auto-complete-config)

(defun set-clang-ac-sources ()
 (require 'auto-complete-clang-async)
 (setq ac-clang-complete-executable "~/.emacs.d/el-get/clang-complete-async/clang-complete")
 (setq ac-sources '(ac-source-clang-async))
 (ac-clang-launch-completion-process))

(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
(define-key ac-completing-map (kbd "RET") nil)
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
;; (add-hook 'c-mode-hook #'set-clang-ac-sources)
;; (add-hook 'c++-mode-hook #'set-clang-ac-sources)
;; (add-hook 'objc-mode-hook #'set-clang-ac-sources)
;; (add-hook 'c #'set-clang-ac-sources)

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
             ("\\.v\\'" . (lambda () (progn (proof-load) (coq-mode))))))
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

(ignore-errors (require 'lilypond-init))

(add-hook 'haskell-mode-hook
 (lambda ()
  (require 'haskell-compile)
  (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
  (local-set-key (kbd "C-c c") #'recompile)
  ;; (set (make-local-variable 'compile-command)
  ;;  (format haskell-compile-command
  ;;   (file-name-nondirectory buffer-file-name)))
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

(add-hook 'coq-mode-hook
 (lambda ()
  (define-key coq-mode-map (kbd "C-c c") (lambda () (interactive) (ding)))))

(fset 'proof-load
 (let ((proof-loaded nil))
  (lambda ()
   (interactive)

   (unless proof-loaded
    (setq proof-splash-enable nil)
    (load-file "~/.emacs.d/el-get/ProofGeneral/ProofGeneral/generic/proof-site.el")
    (setq coq-prog-args '("-emacs-U" "-I" "/Users/acobb/programs/cpdt/cpdt/src"))
    (load-file (shell-command-to-string "agda-mode locate"))
    (setq agda2-include-dirs
     (list "." (expand-file-name "~/programs/agda-stdlib-0.9/src")))
    (setq proof-loaded t)))))

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

;; (setq unicode-fonts-skip-font-groups nil)
;; (require 'unicode-fonts)
;; (unicode-fonts-setup)

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

(defun find-first-non-ascii-char ()
 "Find the first non-ascii character from point onwards."
 (interactive)
 (let (point)
  (save-excursion
   (setq point
    (catch 'non-ascii
     (while (not (eobp))
      (or (eq (char-charset (following-char))
           'ascii)
       (throw 'non-ascii (point)))
      (forward-char 1)))))
  (if point
   (goto-char point)
   (message "No non-ascii characters."))))

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
    (uncrustify-mode unicode-enbox racket-mode misc-cmds hl-spotlight gnu-apl-mode)))
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
 '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-field-face ((t (:foreground "#ad7fa8"))))
 '(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
 '(agda2-highlight-inductive-constructor-face ((t (:foreground "#ef2929"))))
 '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
 '(agda2-highlight-module-face ((t (:inherit font-lock-builtin-face))))
 '(agda2-highlight-number-face ((t (:inherit font-lock-constant-face))))
 '(agda2-highlight-postulate-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-primitive-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-record-face ((t (:inherit font-lock-type-face))))
 '(agda2-highlight-string-face ((t (:inherit font-lock-string-face)))))
