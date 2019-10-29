;; -*- no-byte-compile: t; lexical-binding: t -*-

;; Do this first to minimize color flash
(or
 (ignore-errors
  (progn
   (load "~/.emacs.d/el-get/color-theme-sanityinc-tomorrow/color-theme-sanityinc-tomorrow.el")
   (require 'color-theme-sanityinc-tomorrow)
   (load-theme 'sanityinc-tomorrow-bright t)))
 (load-theme 'manoj-dark t))

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq ns-pop-up-frames nil)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(blink-cursor-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq ad-redefinition-action 'accept) ;; silence advice warning about redefinition

(setq evil-want-abbrev-expand-on-insert-exit nil)
(setq evil-regexp-search t)

(defmacro add-my-hook (hook-name args &rest body)
 "This will define a hook named \"my/`hook-name'\" and put the contents of
 `body' into it. There can only be one such definition, and new ones will
 overwrite old ones. This is intentional and should only be used in the init
 file to preserve sanity and compositionality."
 (let* ((hook-name-str (symbol-name hook-name))
        (my-hook-name (intern (concat "my/" hook-name-str))))
 `(progn
   (defun ,my-hook-name ,args
    ,@body)
   (add-hook ',hook-name ',my-hook-name))))

(defun add-to-list* (list-var elements &optional append compare-fn)
 (dolist (a elements)
  (add-to-list list-var a append compare-fn))
 (symbol-value list-var))

(add-to-list* 'load-path
 (mapcar #'expand-file-name
  (list
   "~/.emacs.d/lisp"
   "~/.emacs.d/el-get/el-get"
   "~/.emacs.d/el-get/agda-input"
   "~/.emacs.d/el-get/ht"
   "~/.emacs.d/el-get/lsp-mode"
   "~/.emacs.d/el-get/lsp-ui"
   "~/.emacs.d/el-get/lsp-sourcekit"
   "~/.emacs.d/el-get/spinner"
   "~/.emacs.d/el-get/ac-company"
   "~/.emacs.d/el-get/company-mode"
   "~/.emacs.d/el-get/company-lsp"
   "~/.emacs.d/el-get/dash-functional"
   "~/.emacs.d/el-get/auctex"
   "~/.emacs.d/el-get/auctex-latexmk"
   "~/.emacs.d/PG/generic"
   "~/.nix-profile/share/emacs/site-lisp"
   "~/.nix-profile/share/emacs/site-lisp/ProofGeneral/generic"
   "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp/")))

(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
 (when (and opam-share (file-directory-p opam-share))
  ;; Register Merlin
  (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))))

(setq-default insert-directory-program (or (executable-find "gls") "ls"))

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

(el-get 'sync '(use-package-el-get))
(require 'use-package-el-get)
(setq use-package-always-ensure nil)
(use-package-el-get-setup)

(require 'package)
(add-to-list 'package-archives
 '("melpa" . "https://melpa.org/packages/") t)
(require 'el-get-elpa)
;; Build the El-Get copy of the package.el packages if we have not
;; built it before.  Will have to look into updating later ...
(unless (file-directory-p el-get-recipe-path-elpa)
 (el-get-elpa-build-local-recipes))

(el-get 'sync
 '(agda-input
   auto-complete
   avy
   cmake-mode
   coffee-mode
   company
   company-quickhelp
   clojure-mode
   d-mode
   diminish
   dash-at-point
   evil
   f
   fill-column-indicator
   fsharp-mode
   framemove
   glsl-mode
   gnu-apl-mode
   go-mode
   graphviz-dot-mode
   ;;  haskell-mode
   ;; haskell-emacs
   hindent
   hy-mode
   idris-mode
   key-chord

   magit
   markdown-mode
   misc-cmds
   nix-mode
   ocp-indent
   php-mode
   purescript-mode
   racket-mode
   rust-mode
   color-theme-sanityinc-tomorrow
   ;; scala-mode2
   sexp-rewrite
   spaceline
   sml-mode
   ;; unicode-fonts
   web
   window-purpose
   z3-mode
   tuareg-mode
   reason-mode
   yaml-mode
   ))

;; (require 'helm-config)
;; (helm-mode 0)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)

;; (global-set-key (kbd "M-x") 'helm-M-x)
;; (unless (boundp 'completion-in-region-function)
;;  (define-key lisp-interaction-mode-map
;;   [remap completion-at-point]
;;   'helm-lisp-completion-at-point)
;;  (define-key emacs-lisp-mode-map
;;   [remap completion-at-point]
;;   'helm-lisp-completion-at-point))
;; (add-to-list 'display-buffer-alist
;;  `(,(rx bos "*helm" (* not-newline) "*" eos)
;;    (display-buffer-in-side-window)
;;    (inhibit-same-window . t)
;;    (window-height . 0.4)))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define evil-insert-state-map "jk" #'evil-normal-state)
(key-chord-define evil-replace-state-map "jk" #'evil-normal-state)

;; fix broken colors on fancy powerline images
(when (memq window-system '(mac ns))
 (setq powerline-image-apple-rgb t))

(require 'spaceline-config)
(spaceline-toggle-version-control-off)
(spaceline-define-segment wc-segment
 (if (use-region-p)
  (format "[%d %d %d]"
   (abs (- (point) (mark)))
   (count-words-region (point) (mark))
   (abs (- (line-number-at-pos (point))
         (line-number-at-pos (mark)))))
  (format "%d %d %d"
   (- (point-max) (point-min))
   (count-words-region (point-min) (point-max))
   (line-number-at-pos (point-max))))
 :enabled nil)
(spaceline-spacemacs-theme 'wc-segment)
(setq spaceline-highlight-face-func #'spaceline-highlight-face-evil-state)

(defun magic-close-parens ()
 (interactive)
 (require 'racket-mode)
 (dolist (k '(")" "]" "}"))
  (local-set-key (kbd k) #'racket-insert-closing)))

(package-initialize)

(defun el-get-install-optionals ()
 (interactive)
 (dolist (pkg '(auctex
                auctex-latexmk
                ac-company
                company-sourcekit
                ))
  (el-get-install pkg)))

;; adapted from https://emacs.stackexchange.com/a/23785
(add-my-hook after-make-frame-functions (frame)
 (modify-frame-parameters frame
  '((vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil))))

(defun racket-rain-down-judgment ()
 (interactive)
 (goto-char (point-at-eol))
 (let ((col (current-column)))
  (newline-and-indent)
  (insert
   (make-string
    (- col (current-column))
    ?-))))

(defun racket-rain-up-judgment ()
 (interactive)
 (goto-char (point-at-eol))
 (let ((col (current-column)))
  (previous-line)
  (goto-char (point-at-eol))
  (newline-and-indent)
  (insert
   (make-string
    (- col (current-column))
    ?-))))

;; (require 'window-lock)

(setq racket-mode-pretty-lambda nil)
(setq racket-program "/Applications/Racket/bin/racket")
(add-my-hook racket-mode-hook ()
 (require 'sexp-rewrite)
 (require 'racket-rewrites)
 (local-set-key (kbd "C-c d") #'sexprw-mode-keymap)
 ;; (prettify-symbols-mode t)
 (setq-local eldoc-documentation-function nil)
 (local-set-key (kbd "C-M-d") #'racket-visit-definition)
 (local-set-key (kbd "C-c C--") #'racket-rain-down-judgment)
 (local-set-key (kbd "C-c C-=") #'racket-rain-up-judgment))

(require 'evil)
(evil-mode 1)
(setq evil-cross-lines t)
(define-key evil-motion-state-map [down-mouse-1] #'mouse-drag-region)
(define-key evil-motion-state-map (kbd "K") nil)
(define-key evil-visual-state-map (kbd "u") #'undo)
(add-to-list 'evil-intercept-maps '(compilation-mode-map))
(setq evil-echo-state nil)
(setq-default evil-symbol-word-search t)
(setq evil-mode-line-format '(before . mode-line-frame-identification))
(global-undo-tree-mode -1)
(global-auto-revert-mode 1)

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
(setq-default tab-width 8)
(setq coffee-tab-width 4)
(setq-default indent-tabs-mode nil)
(setq-default py-indent-offset 4)
(setq sgml-basic-offset 1)
(setq hamlet-basic-offset 1)
(setq lisp-indent-offset 1)
(setq-default fill-column 80)
(setq fill-column 80)
(setq sentence-end-double-space nil)
(setq window-combination-resize t)

(defun my-terminal-visible-bell ()
 "A friendlier visual bell effect."
 (invert-face 'mode-line)
 (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq visible-bell nil
 ring-bell-function #'my-terminal-visible-bell)

(show-paren-mode 1)

(add-my-hook c-mode-common-hook ()
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
 (c-set-style "correct"))

(setq initial-frame-alist '((width . 100) (height . 53) (top . 20) (left . 0)))
(setq default-frame-alist '((width . 100) (height . 53) (top . 20)))

;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(when (memq window-system '(mac ns))
 (setq frame-resize-pixelwise t)
 (let ((fullscreen-mode 'maximized))
  (when (> (x-display-pixel-width) 1440) ;; crude test for multiple displays
   (setq initial-frame-alist `((left + 4000) . ,initial-frame-alist))
   '(setq fullscreen-mode 'fullscreen))
  ;;(setq ns-use-native-fullscreen nil)
  (setq initial-frame-alist
   `((fullscreen . ,fullscreen-mode) . ,initial-frame-alist))))

;; (require 'exec-path-from-shell)
;; (push "GOPATH" exec-path-from-shell-variables)
;; (exec-path-from-shell-initialize)


;; Reminder: Mac antialiasing can be disabled with:
;; defaults write org.gnu.Emacs AppleAntiAliasingThreshold 999

(defun try-set-font (font)
 (ignore-errors (set-frame-font font t t) t))

(or
 ;; (try-set-font "ProggyTiny 11")
 ;; (try-set-font "Crisp 16")
 (try-set-font "Menlo 12")
 (try-set-font "Menlo 11")
 (try-set-font "Hack 10")
 (when (eq window-system 'w32)
  (try-set-font "DejaVu Sans mono 8"))
 (try-set-font "DejaVu Sans mono 11")
 (try-set-font "DejaVu Sans mono 13")
 (try-set-font "Espresso mono 11")
 (try-set-font "Consolas 13")
 (try-set-font "Input 11")
 )

(setq org-startup-folded nil)
(setq org-M-RET-may-split-line nil)

(defun do-ligatures ()
 (interactive)
 ;; from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
 (when (window-system)
  (set-default-font "Fira Code"))
 (let ((alist '((33 . ".\\(?:\\(?:==\\)\\|[!=]\\)")
                (35 . ".\\(?:[(?[_{]\\)")
                (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                (42 . ".\\(?:\\(?:\\*\\*\\)\\|[*/]\\)")
                (43 . ".\\(?:\\(?:\\+\\+\\)\\|\\+\\)")
                (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=]\\)")
                (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                (58 . ".\\(?:[:=]\\)")
                (59 . ".\\(?:;\\)")
                (60 . ".\\(?:\\(?:!--\\)\\|\\(?:\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[/<=>|-]\\)")
                (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                (63 . ".\\(?:[:=?]\\)")
                (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                (94 . ".\\(?:=\\)")
                (123 . ".\\(?:-\\)")
                (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                (126 . ".\\(?:[=@~-]\\)"))))
  (dolist (char-regexp alist)
   (set-char-table-range composition-function-table (car char-regexp)
    `([,(cdr char-regexp) 0 font-shape-gstring])))))


;; (require 'frame-focus-hints)
(require 'transpose-window-splits)

(defun unload-enabled-themes ()
 (interactive)
 (mapcar #'disable-theme custom-enabled-themes))

(defadvice load-theme (before theme-dont-propagate activate)
 (unload-enabled-themes))

(defadvice proof-layout-windows (around disable-window-resize-for-pg activate)
 (let ((window-combination-resize nil))
  ad-do-it))

(setq compilation-ask-about-save nil)
(setq compilation-always-kill t)
(defcustom compile-always-comint nil "")
(defun my-compile-advice (orig-fun command &optional mode &rest args)
 (apply orig-fun command (or compile-always-comint mode) args))
(advice-add 'compilation-start :around #'my-compile-advice)
(setq compilation-scroll-output 'first-error)

;; https://stackoverflow.com/a/23382008

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook #'display-ansi-colors)

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

(defun find-file-at-mouse (event)
 "like find-file-at-point, but at mouse instead"
 (interactive "e")
 (save-excursion
  (mouse-set-point event)
  (find-file-at-point)))


;;(define-key global-map [down-mouse-1] nil)
(global-set-key (kbd "M-u") #'insert-char)
(global-set-key (kbd "C-c s") #'query-replace-regexp)
(global-set-key (kbd "C-c q") #'auto-fill-mode)
(global-set-key (kbd "C-c a") #'auto-complete-mode)
(global-set-key (kbd "C-c w") #'fixup-whitespace)
(global-set-key (kbd "C-c c") #'recompile)
(global-set-key (kbd "C-c C") #'compile)
(global-set-key (kbd "C-c C-k") #'kill-compilation)
(global-set-key (kbd "C-c C-c") #'comment-region)
(global-set-key (kbd "C-c C-e") #'pp-eval-last-sexp)
(global-set-key (kbd "C-c e") #'toggle-debug-on-error)
(global-set-key (kbd "C-c u") #'revert-buffer)
(global-set-key (kbd "C-c ;") #'ispell-buffer)
(global-set-key (kbd "C-c C--") #'dec-char-at-point)
(global-set-key (kbd "C-c C-=") #'inc-char-at-point)
(global-set-key (kbd "M-`") #'ff-find-other-file)
(global-set-key (kbd "M-h") #'ns-do-hide-emacs)
(global-set-key (kbd "RET") #'newline-and-indent)
(global-set-key (kbd "M-s") #'save-buffer)
(global-set-key (kbd " ") " ") ;; nbsp -> normal space
(global-set-key (kbd "C-M-g") #'keyboard-quit)
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
(global-set-key (kbd "C-x g") #'magit-status)
;; (global-set-key (kbd "C-`") #'toggle-window-dedicated)
(global-set-key (kbd "C-S-h") 'windmove-left)
(global-set-key (kbd "C-S-l") 'windmove-right)
(global-set-key (kbd "C-S-k") 'windmove-up)
(global-set-key (kbd "C-S-j") 'windmove-down)
(global-set-key [M-mouse-1] #'find-file-at-mouse)
(define-key evil-normal-state-map (kbd "TAB") #'indent-for-tab-command)
(define-key evil-normal-state-map (kbd "M-.") nil)
(dolist (map (list
              ;; evil-normal-state-map
              evil-motion-state-map))
 (define-key map (kbd "C-w ;") #'transpose-window-splits))
(eval-after-load "compile"
 '(progn
   (define-key compilation-mode-map (kbd "h") nil)
   (define-key compilation-mode-map (kbd "g") nil)))
(evil-set-initial-state 'compilation-mode 'normal)

(add-my-hook org-mode-hook ()
 (define-key org-mode-map (kbd "M-h") #'ns-do-hide-emacs)
 (linum-mode -1))

(add-my-hook c++-mode-hook ()
 (define-key c++-mode-map (kbd "C-c C-k") nil))


(require 'framemove)
(setq framemove-hook-into-windmove t)

(winner-mode 1)

;; bind C-x 5 3 to be same as C-x 5 2
(define-key ctl-x-5-map (kbd "3") 'make-frame-command)

;; TODO: rewrite as advice
(global-set-key (kbd "C-c i")
 (lambda (&optional arg interactive)
  (interactive "P\np")
  (require 'agda-input)
  (toggle-input-method arg interactive)))

(avy-setup-default)

(when (load "auctex/auctex.el" t t t)
 (require 'auctex-latexmk)
 (setq auctex-latexmk-inherit-TeX-PDF-mode t)
 (auctex-latexmk-setup))

;; this is insane... why are there 3 hooks???
;; (add-my-hook latex-mode-hook ()
;;  (add-to-list 'LaTeX-indent-environment-list '("algorithmic" current-indentation)))

(add-my-hook LaTeX-mode-hook ()
 (local-set-key (kbd "M-q") #'fill-sentence)

 (TeX-source-correlate-mode 1)

 ;; Apparently, some japanese extension for latex likes to override this
 ;; variable unless this magic customize thingy is set??? ugh... it worked just
 ;; fine before auctex-latexmk got involved...

 ;; It gets weirder, tex-jp.el gets loaded on tab completion for inspecting the
 ;; variables I'm interested in. <U+1F926 FACE PALM>.
 (put 'TeX-view-program-selection 'saved-value t)
 (put 'TeX-view-program-list 'saved-value t)

 (add-to-list 'TeX-view-program-list
  '("displayline" "displayline -g %n %o %b" "displayline"))

 (add-to-list 'TeX-view-program-selection
  '(output-pdf "displayline")))

(defun fill-by-auto-fill ()
 (interactive)
 (save-excursion
  (move-end-of-line nil)
  (funcall (or normal-auto-fill-function #'do-auto-fill))))

(add-my-hook auto-fill-mode-hook ()
 (local-set-key (kbd "M-q") #'fill-by-auto-fill))

(add-my-hook tex-mode-hook ()
 (set (make-local-variable 'before-save-hook) nil))

(require 'auto-complete)
(require 'auto-complete-config)

(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
(define-key ac-completing-map (kbd "RET") nil)
(add-my-hook emacs-lisp-mode-hook ()
 (add-to-list 'ac-sources 'ac-source-symbols))
(add-my-hook auto-complete-mode-hook ()
 (add-to-list 'ac-sources 'ac-source-filename))

;;(define-key ac-complete-mode-map viper-ESC-key 'viper-intercept-ESC-key)
(add-my-hook objc-mode-hook ()
 (run-at-time ".1 second" nil
  (lambda () (auto-complete-mode 1))))

(define-key ac-menu-map (kbd "RET") nil)


(setq ac-auto-start t)
(setq ac-dwim t)
(global-auto-complete-mode t)


(setq help-window-select t)

(global-hl-line-mode t)
(set-face-foreground 'hl-line nil)
(set-face-background 'fringe "#444444")
(fringe-mode (cdr-safe (assoc "half-width" fringe-styles)))

(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline-p 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")
;;(define-key ac-completing-map (kbd "M-n") 'ac-next)
;;(define-key ac-completing-map (kbd "M-p") 'ac-previous)
(ac-linum-workaround)

;;(require 'objc-help)
;;(iphoneize)

;; from https://stackoverflow.com/a/22074203
(defun crazy-yank ()
 "yank, but overwrite instead of insert"
 (interactive)
 (let ((txt (string-trim (current-kill 0))))
  (delete-char (length txt))
  (insert txt)))
(global-set-key (kbd "C-M-y") #'crazy-yank)

(defun copy-filename-and-line-number ()
 (interactive)
 (kill-new
  (format "%s:%s" (buffer-name) (line-number-at-pos (point)))))
(global-set-key (kbd "C-c C-p") #'copy-filename-and-line-number)

(dolist (l '((racket-mode . "racket")
             (scheme-mode . "racket")))
 (add-to-list 'dash-at-point-mode-alist l))

(add-my-hook scheme-mode-hook ()
 (magic-close-parens))

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

 (let ((fmt nil))
  (add-my-hook linum-before-numbering-hook ()
   (setq fmt
    (ignore-errors
     (let ((w (length (number-to-string
                       (count-lines (point-min) (point-max))))))
      (concat "%" (number-to-string w) "d")))))

  (defun custom-linum-formatter (n)
   (propertize (format (or fmt "%d") n) 'face 'linum)))

 ;; (setq linum-format "%d ")
  (setq linum-format #'custom-linum-formatter))
(column-number-mode t)

(defun eml-modoid ()
 (interactive)
 (fundamental-mode)
 (auto-fill-mode)
 (setq-local fill-column 72))

(autoload 'LilyPond-mode "lilypond-mode" "LilyPond Editing Mode" t)
(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

(require 'use-packages)

(add-to-list* 'auto-mode-alist
 '(("\\.h\\'" . c++-mode)
   ("\\.clj\\'" . clojure-mode)
   ("\\.coffee\\'" . coffee-mode)
   ("\\.v\\'" . coq-mode-shim)
   ("\\.cs\\'" . csharp-mode)
   ("\\.dart\\'" . dart-mode)
   ("\\.\\(v\\|f\\|tc\\|te\\)sh\\'" . glsl-mode)
   ("\\.jsont\\'" . html-mode)
   ("\\.ijs\\'" . j-mode)
   ;; ("\\.js\\'" . js-mode)
   ;; ("\\.jxa\\'" . js-mode)
   ;; ("\\.json\\'" . js-mode)
   ;; ("\\.julius\\'" . js-mode)
   ("\\.tex\\'" . LaTeX-mode)
   ("\\.ly$" . LilyPond-mode)
   ("\\.ily$" . LilyPond-mode)
   ("\\.cl\\'" . lisp-mode)
   ("[Mm]akefile." . makefile-mode)
   ("\\.mm\\'" . objc-mode)
   ("\\.pro\\'" . qmake-mode)
   ("\\.rkt\\'" . scheme-mode)
   ("\\.fscr\\'" . smalltalk-mode)
   ("\\tiger.lex\\'" . sml-lex-mode)
   ("\\.eml\\'" . eml-modoid)))

;; https://www.emacswiki.org/emacs/MacOSXPlist
(add-to-list 'jka-compr-compression-info-list
             ["\\.\\(?:plist\\|strings\\|nib\\|tracetemplate\\)\\'"
              "converting text XML to binary plist"
              "plutil"
              ("-convert" "binary1" "-o" "-" "-")
              "converting binary plist to text XML"
              "plutil"
              ("-convert" "xml1" "-o" "-" "-")
              nil nil "bplist"])
(jka-compr-update)

(defun coq-mode-shim ()
 (interactive)
 (proof-load)

 (setq coq-compile-before-require t)
 (setq coq-compile-auto-save 'save-coq)
 (coq-mode))

(add-my-hook js-mode-hook ()
;;; make emacs recognize the error format produced by jslint
 (set (make-local-variable 'compilation-error-regexp-alist)
  '(("^\\([a-zA-Z.0-9_/-]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3)))
 ;; (set (make-local-variable 'compile-command)
 ;;  (let ((file (file-name-nondirectory buffer-file-name)))
 ;;   (concat "/usr/share/jslint/jslint " file)))
 )

(require 'ido)
(setq ido-create-new-buffer 'always)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 'both)
(setq ido-auto-merge-work-directories-length -1)
;; (setq ido-file-extensions-order '(".lean"))
(add-to-list* 'ido-ignore-files
 '("\\.clean\\'"
   "\\.olean\\'"
   "\\.ilean\\'"
   "\\.v.d\\'"
   ))

(defun sort-lines-by (reverse beg end by)
 "Sort lines in region using BY; argument means descending order.
Called from a program, there are four arguments: REVERSE (non-nil
means reverse order), BEG and END (region to sort)."
 (interactive "P\nr\nXby: ")
 (save-excursion
  (save-restriction
   (narrow-to-region beg end)
   (goto-char (point-min))
   (let ;; To make `end-of-line' and etc. to ignore fields.
    ((inhibit-field-text-motion t))
    (sort-subr reverse 'forward-line 'end-of-line nil nil
     (lambda (a b)
      (let ((a1 (funcall by (buffer-substring (car a) (cdr a))))
            (b1 (funcall by (buffer-substring (car b) (cdr b)))))
       (cond
        ((and (number-or-marker-p a1) (number-or-marker-p b1))
         (< a1 b1))
        ((and (stringp a1) (stringp b1))
         (string< a1 b1))
        ((and (symbolp a1) (symbolp b1))
         (string< (symbol-name a1) (symbol-name b1)))
        (t (error "don't know how to compare %s and %s" a1 b1))))))))))


(defun byte-compile-all-in-emacs-d-lisp ()
 (interactive)
 (byte-recompile-directory (expand-file-name "~/.emacs.d/lisp") 0))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;(load "~/.emacs.d/el-get/haskell-mode/haskell-site-file")
;;(require 'hamlet-mode)

(ignore-errors (require 'lilypond-init))

(setq lean-rootdir (expand-file-name "~/.nix-profile"))
(setq lean-server-options '("--memory=4096"))
;; (ignore-errors (require 'lean-mode))

(add-my-hook lean-mode-hook ()
 (setq evil-shift-width 2)
 (local-set-key (kbd "M-RET") #'lean-show-goal-at-pos))

(defvar hindent-line-length 102)

(eval-after-load 'hindent
 (lambda ()
  ;; (setq hindent-style "gibiansky")
  (defun hindent-extra-arguments-advice (fn)
   (append
    `("--line-length" ,(format "%d" hindent-line-length))
    (funcall fn)))
  (advice-add 'hindent-extra-arguments :around #'hindent-extra-arguments-advice)))

(add-my-hook haskell-mode-hook ()
 (require 'haskell-compile)
 (define-key haskell-mode-map "\C-ch" 'haskell-hoogle)
 (local-set-key (kbd "C-c c") #'recompile)
 ;; (set (make-local-variable 'compile-command)
 ;;  (format haskell-compile-command
 ;;   (file-name-nondirectory buffer-file-name)))
 (turn-on-haskell-doc-mode)
 ;; (turn-on-haskell-indentation)
 (if (fboundp 'electric-indent-local-mode)
  (electric-indent-local-mode -1))
 (hindent-mode 1)
 (setq compilation-error-regexp-alist haskell-compilation-error-regexp-alist)
 '(add-to-list 'prettify-symbols-alist '("\\" . ?λ))
 '(prettify-symbols-mode t))
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

(add-my-hook purescript-mode-hook ()
 (require 'haskell-compile)
 (local-set-key (kbd "C-c c") #'recompile)
 (turn-on-purescript-indentation)
 (if (fboundp 'electric-indent-local-mode)
  (electric-indent-local-mode -1))
 (hindent-mode 1))

(add-my-hook before-save-hook ()
 (delete-trailing-whitespace)
 (refmt-before-save))

(defface my-visible-mark-face-1
  `((t (:background "plum4" :foreground "white")))
  "Face for the mark."
  :group 'visible-mark)

(setq visible-mark-faces `(my-visible-mark-face-1))

(require 'visible-mark)
(global-visible-mark-mode t)

(setq confirm-kill-processes nil)

(defun interactive-ding () (interactive) (ding))

(add-my-hook coq-mode-hook ()
 (make-local-variable 'evil-insert-state-exit-hook)
 (setq evil-insert-state-exit-hook
  (remove #'expand-abbrev evil-insert-state-exit-hook))
 ;; (define-key coq-mode-map (kbd "C-c c") #'interactive-ding)
 )

(ignore-errors
 ;; (load-file (shell-command-to-string "agda-mode locate"))

 ;; (define-key ...? (kbd "M-\\") "\\")

 (setq agda-input-tweak-all
  '(agda-input-compose
    (agda-input-prepend "\\")
    (agda-input-nonempty)))

 (require 'agda-input)
 (setq default-input-method 'Agda))

(fset 'proof-load
 (let ((proof-loaded nil))
  (lambda ()
   (interactive)

   (unless proof-loaded
    (setq proof-splash-enable nil)
    (setq proof-shell-process-connection-type nil)
    (setq proof-auto-action-when-deactivating-scripting 'retract)
    (require 'proof-site)

    ;; (load-file "~/.emacs.d/el-get/ProofGeneral/ProofGeneral/generic/proof-site.el")
    ;; (setq coq-prog-args '("-emacs-U" "-I" "/Users/acobb/programs/cpdt/cpdt/src"))
    (or
     (ignore-errors
      (load-file (shell-command-to-string "agda-mode locate"))
      t)
     (message "%s" "couldn't locate agda-mode, continuing without"))
    ;; (setq agda2-include-dirs
    ;;  (list "." (expand-file-name "~/programs/agda-stdlib-0.9/src")))
    (setq proof-loaded t)))))

(defun enable-acl2r-mode ()
 (interactive)
 (proof-load)
 ;; (add-to-list 'proof-assistant-table '(acl2 "ACL2" "acl2"))
 ;; (add-to-list 'proof-general-configured-provers 'acl2)
 ;; (proofgeneral "acl2")
 (proof-ready-for-assistant 'acl2r "ACL2r")
 (require 'acl2r)
 (require 'proof-tree)
 (acl2r-mode)
 (setq indent-line-function 'lisp-indent-line)
 )

(add-my-hook d-mode-hook ()
 (add-to-list
  'compilation-error-regexp-alist
  '("^\\([^ \n]+\\)(\\([0-9]+\\)): \\(?:error\\|.\\|warnin\\(g\\)\\|remar\\(k\\)\\)"
    1 2 nil (3 . 4))))

(setq emdroid-activity-creator "activityCreator.py")
(setq emdroid-tools-dir "/Users/acobb/Desktop/programs/android/tools/")

(add-my-hook nix-mode-hook ()
 (local-set-key (kbd "M-`") #'find-file-at-point))

(setq purpose-layout-dirs (expand-file-name "~/.emacs.d/purpose-layouts"))
(purpose-mode 1)
(progn
 (add-to-list* 'purpose-user-mode-purposes
  '((haskell-mode . edit)
    (rust-mode . edit)
    (reason-mode . edit)
    (coq-mode . edit)
    ))
 (purpose-compile-user-configuration))
(require 'purpose-color)
(diminish 'purpose-mode)

;; ocaml stuff
(add-my-hook tuareg-mode-hook ()
 (setq compilation-error-regexp-alist
  (list '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
          2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face))))
 ;; Load merlin-mode
 (require 'merlin)
 (setq merlin-command 'opam)
 ;; Enable auto-complete
 (setq merlin-use-auto-complete-mode 'easy)
 (company-mode 1)
 (require 'ocp-indent)
 (company-quickhelp-mode 1)
 ;; Important to note that setq-local is a macro and it needs to be
 ;; separate calls, not like setq
 (setq-local merlin-completion-with-doc t)
 (setq-local merlin-error-on-single-line t)
 (setq-local indent-tabs-mode nil)
 (setq-local show-trailing-whitespace t)
 (setq-local indent-line-function 'ocp-indent-line)
 (setq-local indent-region-function 'ocp-indent-region) (define-key tuareg-mode-map (kbd "C-c C-k") nil)
 (modify-syntax-entry ?# "." tuareg-mode-syntax-table) ;; make foo#bar multiple symbols
 (merlin-mode 1)
 (diminish 'merlin-mode)
 )


(add-my-hook reason-mode-hook ()
 ;; Load merlin-mode
 (require 'merlin)
 (setq merlin-command "ocamlmerlin")

 ;; Enable auto-complete
 (setq merlin-use-auto-complete-mode 'easy)
 (company-mode 1)
 (company-quickhelp-mode 1)
 ;; Important to note that setq-local is a macro and it needs to be
 ;; separate calls, not like setq
 (setq-local merlin-completion-with-doc t)
 (setq-local indent-tabs-mode nil)
 (setq-local show-trailing-whitespace t)
 (merlin-mode 1)
 )

(add-my-hook sml-mode-hook ()
 (require 'sml-proc)
 (add-to-list* 'compilation-error-regexp-alist sml-error-regexp-alist))

(require 'dash)
(require 'apl-map)

;; (setq unicode-fonts-skip-font-groups nil)
;; (require 'unicode-fonts)
;; (unicode-fonts-setup)

(setq dabbrev-case-fold-search nil)

(setq ispell-program-name "aspell")

(setq doc-view-resolution 200)

(setq graphviz-dot-auto-indent-on-semi nil)

(defun set-lisp-indent-offset (n)
 (interactive "Nlisp-indent-offset: ")
 (set (make-local-variable 'lisp-indent-offset) n))

;; (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(server-start)

(require 'boxfu)

;;(evil-transient-mark -1)
;;(transient-mark-mode -1)

(defun create-tags (dir-name)
 "Create tags file."
 (interactive "DDirectory: ")
 (shell-command
  (format "ctags -e -R %s" (directory-file-name dir-name))))

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

(defun fill-sentence ()
 (interactive)
 (save-excursion
  (or (eq (point) (point-max)) (forward-char))
  (forward-sentence -1)
  (indent-relative t)
  (let ((beg (point))
        (ix (string-match "LaTeX" mode-name)))
   (forward-sentence)
   (if (and ix (equal "LaTeX" (substring mode-name ix)) nil)
    (LaTeX-fill-region-as-paragraph beg (point))
    (fill-region-as-paragraph beg (point))))))

;; https://sites.google.com/site/steveyegge2/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
 "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
 (let ((name (buffer-name))
       (filename (buffer-file-name)))
  (if (not filename)
   (message "Buffer '%s' is not visiting a file!" name)
   (if (get-buffer new-name)
    (message "A buffer named '%s' already exists!" new-name)
    (progn
     (rename-file filename new-name 1)
     (rename-buffer new-name)
     (set-visited-file-name new-name)
     (set-buffer-modified-p nil))))))

(defun move-buffer-file (dir)
 "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
 (let* ((name (buffer-name))
	 (filename (buffer-file-name))
	 (dir
	 (if (string-match dir "\\(?:/\\|\\\\)$")
	 (substring dir 0 -1) dir))
	 (newname (concat dir "/" name)))
  (if (not filename)
   (message "Buffer '%s' is not visiting a file!" name)
   (progn
    (copy-file filename newname 1)
    (delete-file filename)
    (set-visited-file-name newname)
    (set-buffer-modified-p nil)
    t))))

(defun iterm-here ()
 (interactive)
 (call-process "iterm-newtab.applescript"))

(advice-add 'next-error :around
 (defun make-next-error-non-interactive (orig-fun &rest args)
  ;; next-error is too stupid, so it lost it's interaction priveleges
  (cl-letf (((symbol-function 'read-file-name)
             (lambda (&rest read-args)
              (error "file not found!"))))
   (apply orig-fun args))))

(defadvice load-theme (before theme-dont-propagate activate)
 (unload-enabled-themes))

(setq safe-local-variable-values
 '((eval . (visible-mode t))
   (eval . (auto-fill-mode t))
   (eval . (ignore-errors "Write-contents-functions is a buffer-local alternative to before-save-hook"
            (add-hook 'write-contents-functions
             (lambda nil
              (delete-trailing-whitespace)
              nil))
            (require 'whitespace)
            "Sometimes the mode needs to be toggled off and on."
            (whitespace-mode 0)
            (whitespace-mode 1)))
   (whitespace-style . (face tabs trailing lines-tail))
   (whitespace-line-column . 80)
   (idris-load-packages . ("contrib"))
   (encoding . utf-8)))

(setq describe-char-unidata-list
 '(name old-name general-category decomposition uppercase lowercase))

(setq custom-file "/dev/zero")

(set-face-attribute 'show-paren-mismatch nil
 :foreground "#000000"
 :background "#00ff00"
 :weight 'bold
 )

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
