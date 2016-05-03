;; -*- mode: emacs-lisp; no-byte-compile: t; lexical-binding: t -*-

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(
     agda
     auto-completion
     better-defaults
     coq
     c-c++
     emacs-lisp
     latex
     haskell
     ;; spell-checking
     syntax-checking
     themes-megapack
     )
   dotspacemacs-additional-packages '(
                                      auctex
                                      fsharp-mode
                                      nix-mode
                                      ;; pdf-tools
                                      racket-mode
                                      window-purpose
                                      z3-mode
                                      )
   dotspacemacs-excluded-packages '(
                                    magit
                                    auto-compile
                                    exec-path-from-shell
                                    recentf
                                    smartparens
                                    undo-tree
                                    vi-tilde-fringe
                                    )))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '()
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'lisp-interaction-mode
   dotspacemacs-themes '(sanityinc-tomorrow-bright manoj-dark spacemacs-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Menlo"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.3)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location nil
   dotspacemacs-max-rollback-slots 0
   dotspacemacs-use-ido t
   dotspacemacs-helm-resize t
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers t
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
 "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."

 (setq mac-command-modifier 'meta)
 (setq mac-option-modifier 'super)
 (setq ns-pop-up-frames nil)

 (defun other-window-previous (count)
  (interactive "p")
  (other-window (- count)))

 (setq custom-file "/dev/zero")

 (global-set-key (kbd "M-u") #'insert-char)
 (global-set-key (kbd "C-c s") #'query-replace-regexp)
 (global-set-key (kbd "C-c q") #'auto-fill-mode)
 (global-set-key (kbd "C-c a") #'auto-complete-mode)
 (global-set-key (kbd "C-c w") #'fixup-whitespace)
 (global-set-key (kbd "C-c c") #'recompile)
 (global-set-key (kbd "C-c C") #'compile)
 (global-set-key (kbd "C-c C-k") #'kill-compilation)
 (global-set-key (kbd "C-c C-c") #'comment-region)
 (global-set-key (kbd "C-c u") #'revert-buffer)
 (global-set-key (kbd "C-c ;") #'ispell-buffer)
 (global-set-key (kbd "C-c C--") #'dec-char-at-point)
 (global-set-key (kbd "C-c C-=") #'inc-char-at-point)
 (global-set-key (kbd "M-`") #'ff-find-other-file)
 (global-set-key (kbd "RET") #'newline-and-indent)
 (global-set-key (kbd "M-s") #'save-buffer)
 (global-set-key (kbd "\xA0") " ") ;; nbsp -> normal space
 (global-set-key (kbd "C-c C-/") #'describe-char)
 (global-set-key (kbd "M-g M-f") #'first-error)
 (global-set-key (kbd "M-k") #'evil-scroll-up)
 (global-set-key (kbd "M-j") #'evil-scroll-down)
 (global-set-key (kbd "C-x C-o") #'other-window-previous)
 (global-set-key (kbd "M-d") #'dash-at-point)
 (global-set-key (kbd "<C-return>") #'indent-new-comment-line)
 ;; (global-set-key (kbd "M-l") #'google-chrome-goto-location)
 (global-set-key (kbd "<C-M-tab>") #'clang-format-region)
 (global-set-key (kbd "C-;") #'avy-goto-word-1)
 (global-set-key (kbd "C-'") #'avy-goto-char-2)
 (global-set-key (kbd "C-`") #'toggle-window-dedicated)
 (global-set-key (kbd "C-S-h") 'windmove-left)
 (global-set-key (kbd "C-S-l") 'windmove-right)
 (global-set-key (kbd "C-S-k") 'windmove-up)
 (global-set-key (kbd "C-S-j") 'windmove-down)
 (global-set-key (kbd "C-M-e") nil)
 (global-set-key (kbd "M-ESC ESC") nil)


 ;; TODO: rewrite as advice
 (global-set-key (kbd "C-c i")
  (lambda (&optional arg interactive)
   (interactive "P\np")
   (require 'agda-input)
   (toggle-input-method arg interactive)))

 (eval-after-load "compile"
  (progn
   (setq compilation-scroll-output t)
   '(define-key compilation-mode-map (kbd "g") nil)))

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

 (setq evil-cross-lines t)

 (setq undo-limit (round (* 1 1024 1024 1024)))
 (setq undo-strong-limit (round (* 1.5 1024 1024 1024)))

 (setq dabbrev-case-fold-search nil)

 (defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
 (add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

 (eval-after-load 'latex
  (lambda ()
   (add-hook 'LaTeX-mode-hook
    (lambda ()
     (local-set-key (kbd "M-q") #'fill-sentence)))))

 (progn
  (setq el-get-notify-type 'message)
  (setq el-get-dir "~/.spacemacs.d/el-get/")
  (mapc (lambda (x) (add-to-list 'load-path x))
   '("~/.spacemacs.d/el-get/el-get"
     "~/.spacemacs.d/lisp"
     "~/.config/dotfiles/emacs/lisp"
     "/Applications/LilyPond.app/Contents/Resources/share/emacs/site-lisp"))

  (unless (require 'el-get nil 'noerror)
   (with-current-buffer
    (url-retrieve-synchronously
     "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
  (add-to-list 'el-get-recipe-path "~/.spacemacs.d/el-get-user/recipes")
  (el-get 'sync
   '(ProofGeneral
     dash-at-point
     )))

 (setq ghc-sort-key nil)
 (setq ghc-insert-key nil)

 (fset 'proof-load
  (let ((proof-loaded nil))
   (lambda ()
    (interactive)

    (unless proof-loaded
     (setq proof-splash-enable nil)
     (setq proof-shell-process-connection-type nil)
     (load-file "~/.spacemacs.d/el-get/ProofGeneral/ProofGeneral/generic/proof-site.el")
     ;; (setq coq-prog-args '("-emacs-U" "-I" "/Users/acobb/programs/cpdt/cpdt/src"))
     (setq proof-loaded t)))))

 (dolist (a '(("\\.v\\'" . (lambda () (progn (proof-load) (coq-mode))))))
  (add-to-list 'auto-mode-alist a))

 (eval-after-load "ido"
  (lambda () (setq ido-auto-merge-work-directories-length -1)))
 )

(defun fix-save ()
 (interactive)
 (local-unset-key (kbd "M-s")))

(defun dotspacemacs/user-config ()
 "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

 ;(scroll-bar-mode 1)
 (set-face-foreground 'hl-line nil)
 (global-linum-mode 1)
 (setq linum-eager nil)
 (setq linum-delay t)
 (menu-bar-mode 1)

 ;; (require 'frame-focus-hints)
 (require 'transpose-window-splits)

 (dolist (map (list evil-normal-state-map evil-motion-state-map))
  (define-key map (kbd "C-w ;") #'transpose-window-splits))

 (when (memq window-system '(mac ns))
  (setq frame-resize-pixelwise t)
  (let ((fullscreen-mode 'maximized))
   (when (> (x-display-pixel-width) 1440) ;; crude test for multiple displays
    (setq initial-frame-alist `((left + -2000) . ,initial-frame-alist))
    '(setq fullscreen-mode 'fullscreen))
   ;;(setq ns-use-native-fullscreen nil)
   (setq initial-frame-alist
    `((fullscreen . ,fullscreen-mode) . ,initial-frame-alist))))

 (global-set-key (kbd "M-h") #'ns-do-hide-emacs)
 (setq initial-buffer-choice t)
 (save-place-mode -1)

 ;; Make <escape> NOT quit as much as possible, you stupid jerks
 (define-key isearch-mode-map (kbd "<escape>") nil)

 (define-key minibuffer-local-map (kbd "<escape>") nil)
 (define-key minibuffer-local-ns-map (kbd "<escape>") nil)
 (define-key minibuffer-local-completion-map (kbd "<escape>") nil)
 (define-key minibuffer-local-must-match-map (kbd "<escape>") nil)
 (define-key minibuffer-local-isearch-map (kbd "<escape>") nil)

 (setq tab-always-indent t)

 (purpose-mode 1)
 (require 'purpose-color)
 (add-hook 'after-change-major-mode-hook
  #'purpose-color-update-fringe-color-for-current-buffer)
 (add-hook 'purpose-display-buffer-functions
  #'purpose-color-update-fringe-color-for-current-buffer)

 ;; (defun my-undo-boundary ()
 ;;  (when (equal (buffer-name) "*scratch*")
 ;;   (message "before: %s (%s %s %s ...)"
 ;;    undo-auto--last-boundary-cause
 ;;    (car-safe buffer-undo-list)
 ;;    (car-safe (cdr-safe buffer-undo-list))
 ;;    (car-safe (cdr-safe (cdr-safe buffer-undo-list))))
 ;;   (undo-boundary)

 ;;   (message "after:  %s (%s %s %s ...)"
 ;;    undo-auto--last-boundary-cause
 ;;    (car-safe buffer-undo-list)
 ;;    (car-safe (cdr-safe buffer-undo-list))
 ;;    (car-safe (cdr-safe (cdr-safe buffer-undo-list))))
 ;;   ))

 ;; (remove-hook 'evil-insert-state-exit-hook
 ;;  #'my-undo-boundary)

 (setq evil-insert-state-message nil)
 (make-*-better)

 )

(defun fill-sentence ()
  (interactive)
  (save-excursion
    (or (eq (point) (point-max)) (forward-char))
    (forward-sentence -1)
    (indent-relative t)
    (let ((beg (point))
          (ix (string-match "LaTeX" mode-name)))
      (forward-sentence)
      (if (and ix (equal "LaTeX" (substring mode-name ix)))
          (LaTeX-fill-region-as-paragraph beg (point))
        (fill-region-as-paragraph beg (point))))))

(defun make-fill-sentence-default ()
  (interactive)
  (local-set-key (kbd "M-q") #'fill-sentence))

(defun make-*-better ()
 (interactive)
 (define-key evil-normal-state-map (kbd "*") #'evil-search-word-forward))

(defun racket-rain-down-judgment ()
 (interactive)
 (goto-char (point-at-eol))
 (let ((col (current-column)))
  (newline-and-indent)
  (insert
   (make-string
    (- col (current-column))
    ?-))))

(defun do-ligatures ()
 (interactive)
 ;; from https://github.com/tonsky/FiraCode/wiki/Setting-up-Emacs
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
