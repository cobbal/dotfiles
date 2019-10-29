;; -*- no-byte-compile: t; lexical-binding: t -*-

(macroexpand '
(use-package rjsx-mode
 :el-get t
 :commands rjsx-mode
 :mode "\\.jsx?$"
 :config (progn
          (setq js2-mode-show-parse-errors nil)
          (setq js2-mode-show-strict-warnings nil)
          (setq js-indent-level 2)

          ;; Keep js2 from making function params an odd green.
          ;;(set-face-attribute 'js2-function-param nil
          ;;  :foreground 'unspecified
          ;; :inherit 'default))
)))

(use-package swift-mode
 :el-get t
 :commands swift-mode
 :mode "\\.swift\\'"
 ;; :config
 ;; (progn
 ;;          (unless (boundp 'ac-source-company-sourcekit)
 ;;           (el-get 'sync '(ac-company company-sourcekit flycheck flycheck-swift))
 ;;           (require 'ac-company)
 ;;           (require 'company-sourcekit)
 ;;           (require 'flycheck)
 ;;           (require 'flycheck-swift)
 ;;           (flycheck-swift-setup)
 ;;           (ac-company-define-source ac-source-company-sourcekit company-sourcekit)
 ;;           (add-to-list 'ac-sources 'ac-source-company-sourcekit))))
 )

(use-package company-lsp
 :el-get t
 :config
 (add-to-list 'company-lsp company-backends))

(use-package lsp-mode
 :el-get t
 :hook (swift-mode . #'lsp)
 :commands lsp
 :config
 (setq lsp-enable-snippet nil))

(use-package lsp-ui
 :el-get t
 :hook (lsp-mode . lsp-ui-mode)
 :commands lsp-ui-mode)

(use-package lsp-treemacs
 :el-get t
 :commands lsp-treemacs-errors-list)

(use-package lsp-sourcekit
 :el-get t
 :after lsp-mode
 :config
 (setenv "SOURCEKIT_TOOLCHAIN_PATH" "/Library/Developer/Toolchains/swift-latest.xctoolchain")
 (setq lsp-sourcekit-executable (expand-file-name "~/src/sourcekit-lsp/.build/debug/sourcekit-lsp")))
(provide 'use-packages)
