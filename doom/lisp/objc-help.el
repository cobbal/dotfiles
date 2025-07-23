(require 'thingatpt)

(defcustom objc-help-doc-set
 (concat
  "/Developer/Documentation/DocSets/"
  "com.apple.adc.documentation.AppleLion.CoreReference.docset")
 "")

(defun iphoneize ()
 (interactive)
 (setq objc-help-doc-set
  (concat
   "/Developer/Platforms/iPhoneOS.platform/Developer/Documentation/DocSets/com.apple.adc.documentation.AppleiPhone4_0.iPhoneLibrary.docset")))

(defun de-iphoneize ()
 (interactive)
 (setq objc-help-doc-set
  (concat
   "/Developer/Documentation/DocSets/"
   "com.apple.adc.documentation.AppleLion.CoreReference.docset")))


(defcustom objc-help-browser-format
 "osascript -e 'tell application \"Safari\" \n open location \"%s\" \n activate \n end tell'"
 "")

(defun objc-help-lookup (symbol)
 (interactive)
 (let ((results (shell-command-to-string
                 (format "/Developer/usr/bin/docsetutil search -query '%s' -skip-text '%s'"
                  symbol objc-help-doc-set))))
  (shell-command
   (format objc-help-browser-format
    (format "file://%s/Contents/Resources/Documents/%s"
     objc-help-doc-set (car (cdr (split-string results))))))))



(defun objc-help-lookup-under-point ()
 (interactive)
 (objc-help-lookup (word-at-point)))

(add-hook 'objc-mode-hook
 (lambda ()
  (define-key objc-mode-map (kbd "C-h x") 'objc-help-lookup-under-point)))

(provide 'objc-help)
