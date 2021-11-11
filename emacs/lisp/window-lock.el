;; -*- lexical-binding: t -*-

(defvar-local custom-linum-fmt-cache "UNDEF")
(defvar-local custom-linum-face-cache 'linum)
(defvar custom-linum-override-face-thunk nil)

(defun custom-linum-before-numbering-hook ()
 (setq custom-linum-fmt-cache
  (or
   (ignore-errors
    (let ((w (length (number-to-string
                      (count-lines (point-min) (point-max))))))
     (concat "%" (number-to-string w) "d")))
   "ERROR IN custom-linum-before-numbering-hook (fmt)"))
 (setq custom-linum-face-cache
  (or
   (ignore-errors
    (if (functionp custom-linum-override-face-thunk)
     (funcall custom-linum-override-face-thunk)
     'linum))
   'linum)))

(defun custom-limun-formatter (num)
 (or
  (ignore-errors
   (propertize
    (format custom-linum-fmt-cache num)
    'face
    custom-linum-face-cache))
  "ERROR"))

(add-hook 'linum-before-numbering-hook #'custom-linum-before-numbering-hook)
(setq linum-format #'custom-limun-formatter)

(set 'custom-linum-override-face-thunk
 (lambda ()
  (if (let (window (get-buffer-window (current-buffer)))
       (window-dedicated-p window))
   'linum-locked
   'linum)))

;; Toggle window dedication
(defun toggle-window-dedicated ()
 "Toggle whether the current active window is dedicated or not"
 (interactive)
 (message
  (if (let (window (get-buffer-window (current-buffer)))
       (set-window-dedicated-p window
        (not (window-dedicated-p window))))
   "Window '%s' is dedicated"
   "Window '%s' is normal")
  (current-buffer)))

(provide 'window-lock)
