;; -*- lexical-binding: t -*-

;; (progn (setq focus-in-hook nil) (setq focus-out-hook nil))

(defvar frame-focus-hint-focus-color "dark red")
(defvar frame-focus-hint-unfocus-color "grey30")

(defun frame-focus-hint-set-color (color)
 (set-face-attribute 'fringe nil :background color))

(add-hook 'focus-in-hook
 (lambda ()
  (frame-focus-hint-set-color frame-focus-hint-focus-color)))

(add-hook 'focus-out-hook
 (lambda ()
  (frame-focus-hint-set-color frame-focus-hint-unfocus-color)))

(frame-focus-hint-set-color frame-focus-hint-focus-color)


(defface linum-locked
 '((t
    :background "blue"
    :inherit (shadow default)))
 "Face for displaying line numbers in the display margin of a
 locked window.")

  ;; (if (let (window (get-buffer-window (current-buffer)))
  ;;      (set-window-dedicated-p window
  ;;       (not (window-dedicated-p window))))


(provide 'frame-focus-hints)
