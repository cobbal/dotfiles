;; -*- lexical-binding: t -*-

;; (progn (setq focus-in-hook nil) (setq focus-out-hook nil))

(add-hook 'focus-in-hook
 (lambda ()
  (set-face-attribute 'fringe nil :background "dark red")))

(add-hook 'focus-out-hook
 (lambda ()
  (set-face-attribute 'fringe nil :background "grey30")))

(provide 'frame-focus-hints)
