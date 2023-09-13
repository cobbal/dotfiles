;; -*- lexical-binding: t -*-

(require 'color)

(defconst phi-color--phi (/ (+ (sqrt 5) 1) 2))

(defun phi-color-n (n &optional s l)
 (let ((hue (mod (* phi-color--phi n) 1)))
  (apply #'color-rgb-to-hex
   (color-hsl-to-rgb hue (or s 1.0) (or l 0.5)))))

(when nil
 (with-current-buffer "the-colors"
  (erase-buffer)
  (cl-loop for x from 0 to 1000 do
   (when (= 0 (mod x 21))
    (insert "\n"))
   (let ((opoint (point)))
    (insert "  ")
    (put-text-property opoint (point) 'face `(:background ,(phi-color-n x)))))))

(provide 'phi-color)
