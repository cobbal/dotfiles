;; -*- lexical-binding: t -*-

(require 'phi-color)

(defvar purpose-color--fixed-color-assignments
 '((edit . 0)
   (search . 1)
   (helm . 2)
   (general . 3)
   ))

(defvar purpose-color--color-assignments
 (let ((tbl (make-hash-table :weakness 'key)))
  (dolist (x purpose-color--fixed-color-assignments)
   (puthash (car x) (cdr x) tbl))
  tbl))

(defun purpose-color--color-for-purpose (purpose)
 (check-type purpose symbolp)
 (let ((n nil))
  (setq n (gethash purpose purpose-color--color-assignments))
  (unless n
   (setq n
    (let* ((claimed-colors (sort (hash-table-values purpose-color--color-assignments) #'<)))
     (loop
      for i = 0 then (+ u 1) finally return i
      for u in claimed-colors
      when (> u i) return i)))
   (puthash purpose n purpose-color--color-assignments))
  (phi-color-n n 1 0.2)))

(defvar-local purpose-color--face-remap-cookie nil)

(defun purpose-color-update-fringe-color-for-current-buffer (&optional win)
 (interactive)

 (let* ((purpose (purpose-buffer-purpose (current-buffer)))
        (color (purpose-color--color-for-purpose purpose)))

  ;; (setq face-remapping-alist nil)
  ;; apparently this function doesn't get defined until later...
  (when (functionp #'face-remap-remove-relative)
   (face-remap-remove-relative purpose-color--face-remap-cookie))
  (setq purpose-color--face-remap-cookie
   (face-remap-add-relative 'linum :background color))))

(provide 'purpose-color)
