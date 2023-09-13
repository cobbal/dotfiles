;; -*- lexical-binding: t -*-

(define-minor-mode boxfu
"Toggle Boxfu mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

┌────────────────────────────────────────────────────────┐
│ When Boxfu mode is enabled, magic box drawing happens. │
└────────────────────────────────────────────────────────┘"

 :init-value nil
 :lighter " ⧉"
 :keymap `(("-" . boxfu-insert-intersection)
           ("=" . boxfu-insert-double-intersection)))

(global-set-key (kbd "C-c =") #'boxfu)

(defun boxfu-insert-horizontal ()
 (interactive)
 (insert "─")
 (boxfu-fixup-around-point))

(defun boxfu-insert-vertical ()
 (interactive)
 (insert "│")
 (boxfu-fixup-around-point))

(defconst single-box-table
 '((?-                   )
   ;; (?╶              right)
   (?─              right)
   ;; (?╴         left      )
   (?─         left      )
   (?─         left right)
   ;; (?╷    down           )
   (?│    down           )
   (?┌    down      right)
   (?┐    down left      )
   (?┬    down left right)
   ;; (?╵ up                )
   (?│ up                )
   (?└ up           right)
   (?┘ up      left      )
   (?┴ up      left right)
   (?│ up down           )
   (?├ up down      right)
   (?┤ up down left      )
   (?┼ up down left right)))

(defconst double-box-table
 '((?=                   )
   (?═              right)
   (?═         left      )
   (?═         left right)
   (?║    down           )
   (?╔    down      right)
   (?╗    down left      )
   (?╦    down left right)
   (?║ up                )
   (?╚ up           right)
   (?╝ up      left      )
   (?╩ up      left right)
   (?║ up down           )
   (?╠ up down      right)
   (?╣ up down left      )
   (?╬ up down left right)))

(defvar box-table single-box-table)

(defun box-charp (c)
 (and (assoc c box-table) t))

(defun box-char-with-edges (edges)
 (car
  (rassoc
   (sort (copy-seq edges)
    (lambda (a b)
     (memq b (memq a '(up down left right)))))
   box-table)))

(defun move-dir (dir &optional col-adjust)
 (let* ((col (+ (current-column) (or col-adjust 0)))
        (lcol (- col 1))
        (rcol (+ col 1)))
 (pcase dir
  (`up
   (beginning-of-line)
   (and (= 0 (forward-line -1)) (= (move-to-column col) col)))
  (`down
   (end-of-line)
   (and (= 0 (forward-line 1)) (= (move-to-column col) col)))
  (`left
   (and (>= lcol 1) (= (move-to-column lcol) lcol)))
  (`right
   (= (move-to-column rcol) rcol))
  (`here
   (= (move-to-column col) col)))))

(defun appropriate-char-for-dirs (dirs &optional col-adjust dir-names)
 (box-char-with-edges
  (apply #'append
   (mapcar*
    (lambda (dir dir-name)
     (save-excursion
      (when (and
             (move-dir dir col-adjust)
             (box-charp (char-before)))
       (list dir-name))))
    dirs (or dir-names '(up down left right))))))

(defun boxfu-fixup-at-point ()
 (interactive)
 (let ((c (appropriate-char-for-dirs '(up down left right))))
  (when (/= (char-before) c)
   (delete-char -1)
   (insert c))))

(defun boxfu-fixup-around-point ()
 (dolist (dir '(up down left right))
  (save-excursion
   (when (and (move-dir dir) (box-charp (char-before)))
    (boxfu-fixup-at-point)))))

(defun boxfu-insert-intersection ()
 (interactive)
 (insert (appropriate-char-for-dirs '(up down left here) 1))
 (boxfu-fixup-around-point))

(defun boxfu-insert-double-intersection ()
 (interactive)
 (let ((box-table double-box-table))
  (boxfu-insert-intersection)))

(provide 'boxfu)
