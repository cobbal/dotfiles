;; -*- lexical-binding: t -*-

(defun my-window-tree (&optional window)
 (setq window (or window (frame-root-window)))
 (let ((horizontal-combo (window-combination-p window t))
       (acc '())
       (first-child (window-child window)))
  (cl-flet ((min-edge (window)
             (nth (if horizontal-combo 0 1) (window-edges window)))
            (max-edge (window)
             (nth (if horizontal-combo 2 3) (window-edges window))))
   (if (not first-child)
    (window-buffer window)
    `(,(if horizontal-combo 'horiz 'vert)
      ,(window-total-size window horizontal-combo)
      . ,(cl-loop
          for child = first-child then (window-next-sibling child)
          while child
          collect `(,(-
                      (min-edge child)
                      (min-edge window))
                    ,(my-window-tree child))))))))

(defun transpose-window-splits (&optional frame)
 (interactive)
 (let* ((window (frame-root-window frame))
        (tree (my-window-tree window)))
  (delete-other-windows)
  (cl-labels ((rec (tree window)
               (pcase tree
                (`(,old-dir ,old-size . ,children)
                 (let* ((new-dir (if (eq old-dir 'horiz) 'below 'right))
                        (new-size (window-total-size window (eq old-dir 'vert)))
                        (windows
                         (cl-loop
                          for child in children
                          for new-win = window then (split-window new-win (round (* (car child) (/ new-size (float old-size)))) new-dir)
                          collect new-win)))
                  (cl-loop
                   for child in children
                   for win in windows
                   do (rec (cadr child) win))))
                (buffer
                 (set-window-buffer window buffer)))))
   (rec tree (selected-window)))))

(provide 'transpose-window-splits)
