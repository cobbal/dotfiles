(spaceline-define-segment wc-segment
 (if (use-region-p)
  (format "%d,%d,%d"
   (abs (- (point) (mark)))
   (count-words-region (point) (mark))
   (abs (- (line-number-at-pos (point))
         (line-number-at-pos (mark)))))
  (format "%d,%d,%d"
   (- (point-max) (point-min))
   (count-words-region (point-min) (point-max))
   (line-number-at-pos (point-max)))))
