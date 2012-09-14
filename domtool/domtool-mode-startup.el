(autoload (quote domtool-mode) "domtool-mode/domtool-mode" "\
Major Mode for editing Domtool files." t nil)

(add-to-list (quote auto-mode-alist) (quote ("\\.\\(dtl\\|com\\|net\\|org\\|edu\\|mil\\|biz\\|info\\|name\\|be\\|ca\\|cc\\|de\\|fr\\|in\\|mu\\|se\\|uk\\|us\\|ws\\)$" . domtool-mode)))

(provide 'domtool-mode-startup)
