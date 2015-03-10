(autoload 'nav
  "nav"
  "Run nav-mode in a narrow window on the left side."
  t)

(autoload 'http-twiddle-mode
  "http-twiddle"
  "Major mode for twiddling around with HTTP requests and sending them.
Use `http-twiddle-mode-send' (\\[http-twiddle-mode-send]) to send the request."
  t)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/clojure-mode"))
(autoload 'clojure-mode "clojure-mode" nil t)

(autoload 'LilyPond-mode "lilypond-mode")
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)

(autoload 'objj-mode
 "objj-mode"
 "Major mode for editing Objective-J files."
 t)

(autoload 'light-symbol "light-symbol")

(provide 'autoloaded)
