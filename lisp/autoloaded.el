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

(autoload 'http-get
  "http-get"
  "Get URL in a buffer, and return the process.
You can get the buffer associated with this process using
`process-buffer'.

The optional HEADERS are an alist where each element has the form
\(NAME . VALUE).  Both must be strings and will be passed along with
the request.

With optional argument SENTINEL, the buffer is not shown.  It is the
responsibility of the sentinel to show it, if appropriate.  A sentinel
function takes two arguments, process and message.  It is called when
the process is killed, for example.  This is useful when specifying a
non-persistent connection.  By default, connections are persistent.
Add \(\"Connection\" . \"close\") to HEADERS in order to specify a
non-persistent connection.  Usually you do not need to specify a
sentinel, and `ignore' is used instead, to prevent a message being
printed when the connection is closed.

If you want to filter the content as it arrives, bind
`http-filter-pre-insert-hook' and `http-filter-post-insert-hook'.

The optional argument VERSION specifies the HTTP version to use.  It
defaults to version 1.0, such that the connection is automatically
closed when the entire document has been downloaded.  This will then
call SENTINEL, if provided.  If no sentinel is provided, `ignore' will
be used in order to prevent a message in the buffer when the process
is killed.

CONTENT-TYPE is a coding system to use for the encoding of the url
param value.  Its upper case print name will be used for the server.
Possible values are `iso-8859-1' or `euc-jp' and others.

The coding system of the process is set to `binary', because we need to
distinguish between \\r and \\n.  To correctly decode the text later,
use `decode-coding-region' and get the coding system to use from
`http-headers'."
  t)

(autoload 'objj-mode
 "objj-mode"
 "Major mode for editing Objective-J files."
 t)

(autoload 'light-symbol "light-symbol")

(provide 'autoloaded)
