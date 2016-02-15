#lang racket

(define everything
 #hash((org.gnu.Emacs . (public.plain-text
                         public.unix-executable
                         .cpp .cxx .c .cc
                         .nix
                         .sh
                         .py
                         .rkt
                         .el
                         .scala))
       (com.SequentialX.Sequential . (.jpg))
       (org.videolan.vlc . (.mkv))))

(for* ([(app exts) everything]
       [ext exts])
 (displayln (~a app " " ext " " 'all)))
