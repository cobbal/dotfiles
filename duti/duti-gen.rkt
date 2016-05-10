#lang racket

(define everything
 #hash((org.gnu.Emacs . (public.plain-text
                         public.unix-executable
                         .cpp .cxx .c .cc .h .hpp
                         .nix .drv
                         .sh
                         .py
                         .rkt
                         .el
                         .hs .lhs .cabal
                         .erb .rb
                         .scala))
       (com.SequentialX.Sequential . (.jpg .png))
       (edu.ucsd.cs.mmccrack.bibdesk . (.bib))
       (org.videolan.vlc . (.mkv))))

(for* ([(app exts) everything]
       [ext exts])
 (displayln (~a app " " ext " " 'all)))
