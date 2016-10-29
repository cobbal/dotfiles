#lang racket

(define everything
 #hash((org.gnu.Emacs . (public.plain-text
                         public.unix-executable
                         .cpp .cxx .c .cc .h .hpp
                         .nix .drv
                         .sh
                         .md
                         .v .lean .hlean
                         .clj
                         .py
                         .rkt
                         .el
                         .rs
                         .tex .dtx .sty
                         .hs .lhs .cabal
                         .erb .rb
                         .scala))
       (com.mothersruin.SuspiciousPackageApp . (.pkg .mpkg))
       (com.att.graphviz . (.dot))
       ;; (com.SequentialX.Sequential . (.jpg .png))
       (cx.c3.Xee3 . (.jpg .png))
       (edu.ucsd.cs.mmccrack.bibdesk . (.bib))
       (org.videolan.vlc . (.mkv))))

(for* ([(app exts) everything]
       [ext exts])
 (displayln (~a app " " ext " " 'all)))
