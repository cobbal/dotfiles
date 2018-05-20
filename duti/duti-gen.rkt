#lang racket

(define everything
 #hash((org.gnu.Emacs . (public.plain-text
                         public.unix-executable
                         .cpp .cxx .c .cc .h .hpp .m .mm
                         .jl
                         .sml .ml .mli .mly .mll
                         .nix .drv
                         .sh
                         .v .lean .hlean
                         .clj
                         .js .jsm .json
                         .idl
                         .py
                         .rkt
                         .el
                         .rs
                         .tex .dtx .sty
                         .hs .lhs .cabal
                         .cfg
                         .patch
                         .erb .rb
                         .scala))
       (com.brettterpstra.marked2 . (.md))
       (com.mothersruin.SuspiciousPackageApp . (.pkg .mpkg))
       (com.att.graphviz . (.dot))
       ;; (com.SequentialX.Sequential . (.jpg .png))
       (cx.c3.Xee3 . (.jpg .png))
       (edu.ucsd.cs.mmccrack.bibdesk . (.bib))
       (org.videolan.vlc . (.mkv))))

(for* ([(app exts) everything]
       [ext exts])
 (displayln (~a app " " ext " " 'all)))
