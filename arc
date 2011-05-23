#!/usr/bin/env mzscheme
#lang scheme/load

(require scheme/cmdline)

(define run-repl #f)
(define exec-all #f)
(define files-to-load '())

(command-line
 #:once-each
 (("--repl")
    "run the REPL even when specifying files"
    (set! run-repl #t))
 (("-a" "--all")
    "execute all files, rather than only the first"
    (set! exec-all #t))

 #:args files
 (set! files-to-load files))

(define srcdir
  (path->string
   (let-values (((base _2 _3)
                 (split-path (normalize-path
                              (find-system-path 'run-file)))))
     base)))

(namespace-require `(file ,(string-append srcdir "ac.ss")))

(let ((arc (new-arc)))
  (set arc 'srcdir srcdir)

  (parameterize ((current-command-line-arguments
                   (list->vector files-to-load)))

    (when exec-all
      (current-command-line-arguments #()))

    (aload arc (string-append (g srcdir) "core.arc")
               (string-append (g srcdir) "base.arc")
               (string-append (g srcdir) "arc.arc"))

    (let ((load        (g load))
          (load-curdir (g load-curdir)))

      (parameterize ((current-directory (g srcdir)))
        (load "arc3.1/backcompat.arc")
        (load "arc3.1/strings.arc"))

      (cond (exec-all
              (for-each load files-to-load))
            ((pair? files-to-load)
              (load-curdir (car files-to-load))))

      (when (or run-repl (null? files-to-load))
        (parameterize ((current-directory (g srcdir)))
          (load "repl.arc"))
        (noprint ((g repl)))))))
