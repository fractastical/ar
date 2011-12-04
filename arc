#! /usr/bin/env racket
#lang racket/base

#|(require racket/mpair)
(provide (all-defined-out))|#

#|(define namespace (make-base-empty-namespace))

(define namespace-get
  (case-lambda
   ((runtime varname)
    (namespace-variable-value varname #f #f runtime))

   ((runtime varname default)
    (namespace-variable-value varname #f (lambda () default) runtime))))

(define (namespace-set runtime varname value)
  (namespace-set-variable-value! varname value #f runtime))|#


#|(define (ac-eval-all in runtime)
  (let ((x (read in)))
    (if (eof-object? x)
          null
        (begin (eval x runtime)
               (ac-eval-all in runtime)))))

(define (ac-load filename (runtime namespace))
  (call-with-input-file filename
    (lambda (in)
      ((namespace-get namespace 'ac-eval-all) in runtime))))|#


#|(define-syntax-rule (ac-eval-in namespace . body)
  (parameterize ((current-namespace             namespace)
                 (compile-allow-set!-undefined  #t)
                 (port-count-lines-enabled      #t))
    (map (lambda (x)
           ((namespace-get namespace 'eval)
             ((namespace-get namespace 'ac-deep-toarc)
               x)))
         'body)))|#


#|(define ac-read (make-readtable (current-readtable) #\( 'terminating-macro
                  (lambda (ch port src line col pos)
                    (list->mlist (read/recursive port #\( #f)))))|#

;(port-count-lines-enabled #t)

;(require "compiler.arc")
(require racket/cmdline)
(require racket/path)
(require readline)
(require profile)
;(require errortrace)
;(require errortrace/errortrace-lib)

(define repl (make-parameter #f))
(define all  (make-parameter #f))

(define arguments
  (command-line
    #:program "Nu Arc"
    #:once-each
    [("-i" "--repl") "Always execute the repl"
                     (repl #t)]
    [("-a" "--all")  "Execute every file rather than only the first"
                     (all #t)]
    #:args filenames
    filenames))

(if (all)
      (current-command-line-arguments (make-vector 0))
    (current-command-line-arguments (list->vector arguments)))

;(profiling-enabled #t)
;(profile-paths-enabled #t)

;(compile-enforce-module-constants #f)

#|(define namespace (make-base-empty-namespace))

(define (ac-eval-all in runtime)
  (let ((x (read in)))
    (if (eof-object? x)
          null
        (begin (eval x runtime)
               (ac-eval-all in runtime)))))

(define (ac-load filename (runtime namespace))
  (call-with-input-file filename
    (lambda (in)
      ((namespace-variable-value 'ac-eval-all #f #f runtime) in runtime))))|#


;(define exec-dir (path->string (path-only (normalize-path (find-system-path 'run-file)))))

#|(namespace-require '(only racket/private/pre-base
                            module #%app #%top #%top-interaction))|#

(define exec-dir
  (path->string (path-only (normalize-path (find-system-path 'run-file)))))

(parameterize ((current-namespace (make-base-empty-namespace)))
  ;(namespace-require '(only racket/base #%app #%datum #%top #%top-interaction))
  #|(namespace-require '(only racket/private/pre-base
                            module #%app #%top #%top-interaction #%datum))|#
  #|(namespace-require '(only racket/private/pre-base
                            #%app #%top #%top-interaction))
  (namespace-require '(only racket/private/pre-base
                            #%require #%app #%top #%top-interaction))|#
  #|(namespace-require '(only racket/private/pre-base
                            #%top-interaction #%app #%top #%datum #%require))|#
  (namespace-require/copy '(only racket/private/pre-base
                             #%top-interaction #%app #%top #%datum))

  (namespace-require/copy '(prefix racket- racket/base))
  ;(namespace-require '(prefix racket- racket/base))
  ;(namespace-require '(prefix racket- racket/base))
  ;(namespace-require '(prefix racket- racket/mpair))
  ;(namespace-require '(prefix racket- racket/path))
  ;(namespace-require '(prefix racket- racket/system))
  ;(namespace-require "compiler.arc")

  (namespace-set-variable-value! 'exec-dir* exec-dir #f)

  (profile-thunk (lambda ()
    (load/use-compiled (build-path exec-dir "compiler.arc"))
    ;(load/use-compiled (string->path "compiler.arc"))

     ;(displayln (namespace-mapped-symbols))

     ;(parameterize ((current-namespace (module->namespace "compiler.arc"))))

    (let (;(exec-dir (namespace-variable-value 'exec-dir* #f))
          (ac-load  (namespace-variable-value 'ac-load #f)))

          ;(ac-load "compiler.arc")
          (ac-load "core.arc")
          (ac-load "ssyntax.arc")
          (ac-load "compat.arc")
          (ac-load "arc.arc")
          (ac-load "extra.arc")
          ;(ac-load "lib/re.arc")
          ;(ac-load "lib/script.arc")
          (ac-load "import.arc")

          ;(ac-load "lib/strings.arc")
          ;(ac-load "lib/time.arc")
          ;(ac-load "lib/compile.arc")

      (unless (null? arguments)
        (if (all)
              (map ac-load arguments)
            (ac-load (car arguments))))

      (when (or (repl) (null? arguments))
        (ac-load "repl.arc")))
  ))

  ;; This is to prevent () from being printed when the REPL exits
  (void))

#|(parameterize ((current-namespace namespace))
  (namespace-require '(only racket/base #%app #%datum #%top #%top-interaction))
  (namespace-require '(prefix racket- racket/base))
  (namespace-require '(prefix racket- racket/mpair))
  (namespace-require '(prefix racket- racket/system))
  ;(namespace-require '(prefix racket- racket/foreign))
  ;(namespace-require '(prefix racket- racket/stxparam))
  (namespace-require '(prefix racket- readline))
  ;(namespace-require '(prefix racket- readline/pread))
  ;(namespace-require '(prefix racket- readline/readline))

  ;(namespace-set namespace 'namespace     namespace)
  ;(namespace-set namespace 'namespace-get namespace-get)
  ;(namespace-set namespace 'namespace-set namespace-set)

  ;(namespace-set namespace 'ac-eval-all   ac-eval-all)
  ;(namespace-set namespace 'ac-load       ac-load)

  ;(load/use-compiled "compiler.arc")

  (namespace-set-variable-value! 'ac-eval-all  ac-eval-all  #f)
  (namespace-set-variable-value! 'ac-load      ac-load      #f)

  (namespace-set-variable-value! 'exec-dir*    exec-dir     #f)

  (parameterize ((compile-allow-set!-undefined  #t)
                 (port-count-lines-enabled      #t)
                 ;; TODO: !!! This is super slow !!!
                 ;(use-compiled-file-paths       (list (string->path ".compiled")))
                 ;(compile-enforce-module-constants #f)
                 )
    (let ((load (namespace-variable-value 'ac-load #f)))
      ;(profile-thunk (lambda ()
        ;(namespace-require "compiler.rkt")
        ;(display (variable-reference-constant? 'complement))
        ;(errortrace-annotate (begin))

        (parameterize ((current-directory exec-dir))
          (load "compiler.arc")
          (load "core.arc")
          (load "ssyntax.arc")
          (load "compat.arc")
          (load "arc.arc")
          (load "extra.arc")
          (load "lib/re.arc")
          (load "lib/script.arc")
          (load "import.arc")
          ;(load "lib/strings.arc")
          ;(load "lib/time.arc")
          ;(load "lib/compile.arc")
        )
      ;))

      (if (> (length arguments) 0)
            (begin (load (car arguments))
                   (when (repl) (load (string-append exec-dir "repl.arc"))))
          (load (string-append exec-dir "repl.arc")))))

  ;(output-profile-results #t #t)

  ;(display (get-coverage))

  ;; This is to prevent () from being printed when the REPL exits
  (void))|#
