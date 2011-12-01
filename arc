#! /usr/bin/env racket
#lang racket

#|(require racket/mpair)
(provide (all-defined-out))|#

#|(define namespace (make-base-empty-namespace))

(define namespace-get
  (case-lambda
   ((runtime varname)
    (namespace-variable-value varname #t #f runtime))

   ((runtime varname default)
    (namespace-variable-value varname #t (lambda () default) runtime))))

(define (namespace-set runtime varname value)
  (namespace-set-variable-value! varname value #t runtime))|#


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

(require profile)
;(require errortrace)
;(require errortrace/errortrace-lib)

;(profiling-enabled #t)
;(profile-paths-enabled #t)

;(compile-enforce-module-constants #f)

(define namespace (make-base-empty-namespace))

(define (ac-eval-all in runtime)
  (let ((x (read in)))
    (if (eof-object? x)
          null
        (begin (eval x runtime)
               (ac-eval-all in runtime)))))

(define (ac-load filename (runtime namespace))
  (call-with-input-file filename
    (lambda (in)
      ((namespace-variable-value 'ac-eval-all #t #f runtime) in runtime))))


(parameterize ((current-namespace namespace))
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

  (namespace-set-variable-value! 'ac-eval-all  ac-eval-all)
  (namespace-set-variable-value! 'ac-load      ac-load)

  (parameterize ((compile-allow-set!-undefined  #t)
                 (port-count-lines-enabled      #t)
                 ;; TODO: !!! This is super slow !!!
                 ;(use-compiled-file-paths       (list (string->path ".compiled")))
                 ;(compile-enforce-module-constants #f)
                 )
    (let ((load (namespace-variable-value 'ac-load)))
      (profile-thunk (lambda ()
        ;(namespace-require "compiler.rkt")
        ;(display (variable-reference-constant? 'complement))

        ;(errortrace-annotate (begin))
        (load "compiler.arc")
        (load "core.arc")
        (load "ssyntax.arc")
        (load "compat.arc")
        (load "arc.arc")
        (load "lib/script.arc")
        (load "lib/re.arc")
        (load "lib/strings.arc")
        (load "lib/time.arc")
        (load "lib/compile.arc")
        ;(load "repl.arc")
      ))

      (let ((cli (current-command-line-arguments)))
        (if (> (vector-length cli) 0)
              (load (vector-ref cli 0))
            (load "repl.arc")))))

  ;(output-profile-results #t #t)

  ;(display (get-coverage))

  ;; This is to prevent () from being printed when the REPL exits
  (void))
