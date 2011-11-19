#! /usr/bin/env mzscheme
#lang scheme

(require scheme/mpair)

(define namespace (make-base-empty-namespace))

(define namespace-get
  (case-lambda
   ((runtime varname)
    (namespace-variable-value varname #t #f runtime))

   ((runtime varname default)
    (namespace-variable-value varname #t (lambda () default) runtime))))

(define (namespace-set runtime varname value)
  (namespace-set-variable-value! varname value #t runtime))


(define (ac-eval-all in runtime)
  (let ((x (read in)))
    (if (eof-object? x)
          null
        (begin (eval x runtime)
               (ac-eval-all in runtime)))))

(define (ac-load filename (runtime namespace))
  (call-with-input-file filename
    (lambda (in)
      ((namespace-get namespace 'ac-eval-all) in runtime))))


#|(define ac-read (make-readtable (current-readtable) #\( 'terminating-macro
                  (lambda (ch port src line col pos)
                    (list->mlist (read/recursive port #\( #f)))))|#

(parameterize ((current-namespace             namespace)
               (compile-allow-set!-undefined  #t)
               ;(compile-enforce-module-constants #f)
               )
  (namespace-require '(only scheme/base #%app #%datum #%top)) ;#%top-interaction
  (namespace-require '(prefix racket- scheme/base))
  (namespace-require '(prefix racket- scheme/mpair))
  (namespace-require '(prefix racket- scheme/system))
  ;(namespace-require '(prefix racket- scheme/foreign))
  ;(namespace-require '(prefix racket- scheme/stxparam))

  (namespace-set namespace 'namespace     namespace)
  (namespace-set namespace 'namespace-get namespace-get)
  (namespace-set namespace 'namespace-set namespace-set)

  (namespace-set namespace 'ac-eval-all   ac-eval-all)
  (namespace-set namespace 'ac-load       ac-load)

  (ac-load "compiler.arc" namespace)
  (ac-load "core.arc"     namespace)
  (ac-load "ssyntax.arc"  namespace)
  (ac-load "compat.arc"   namespace)
  (ac-load "arc.arc"      namespace)
  (ac-load "lib/time.arc"      namespace)
  (ac-load "repl.arc"     namespace))
