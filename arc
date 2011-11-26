#! /usr/bin/env mzscheme
#lang scheme

(require scheme/mpair)
(provide (all-defined-out))

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


(define-syntax-rule (ac-eval-in namespace . body)
  (parameterize ((current-namespace             namespace)
                 (compile-allow-set!-undefined  #t)
                 (port-count-lines-enabled      #t))
    (map (lambda (x)
           ((namespace-get namespace 'eval)
             ((namespace-get namespace 'ac-deep-toarc)
               x)))
         'body)))


#|(define ac-read (make-readtable (current-readtable) #\( 'terminating-macro
                  (lambda (ch port src line col pos)
                    (list->mlist (read/recursive port #\( #f)))))|#

;(port-count-lines-enabled #t)

(require profile)
;(require errortrace)
;(require errortrace/errortrace-lib)

;(profiling-enabled #t)
;(profile-paths-enabled #t)

(parameterize ((current-namespace             namespace)
               (compile-allow-set!-undefined  #t)
               (port-count-lines-enabled      #t)
               ;; TODO: !!! This is super slow !!!
               ;(use-compiled-file-paths       (list (string->path ".compiled")))
               ;(compile-enforce-module-constants #f)
               )
  (namespace-require '(only scheme/base #%app #%datum #%top)) ;#%top-interaction
  (namespace-require '(prefix racket- scheme/base))
  (namespace-require '(prefix racket- scheme/mpair))
  (namespace-require '(prefix racket- scheme/system))
  ;(namespace-require '(prefix racket- scheme/foreign))
  ;(namespace-require '(prefix racket- scheme/stxparam))
  (namespace-require '(prefix racket- readline))
  ;(namespace-require '(prefix racket- readline/pread))
  ;(namespace-require '(prefix racket- readline/readline))

  (namespace-set namespace 'namespace     namespace)
  (namespace-set namespace 'namespace-get namespace-get)
  (namespace-set namespace 'namespace-set namespace-set)

  (namespace-set namespace 'ac-eval-all   ac-eval-all)
  (namespace-set namespace 'ac-load       ac-load)

  ;(load/use-compiled "compiler.arc")

  (profile-thunk (lambda ()
    ;(errortrace-annotate (begin
    (ac-load "compiler.arc"    namespace)
    (ac-load "core.arc"        namespace)
    (ac-load "ssyntax.arc"     namespace)
    (ac-load "compat.arc"      namespace)
    (ac-load "arc.arc"         namespace)
    (ac-load "lib/script.arc"  namespace)
    (ac-load "lib/re.arc"      namespace)
    (ac-load "lib/strings.arc" namespace)
    (ac-load "lib/time.arc"    namespace)
    ;(ac-load "repl.arc"        namespace)
  ))

  ;(output-profile-results #t #t)

  ;(display (get-coverage))

  (let ((cli (current-command-line-arguments)))
    (when (> (vector-length cli) 0)
      ;(display cli)
      (ac-load (vector-ref cli 0) namespace)))

  ;; This is to prevent () from being printed when the REPL exits
  (void))
