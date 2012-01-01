#! /usr/bin/env racket
#lang racket/base

(require racket/cmdline)
(require racket/path) ;; TODO: if I don't require this, it's very slow to startup
(require readline)
(require profile)

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
    #:args args
    args))

(if (all)
      (current-command-line-arguments (make-vector 0))
    (current-command-line-arguments (list->vector arguments)))

(define exec-dir (path-only (normalize-path (find-system-path 'run-file))))

(parameterize ((current-namespace (make-base-empty-namespace)))
  (profile-thunk (lambda ()
    (parameterize ((current-directory exec-dir))
      ;(namespace-require 'racket/base)
      ;(load "01 arc.rkt")
      (namespace-require '(file "01 ac.rkt"))
      ;(namespace-require '(file "02 arc.rkt"))
      ;(namespace-require '(file "02 nu.rkt"))
      ;(namespace-require '(file "01 ac.rkt.bak (1)"))
      (eval `(init ,(path->string exec-dir))))

      ;(init ,(path->string exec-dir))

    (let ((load (eval 'aload)))
    ;(let ((load (eval '(var 'importfn1)))) ) ;; TODO: better pattern than using var, should access the variable directly
      (unless (null? arguments)
        (if (all)
              (map load arguments)
            (load (car arguments))))

      (when (or (repl) (null? arguments))
        (eval `(repl))))
  ))

  ;; This is to prevent () from being printed when the REPL exits
  (void))
