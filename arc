#! /usr/bin/env racket
#lang racket/base

(require racket/cmdline)
(require racket/path)
(require profile)

(define all   (make-parameter #f))
(define debug (make-parameter #f))
(define repl  (make-parameter #f))

(define arguments
  (command-line
    #:program "Nu Arc"
    #:once-each
    [("-a" "--all")   "Execute every file rather than only the first"
                      (all #t)]
    [("-d" "--debug") "Turns debug mode on, causing extra messages to appear"
                      (debug #t)]
    [("-i" "--repl")  "Always execute the repl"
                      (repl #t)]
    #:args args
    args))

(if (all)
    (current-command-line-arguments (make-vector 0))
    (current-command-line-arguments (list->vector arguments)))

(define exec-dir (path-only (normalize-path (find-system-path 'run-file))))

#|(parameterize ((current-directory exec-dir))
  (namespace-require '(file "01 nu.rkt")))

(init (path->string exec-dir))

(aload (build-path exec-dir "02 arc.arc"))

(unless (null? arguments)
  (if (all)
      (map aload arguments)
      (aload (car arguments))))

(when (or (repl) (null? arguments))
  (aload (build-path exec-dir "03 repl.arc")))|#

(parameterize ((current-namespace (make-base-empty-namespace)))
  ;(profile-thunk (lambda ()
    (parameterize ((current-directory exec-dir))
      (namespace-require '(file "01 nu.rkt")))

    (eval `(init ,(path->string exec-dir)))

    (let ((load (eval 'aload)))
      (load (build-path exec-dir "02 arc.arc"))
      (load (build-path exec-dir "03 utils.arc"))
      (load (build-path exec-dir "04 parameters.arc"))
      (load (build-path exec-dir "05 paths.arc"))
      (load (build-path exec-dir "06 import.arc"))

      (when (debug)
              ;; TODO: hacky
        (eval '(ac-eval '(= debug? t))))

                        ;; TODO: hacky
      (let ((load (eval '(ac-eval 'import1))))
        (unless (null? arguments)
          (if (all)
              (map load arguments)
              (load (car arguments)))))

      (when (or (repl) (null? arguments))
        (load (build-path exec-dir "07 repl.arc"))))
  ;))

  ;; This is to prevent () from being printed when the REPL exits
  (void))
