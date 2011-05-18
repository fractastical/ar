#lang scheme

;; tests that need to be run as root

(require "../ac.ss")

(aload (new-arc)
       "../core.arc"
       "../base.arc"
       "../arc.arc"
       "../arc3.1/backcompat.arc"
       "../io.arc"
       "equal-wrt-testing.arc"
       "test.arc"
       "io-root.t")
