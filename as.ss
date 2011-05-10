#!/usr/bin/env racket
#lang scheme

(require "ac.ss")

(let ((arc (new-arc)))
  (aload arc
    "core.arc"
    "arc.arc"
    "arc3.1/strings.arc"
    "repl.arc")
  (noprint ((get arc 'repl))))
