#lang scheme

(require "../ac.ss")

(aload (new-arc)
  "arc.arc"
  "tests/equal-wrt-testing.arc"
  "tests/test.arc"
  "tests/arc.t")
