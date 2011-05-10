#lang scheme

(require "../ac.ss")

(aload (new-arc)
  "core.arc"
  "arc.arc"
  "arc3.1/strings.arc"
  "tests/equal-wrt-testing.arc"
  "tests/test.arc"
  "tests/strings.t")
