#lang scheme

(require "../ac.ss")

(aload (new-arc)
  "../core.arc"
  "../arc.arc"
  "../arc3.1/strings.arc"
  "equal-wrt-testing.arc"
  "test.arc"
  "strings.t")
