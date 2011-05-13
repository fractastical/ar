#lang scheme

(require "../ac.ss")

(aload (new-arc)
  "../core.arc"
  "../base.arc"
  "../arc.arc"
  "equal-wrt-testing.arc"
  "test.arc"
  "arc.t")
