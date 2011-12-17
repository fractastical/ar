How to run it
=============

Just call `./arc` and you'll get a REPL. You can also load the file "foo.arc"
with `./arc foo`.

If you would like to load a file *and* run the REPL, use the `-i` or `--repl`
flags: `./arc -i foo`

If you would like to load *all* the files rather than only the first, use the
`-a` or `--all` flags: `./arc -a foo bar qux`

If you would like to run all the unit tests in the `tests/` subdirectory, just
call `./arc run-tests | less`


Details
=======

_Nu_ is an Arc compiler which is derived from _ar_, but differs significantly
in certain ways. Large chunks of the compiler were copied from _ar_, but a lot
was written by me.

  * [New features added in Nu (not found in Arc 3.1 or ar)][new]

  * [Differences between Arc 3.1 and Nu][arc]

  * [Differences between ar and Nu][ar]

  [new]: notes/new%20features.md
  [arc]: notes/changes%20%28ar%29.md
  [ar]:  notes/changes%20%28Arc%203.1%29.md
