Timing notes
============

  * fexprs are shockingly costly:

        > (do (def foo (a b) (list a b))
              (eval #`(timeit (% ,(ac-compile '(let a 5
                                                 (foo 1 2)))))))

        > (do (def foo (a b) (list a b))
              (timeit (let a 5
                        (foo 1 2))))
        iter: 8,520,845  gc: 68  mem: 10783456
        iter: 9,082,125  gc: 72  mem: -482984
        iter: 8,231,142  gc: 72  mem: 7929048
        iter: 8,770,578  gc: 68  mem: 11019040

        > (do (fexpr foo (env a b) (list a b))
              (timeit (let a 5
                        (foo 1 2))))
        iter: 1,640,542  gc: 152  mem: 2066224
        iter: 1,564,720  gc: 136  mem: -1799440
        iter: 1,713,624  gc: 196  mem: 3967376

    But that's because it's capturing the lexical environment of `timeit`:

        > (do (fexpr foo (env a b) (list a b))
              (eval #`(timeit (% ,(ac-compile '(let a 5
                                                 (foo 1 2)))))))
        iter: 9,139,868  gc: 180  mem: 6332872

        > (do (fexpr foo (env a b) (list a b))
              (eval #`(timeit (% ,(ac-compile
                '(let a 5
                   (let a 5
                     (let a 5
                       (let a 5
                         (let a 5
                           (foo 1 2)))))))))))
        iter: 8,973,283  gc: 172  mem: 470584

        > (do (fexpr foo (env a b) (list a b))
              (eval #`(timeit (% ,(ac-compile
                '(with (a 1
                        b 2
                        c 3
                        d 4
                        ;e 5
                        )
                   (foo 1 2)))))))
        iter: 7,590,154  gc: 304  mem: 10051704
        iter: 8,177,674  gc: 220  mem: 11069152
        iter: 7,982,771  gc: 340  mem: 11486456


        ((racket-lambda nil ((racket-lambda nil (racket-if (#<fn:ac-true> (#<fn:bound> #<quoted>)) (#<fn:redefine-warning> #<quoted>) nil) (racket-begin (#<fn:sref> sig #<quoted> #<quoted>) (#<fn:ac-assign-global-raw> #<namespace:0> (racket-quote foo)
        (racket-lambda (a b) (#<fn:ac-funcall2> (#<fn:ac-lookup-global> list) a b))))))
        ((racket-lambda (g314) (racket-let* ((g311 (#<fn:car> g314)) (g314 (#<fn:cdr> g314)) (g312 (#<fn:car> g314)) (g314 (#<fn:cdr> g314)) (g313 (#<fn:car> g314))) (#<fn:prn> "iter: " (#<fn:commafy> g311) "  gc: " g312 "  mem: " g313) nil)) ((racket-lambda (g316 g317 g318 g319) (#<fn:ac-funcall0> ((racket-lambda (g315) (racket-begin (racket-set! g315 (racket-lambda nil ((racket-lambda (a) (#<fn:ac-funcall2> (#<fn:ac-lookup-global> foo) 1 2)) 5) (racket-begin (racket-set! g316 (#<fn:+> g316 1)) g316) (racket-if (#<fn:ac-true> (#<fn:<> (#<fn:-> (#<fn:msec>) g317) 10000)) (#<fn:ac-funcall0> g315) nil))) g315)) nil)) (#<fn:list> g316 (#<fn:-> (#<fn:gc-msec>) g318) (#<fn:-> (#<fn:memory>) g319))) 0 (#<fn:msec>) (#<fn:gc-msec>) (#<fn:memory>)))))


        ((racket-lambda nil ((racket-lambda nil (racket-if (#<fn:ac-true> (#<fn:bound> #<quoted>)) (#<fn:redefine-warning> #<quoted>) nil) (#<fn:ac-assign-global-raw> #<namespace:0> (racket-quote foo) (#<fn:annotate> #<quoted> (racket-lambda (env a b) (#<fn:ac-funcall2> (#<fn:ac-lookup-global> list) a b))))))
        ((racket-lambda (g323) (racket-let* ((g320 (#<fn:car> g323)) (g323 (#<fn:cdr> g323)) (g321 (#<fn:car> g323)) (g323 (#<fn:cdr> g323)) (g322 (#<fn:car> g323))) (#<fn:prn> "iter: " (#<fn:commafy> g320) "  gc: " g321 "  mem: " g322) nil)) ((racket-lambda (g325 g326 g327 g328) (#<fn:ac-funcall0> ((racket-lambda (g324) (racket-begin (racket-set! g324 (racket-lambda nil ((racket-lambda (a) (#<fn> (#<fn:list> #<quoted> g325 #<quoted> g326 #<quoted> g327 #<quoted> g328 #<quoted> g324 #<quoted> a) #<quoted> #<quoted>)) 5) (racket-begin (racket-set! g325 (#<fn:+> g325 1)) g325) (racket-if (#<fn:ac-true> (#<fn:<> (#<fn:-> (#<fn:msec>) g326) 10000)) (#<fn:ac-funcall0> g324) nil))) g324)) nil)) (#<fn:list> g325 (#<fn:-> (#<fn:gc-msec>) g327) (#<fn:-> (#<fn:memory>) g328))) 0 (#<fn:msec>) (#<fn:gc-msec>) (#<fn:memory>)))))

  * Raw global variables are fastest, followed by parameters, followed by
    looking up a global variable at runtime:

        > (do (= foo 5)
              (timeit (% foo)))
        iter: 15,808,324  gc: 0  mem: 747424

        > (do (= foo (racket-make-parameter 5))
              (timeit (% (foo))))
        iter: 9,740,432  gc: 0  mem: 637640

        > (do (parameter foo 5)
              (timeit foo))
        iter: 7,638,069  gc: 0  mem: 699064

        > (do (make-parameter foo (racket-make-parameter 5))
              (timeit foo))
        iter: 7,146,990  gc: 0  mem: 567968

        > (do (= foo 5)
              (timeit (% (racket-namespace-variable-value (racket-quote foo)))))
        iter: 5,029,164  gc: 60  mem: 11132264

  * `racket-begin` is a bit faster than using `fn`:

        ((fn () ...))
        Total cpu time observed: 1038ms (out of 1080ms)
        Number of samples taken: 26 (once every 40ms)

        (racket-begin ...)
        Total cpu time observed: 978ms (out of 1024ms)
        Number of samples taken: 25 (once every 39ms)


        > (timeit (% (racket-begin 1 2 3)))
        iter: 15,465,466  gc: 0  mem: 733112

        > (timeit ((fn () 1 2 3)))
        iter: 15,356,915  gc: 0  mem: 754384

  * How the hell is ar so fast for sorting?

        > (let bb (n-of 1000 (rand 50)) (time10 (bestn 100 > bb)))
        Nu time:      247 msec
        Nu time:      149 msec
        Arc 3.1 time: 190 msec
        ar time:      62  msec

        > (let td (n-of 10000 (rand 100)) (time (sort < td)) 1)
        Nu time:      334 msec
        Arc 3.1 time: 223 msec
        ar time:      81  msec


        > (do (time (n-of 1000 (rand 50))) nil)

        > (do (time (repeat 1000 (bestn 100 > '(1 2 3 4 5 6 7 8 9 10)))) nil)


        > (time (repeat 10000000 (+ 1 2)))
        ar time:      6387 msec
        Nu time:      6011 msec
        Arc 3.1 time: 5579 msec


        > (do (time (n-of 10000 (rand 100))) nil)
        Arc 3.1 time: 53 msec
        ar time:      48 msec
        Nu time:      17 msec

        > (do (time (repeat 10000 (< 1 2))) nil)
        ar time:      14 msec
        Arc 3.1 time: 8  msec
        Nu time:      9  msec

        > (do (time (repeat 10000 (> 1 2))) nil)
        ar time:      13 msec
        Arc 3.1 time: 14 msec
        Nu time:      11 msec

  * `nrev` should be used in place of `rev` whenever it is safe to do so:

        > (rev (list 1 2 3 4 5))
        iter: 679,497  gc: 56  mem: 2698712
        iter: 678,302  gc: 60  mem: 1774504
        iter: 675,079  gc: 60  mem: -649728

        > (nrev (list 1 2 3 4 5))
        iter: 1,273,115  gc: 88  mem: -4054776
        iter: 1,260,730  gc: 112  mem: 2590656
        iter: 1,278,143  gc: 108  mem: -801360

  * `direct-calls` only has a tiny (negligible) speed boost in Racket v5.1.3:

        > (+ 1 2)
        iter: 2,510,765  gc: 0  mem: 504
        iter: 2,647,141  gc: 0  mem: 504
        iter: 2,510,846  gc: 0  mem: 824

        > (% (+ 1 2))
        iter: 2,795,937  gc: 0  mem: 984
        iter: 2,673,457  gc: 0  mem: 824
        iter: 2,821,147  gc: 0  mem: 1048

  * `do` vs `racket-begin`:

        > (do 1 2 3)
        iter: 3,914,909  gc: 0  mem: 824
        iter: 3,860,042  gc: 0  mem: 824
        iter: 3,820,783  gc: 0  mem: 824

        > (% (racket-begin 1 2 3))
        iter: 3,863,665  gc: 0  mem: 984
        iter: 3,804,157  gc: 0  mem: 824
        iter: 3,875,776  gc: 0  mem: 824

  * `apply` vs function application:

        > (timeit (apply idfn (list 'x)))
        iter: 2,117,009  gc: 88  mem: -4760624 (ac-arg-list*)
        iter: 2,092,668  gc: 120  mem: 1844448 (ac-arg-list*)
        iter: 1,987,647  gc: 80  mem: -5397256 (racket-apply list*)

        > (timeit (idfn 'x))
        iter: 3,614,741  gc: 0  mem: 1454864


        > (timeit (apply list 1 2 3 4 (list 5)))
        iter: 1,292,642  gc: 300  mem: -3410104 (ac-arg-list*)
        iter: 1,088,763  gc: 248  mem: -7513680 (racket-apply list*)

        > (timeit (list 1 2 3 4 5))
        iter: 2,585,728  gc: 232  mem: -2848912

  * How is calling `(list)` faster than just using `nil`?!

        > (timeit (list))
        iter: 4,325,157  gc: 0  mem: 2343880

        > (timeit nil)
        iter: 3,878,884  gc: 0  mem: 2755496


        > (list)
        iter: 3,494,930  gc: 0  mem: 1624
        iter: 3,595,272  gc: 0  mem: 664
        iter: 3,455,980  gc: 0  mem: 984

        > nil
        iter: 3,270,729  gc: 0  mem: 824
        iter: 3,410,667  gc: 0  mem: 824
        iter: 3,434,929  gc: 0  mem: 984

  * Using `maplast` is slower than using `(eval #\`(do ...))` but has less
    garbage collection:

        > (maplast eval '((+ 1 2) (+ 2 3)))
        iter: 52,314  gc: 148  mem: 3510312
        iter: 52,357  gc: 172  mem: 4446592
        iter: 52,553  gc: 160  mem: -5869064

        > (eval #`(do (+ 1 2) (+ 2 3)))
        iter: 59,257  gc: 688  mem: 21830208
        iter: 57,898  gc: 913  mem: -15126832
        iter: 58,214  gc: 860  mem: 2917064

  * Unquoting macros and functions inside a macro has a small speed boost:

        Normal macros:
          Total cpu time observed: 1248ms (out of 1304ms)
          Number of samples taken: 31 (once every 40ms)

        Unquoted macros:
          Total cpu time observed: 1154ms (out of 1208ms)
          Number of samples taken: 29 (once every 40ms)

  * `procedure-rename` has a very small (12%) cost:

        Direct:
          > (timeit 'foo)
          iter: 3,443,284  gc: 0  mem: 917376

        procedure-rename:
          > (timeit 'foo)
          iter: 3,083,123  gc: 0  mem: 1144280

  * Namespaces are costly:

        #<namespace (len 1)>
        > (timeit (+ 1 2))
        iter: 3,087,359  gc: 0  mem: 1150720

        #<namespace (len 2)>
        > (timeit (+ 1 2))
        iter: 673,656  gc: 208  mem: 2043552

        #<namespace (len 3)>
        > (timeit (+ 1 2))
        iter: 542,289  gc: 196  mem: 3957072

        #<namespace (len 4)>
        > (timeit (+ 1 2))
        iter: 451,246  gc: 240  mem: -5491480

        #<namespace (len 5)>
        > (timeit (+ 1 2))
        iter: 383,279  gc: 208  mem: 1315664

        #<namespace (len 6)>
        > (timeit (+ 1 2))
        iter: 339,967  gc: 204  mem: -2596248

        #<namespace (len 7)>
        > (timeit (+ 1 2))
        iter: 303,009  gc: 244  mem: -9807392

        #<namespace (len 8)>
        > (timeit (+ 1 2))
        iter: 271,724  gc: 232  mem: 2706624

        #<namespace (len 9)>
        > (timeit (+ 1 2))
        iter: 237,372  gc: 456  mem: -6715504

        #<namespace (len 10)>
        > (timeit (+ 1 2))
        iter: 221,411  gc: 276  mem: 823096


        1   2      3    4    6    6    7
        0%  1732%  31%  22%  20%  17%  12%

        06 77 82 83 86 89 ... 92 95 98 100

        #<namespace (len 1)>
        > (timeit ''foo)
        iter: 3,550,488  gc: 0  mem: 966424

        #<namespace (len 2)>
        > (timeit ''foo)
        iter: 193,802  gc: 220  mem: 2369368

        #<namespace (len 3)>
        > (timeit ''foo)
        iter: 148,374  gc: 224  mem: -10849240

        #<namespace (len 4)>
        > (timeit ''foo)
        iter: 121,625  gc: 240  mem: -535848

        #<namespace (len 5)>
        > (timeit ''foo)
        iter: 101,397  gc: 224  mem: 1595008

        #<namespace (len 6)>
        > (timeit ''foo)
        iter: 86,834  gc: 236  mem: 5720480

        #<namespace (len 7)>
        > (timeit ''foo)
        iter: 77,401  gc: 244  mem: 1045880

  * Use `quote` whenever possible, instead of `list`:

        > '(1 2 3 4 5)
        iter: 1,428,372  gc: 0  mem: 1744

        > (list 1 2 3 4 5)
        iter: 475,625    gc: 23  mem: 221516


        ;; TODO: redo these comparisons, in light of the above
        > (timeit '(foo bar qux))
        iter: 3,333,270  gc: 0  mem: 1688488

        > (timeit (list 'foo 'bar 'qux))
        iter: 3,354,804  gc: 52  mem: 2700912

  * `path->complete-path` is quite fast:

        > (timeit (racket-path->string:racket-path->complete-path (expandpath "~/foobar")))
        iter: 40,988  gc: 220  mem: -6044496

        > (timeit (joinpath cwd "~/foobar"))
        iter: 20,340  gc: 296  mem: 14891960

  * Assigning to a global variable is basically just as fast as assigning to
    the namespace:

        > (timeit (= foo 'bar))
        iter: 129,091  gc: 172  mem: -5105664

        > (timeit (sref (namespace) 'bar 'foo))
        iter: 124,680  gc: 196  mem: 15249328

  * Nu startup times took a hit when switching to `namespace-variable-value`:

        Direct access:
          Total cpu time observed: 1334ms (out of 1432ms)
          Number of samples taken: 36 (once every 37ms)

        namespace-variable-value:
          Total cpu time observed: 2150ms (out of 2252ms)
          Number of samples taken: 52 (once every 41ms)


        Direct access:
          Total cpu time observed: 1548ms (out of 1668ms)
          Number of samples taken: 194 (once every 8ms)

        namespace-variable-value #f:
          Total cpu time observed: 1846ms (out of 1908ms)
          Number of samples taken: 42 (once every 44ms)

        namespace-variable-value #f:
          Total cpu time observed: 1988ms (out of 2040ms)
          Number of samples taken: 43 (once every 46ms)

        namespace-variable-value #f:
          Total cpu time observed: 2474ms (out of 2544ms)
          Number of samples taken: 58 (once every 43ms)


        Direct access:
          Total cpu time observed: 1528ms (out of 1636ms)
          Number of samples taken: 43 (once every 36ms)

  * `namespace-set-variable-value!` is slower than `set!`:

        > (timeit (% (racket-set! foo 10)))
        iter: 11,389,643  gc: 0  mem: 1028704

        > (timeit (% (racket-namespace-set-variable-value! (racket-quote foo) 10)))
        iter: 7,369,962  gc: 0  mem: 1024064

        > (timeit (% (racket-namespace-set-variable-value! (racket-quote foo) 10 #f)))
        iter: 7,595,915  gc: 0  mem: 922336

        > (timeit (% (racket-namespace-set-variable-value! (racket-quote foo) 10 #t)))
        iter: 7,078,268  gc: 0  mem: 762368

        > (let name (obj foo 5)
            (timeit (= name!foo 10)))
        iter: 5,567,179  gc: 36  mem: -10775384

  * `namespace-variable-value` is slower than direct access:

        > (timeit (% foo))
        iter: 10,617,404  gc: 0  mem: 1616824

        > (timeit (% (racket-namespace-variable-value (racket-quote foo))))
        iter: 3,954,457  gc: 36  mem: -7896744

        > (timeit (% (racket-namespace-variable-value (racket-quote foo) #f)))
        iter: 7,542,512  gc: 0  mem: 1171400

        ;; TODO: alternative to %compile
        > (timeit (% (racket-namespace-variable-value (racket-quote foo) #f (%compile (fn () nil)))))
        iter: 7,115,895  gc: 0  mem: 1137640

        > (timeit (% (racket-namespace-variable-value (racket-quote foo) #f (racket-lambda () nil))))
        iter: 7,020,827  gc: 0  mem: 1342384

        > (timeit (racket-namespace-variable-value 'foo #f (fn () nil)))
        iter: 7,161,521  gc: 0  mem: 1287848

        > (timeit (ac-var 'foo))
        iter: 6,889,370  gc: 20  mem: -2489272

        > (let name (obj foo 5)
                      (timeit name!foo))
        iter: 6,086,214  gc: 112  mem: -9602608

  * `racket-build-path` is *really fast*:

        > (timeit (racket-string-append "/foo/" "bar/qux/corge/" "nou.jpg"))
        iter: 6,535,348  gc: 136  mem: -12813384

        > (timeit (racket-path->string (racket-build-path "/foo/" "bar/qux/corge/" "nou.jpg")))
        iter: 1,316,847  gc: 56  mem: 2529688

        > (timeit (racket-path->string (apply racket-build-path (map expandpath '("/foo/" "bar/qux/corge/" "nou.jpg")))))
        iter: 249,537  gc: 112  mem: 11936928


        > (timeit (old-joinpath "/foo/" "bar/qux/corge/" "nou.jpg"))
        iter: 129,113  gc: 256  mem: 6253272

        > (timeit (joinpath "/foo/" "bar/qux/corge/" "nou.jpg"))
        iter: 240,388  gc: 124  mem: -11613952

  * `after` is really slow:

        > (timeit (do1 10 20))
        iter: 11,777,563  gc: 0  mem: 984

        > (timeit (after 10 20))
        iter:  5,337,566  gc: 384  mem: 2548048

    For instance, consider the following macro:

        (mac w/ (name value . body)
          (w/uniq u
            `(let ,u ,name
               (assign ,name ,value)
               (after (do ,@body)
                      (assign ,name ,u)))))

    And now let's compare an ordinary variable "foo" with a parameter "bar":

        > (timeit (w/ foo 10 foo))
        iter: 2,615,291  gc: 196  mem: -9667968

        > (timeit (w/bar 10 bar))
        iter: 5,296,878  gc: 148  mem: -11016096

        > (timeit (let bar 10 bar))
        iter: 11,164,302  gc: 0  mem: 824


        > (timeit (w/ foo 10))
        iter: 2,601,117  gc: 168  mem: 1924400

        > (timeit (w/bar 10))
        iter: 6,404,185  gc: 172  mem: -4969160

        > (timeit (let bar 10))
        iter: 10,522,936  gc: 0  mem: 504

    Now let's implement `w/` with `do1`:

        (mac w/ (name value . body)
          (w/uniq u
            `(let ,u ,name
               (assign ,name ,value)
               (do1 (do ,@body)
                    (assign ,name ,u)))))

        > (timeit (w/ foo 10 foo))
        iter: 4,293,050  gc: 212  mem: -2171880

    As the above also demonstrates, parameters are *faster* than global
    variables, but significantly slower than lexical variables.

  * As an expansion on the above, parameters have a high cost vs function
    arguments:

        > (parameter foo)

        > (def bar ()
            (+ foo 10))

        > (timeit (w/foo 25 (bar)))
        iter: 4,403,540  gc: 128  mem: -4611472

        > (def bar (x)
            (+ x 10))

        > (timeit (bar 25))
        iter: 8,485,407  gc: 0  mem: 504

  * Boyer-Moore is faster than Arc's posmatch with small pattern strings:

        > (timeit (posmatch "foo" "barfoo"))
        iter: 423,974  gc: 760  mem: -19140888

        > (timeit (boyer-posmatch "foo" "barfoo"))
        iter: 585,847  gc: 140  mem: -74992

  * Due to preprocessing, the situation is reversed with long pattern strings:

        > (timeit (posmatch "foobarquxcorgenou" "adadadadadadadadadadadadadad"))
        iter: 248,074  gc: 368  mem: 4458200

        > (timeit (boyer-posmatch "foobarquxcorgenou" "adadadadadadadadadadadadadad"))
        iter: 164,709  gc: 112  mem: -10014592

  * But, when precomputing the pattern, Boyer-Moore is *drastically* faster:

        > (let x (boyer-moore-process "foobarquxcorgenou")
            (timeit (boyer-moore-search x "adadadadadadadadadadadadadad")))
        iter: 1,758,602  gc: 124  mem: -5842336


        > (timeit (posmatch "Chrono Cross CrossFire OC ReMix"
                            "lists/Chrono Cross/Remix/Chrono Cross CrossFire OC ReMix.mp3"))
        iter: 69,847  gc: 205  mem: 14991456

        > (timeit (boyer-posmatch "Chrono Cross CrossFire OC ReMix"
                                  "lists/Chrono Cross/Remix/Chrono Cross CrossFire OC ReMix.mp3"))
        iter: 51,026  gc: 116  mem: 11917568

        > (let x (boyer-moore-process "Chrono Cross CrossFire OC ReMix")
            (timeit (boyer-moore-search x "lists/Chrono Cross/Remix/Chrono Cross CrossFire OC ReMix.mp3")))
        iter: 170,348  gc: 124  mem: -16630560


  * Boyer-Moore is also faster for multi-string searches:

        > (timeit (multi-match '("foo" "bar" "qux") '("corge" "foo" "bar" "qux")))
        iter: 56,169  gc: 948  mem: -23564104

        > (timeit (boyer-multi-match '("foo" "bar" "qux") '("corge" "foo" "bar" "qux")))
        iter: 84,598  gc: 136  mem: 4189208

        > (let x (map boyer-moore-process '("foo" "bar" "qux"))
            (timeit (boyer-multi-match1 x '("corge" "foo" "bar" "qux"))))
        iter: 108,929  gc: 124  mem: -104176


        > (timeit (multi-match '("foo" "bar" "qux") '("ufoobarquxcorge")))
        iter: 357,005  gc: 484  mem: -103664

        > (timeit (boyer-multi-match '("foo" "bar" "qux") '("ufoobarquxcorge")))
        iter: 214,963  gc: 128  mem: 16251304

        > (let x (map boyer-moore-process '("foo" "bar" "qux"))
            (timeit (boyer-multi-match1 x '("ufoobarquxcorge"))))
        iter: 471,928  gc: 116  mem: 21216504

  * Inlining function values gives a noticable speed boost:

        (eval `(ac-funcall2 + 1 2))   -> 9211 ms
        (eval `(,ac-funcall2 + 1 2))  -> 5321 ms
        (eval `(,ac-funcall2 ,+ 1 2)) -> 4108 ms

  * `make-keyword-procedure` is about twice as slow as a normal `lambda`:

        > (timeit (% ((racket-make-keyword-procedure (racket-lambda (kw kw-val . rest) rest)) 1 2 3)))
        time: 18.043  gc: 0.856  mem: 2992.04

        > (timeit (% ((racket-lambda rest rest) 1 2 3)))
        time:  9.149  gc: 0.1    mem: -8635.272


        > (timeit (% ((racket-make-keyword-procedure (racket-lambda (kw kw-val a) a)) 1)))
        time: 13.35   gc: 0.38   mem: 645.072

        > (timeit (% ((racket-lambda (a) a) 1)))
        time:  7.957  gc: 0.004  mem: -14541.976

    But it has a much smaller cost when the function is created only once:

        > (let foo (% (racket-make-keyword-procedure (racket-lambda (kw kw-val a) a)))
            (timeit (% (foo 1))))
        time: 9.565  gc: 0.032  mem: -2701.456

        > (let foo (% (racket-lambda (a) a))
            (timeit (% (foo 1))))
        time: 7.99   gc: 0.0    mem: 97.504


  * `keyword-apply` is *very comparable* in speed to a normal `apply`:

        > (let foo (% (racket-lambda rest rest))
            (timeit (% (racket-keyword-apply foo nil nil (racket-list 1 2 3)))))
        time: 12.951 gc: 0.388 mem: -1748.512

        > (let foo (% (racket-lambda rest rest))
            (timeit (% (racket-apply foo (racket-list 1 2 3)))))
        time: 12.692 gc: 0.536 mem: -10373.568


        > (let foo (% (racket-lambda (#:b (b nil) . rest) rest))
            (timeit (% (racket-keyword-apply foo nil nil (racket-list 1 2 3)))))
        time: 15.309 gc: 0.536 mem: -1729.12

        > (let foo (% (racket-lambda (#:b (b nil) . rest) rest))
            (timeit (% (racket-apply foo (racket-list 1 2 3)))))
        time: 14.282 gc: 0.356 mem: -1215.76

  * Keyword functions are almost twice as slow as normal functions:

        > (let foo (fn (:a :b) (list a b))
            (timeit (foo :a 1 :b 2)))
        time: 17.521  gc: 0.408  mem: 2303.568

        > (let foo (fn (a b) (list a b))
            (timeit (foo 1 2)))
        time:  9.515  gc: 0.3    mem: -22736.128


  * Using `%` is slow because it calls `ac-mappend`:

        Special forms with %:          3462ms
        Special forms with ac-compile: 2340ms
