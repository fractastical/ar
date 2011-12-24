==============================================================================

> (+ 1 2)
 arc3 eval: (ar-funcall2 _+ 1 2)
iter: 309,531  gc: 10  mem: 854961  diff: 0%

   ar eval: (#<procedure:ar-funcall2> + 1 2)
iter: 11,519  gc: 27  mem: -293404  diff: -2,587.13%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> +) 1 2)
iter: 20,137  gc: 9  mem: -762849  diff: -1,437.13%

==============================================================================

> (< 1 2)
 arc3 eval: (ar-funcall2 _< 1 2)
iter: 277,055  gc: 17  mem: 779398  diff: -11.72%

   ar eval: (#<procedure:ar-funcall2> < 1 2)
iter: 8,628  gc: 29  mem: -572150  diff: -3,111.11%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> <) 1 2)
iter: 20,171  gc: 8  mem: -641134  diff: -1,273.53%

==============================================================================

> (> 1 2)
 arc3 eval: (ar-funcall2 _> 1 2)
iter: 278,214  gc: 14  mem: -449386  diff: 0.42%

   ar eval: (#<procedure:ar-funcall2> > 1 2)
iter: 8,588  gc: 23  mem: 247766  diff: -3,139.57%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> >) 1 2)
iter: 20,134  gc: 7  mem: 1115257  diff: -1,281.81%

==============================================================================

> (let a 5 a)
 arc3 eval: ((lambda (a) a) 5)
iter: 62,440  gc: 26  mem: -810071  diff: -345.57%

   ar eval: ((racket-lambda (a) a) 5)
iter: 4,149  gc: 25  mem: -978326  diff: -1,404.94%

   nu eval: ((racket-lambda (a) a) 5)
iter: 7,915  gc: 64  mem: 1769487  diff: -688.88%

==============================================================================

> (list 1 2)
 arc3 eval: (ar-funcall2 _list 1 2)
iter: 244,737  gc: 11  mem: 656538  diff: 74.49%

   ar eval: (#<procedure:ar-funcall2> list 1 2)
iter: 7,649  gc: 24  mem: -315224  diff: -3,099.59%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2)
iter: 15,722  gc: 6  mem: 1260715  diff: -1,456.65%

==============================================================================

> (car nil)
 arc3 eval: (ar-funcall1 _car (quote nil))
iter: 250,158  gc: 13  mem: -1392506  diff: 2.17%

   ar eval: (#<procedure:ar-funcall1> car nil)
iter: 8,944  gc: 19  mem: 1127616  diff: -2,696.94%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) (#<procedure:ac-lookup-global-arg> nil))
iter: 10,603  gc: 6  mem: -77753  diff: -2,259.31%

==============================================================================

> (car '())
 arc3 eval: (ar-funcall1 _car (quote nil))
iter: 230,439  gc: 16  mem: -2146296  diff: -8.56%

   ar eval: (#<procedure:ar-funcall1> car ((racket-quote #<procedure>)))
iter: 8,692  gc: 20  mem: 1089145  diff: -2,551.16%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))
iter: 11,024  gc: 9  mem: -1268012  diff: -1,990.34%

==============================================================================

> (let a (list 1 2) (car a))
 arc3 eval: ((lambda (a) (ar-funcall1 _car a)) (ar-funcall2 _list 1 2))
iter: 41,695  gc: 20  mem: 1432173  diff: -452.68%

   ar eval: ((racket-lambda (a) (#<procedure:ar-funcall1> car a)) (#<procedure:ar-funcall2> list 1 2))
iter: 2,322  gc: 21  mem: -1042574  diff: -1,695.65%

   nu eval: ((racket-lambda (a) (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) a)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 4,022  gc: 19  mem: 1340202  diff: -936.67%

==============================================================================

> (let (a b) (list 1 2) a)
 arc3 eval: ((lambda gs478 (let* ((a (ar-xcar (car gs478))) (b (ar-xcar (ar-xcdr (car gs478))))) a)) (ar-funcall2 _list 1 2))
iter: 39,049  gc: 75  mem: -2936170  diff: -6.78%

   ar eval: ((racket-lambda g1335 (racket-let ((g1334 (#<procedure:ar-r/list-toarc> g1335))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () a))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g1334))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g1334))))) (#<procedure:ar-funcall2> list 1 2))
iter: 624  gc: 27  mem: -526229  diff: -6,157.85%

   nu eval: ((racket-lambda (g492) (racket-let* ((a (#<procedure:car> g492)) (g492 (#<procedure:cdr> g492)) (b (#<procedure:car> g492))) a)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 4,304  gc: 31  mem: 1386599  diff: -807.27%

==============================================================================

> (let (a b) (list 1 2) b)
 arc3 eval: ((lambda gs390966 (let* ((a (ar-xcar (car gs390966))) (b (ar-xcar (ar-xcdr (car gs390966))))) b)) (ar-funcall2 _list 1 2))
iter: 40,445  gc: 46  mem: 106662  diff: 3.45%

   ar eval: ((racket-lambda g13811 (racket-let ((g13810 (#<procedure:ar-r/list-toarc> g13811))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () b))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g13810))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g13810))))) (#<procedure:ar-funcall2> list 1 2))
iter: 626  gc: 19  mem: 710546  diff: -6,360.86%

   nu eval: ((racket-lambda (g43559) (racket-let* ((a (#<procedure:car> g43559)) (g43559 (#<procedure:cdr> g43559)) (b (#<procedure:car> g43559))) b)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 4,215  gc: 60  mem: -2483586  diff: -859.55%

==============================================================================

> (let a '(1 2) (car a))
 arc3 eval: ((lambda (a) (ar-funcall1 _car a)) (quote (1 2 . nil)))
iter: 48,205  gc: 21  mem: -37730  diff: 16.1%

   ar eval: ((racket-lambda (a) (#<procedure:ar-funcall1> car a)) ((racket-quote #<procedure>)))
iter: 2,967  gc: 22  mem: -570978  diff: -1,524.71%

   nu eval: ((racket-lambda (a) (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) a)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))
iter: 4,616  gc: 21  mem: 2346093  diff: -944.3%

==============================================================================

> (let (a b) '(1 2) a)
 arc3 eval: ((lambda gs795419 (let* ((a (ar-xcar (car gs795419))) (b (ar-xcar (ar-xcdr (car gs795419))))) a)) (quote (1 2 . nil)))
iter: 46,570  gc: 55  mem: -824896  diff: -3.51%

   ar eval: ((racket-lambda g26327 (racket-let ((g26326 (#<procedure:ar-r/list-toarc> g26327))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () a))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g26326))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g26326))))) ((racket-quote #<procedure>)))
iter: 664  gc: 16  mem: 1427669  diff: -6,913.55%

   nu eval: ((racket-lambda (g85751) (racket-let* ((a (#<procedure:car> g85751)) (g85751 (#<procedure:cdr> g85751)) (b (#<procedure:car> g85751))) a)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))
iter: 4,844  gc: 61  mem: -1650474  diff: -861.4%

==============================================================================

> (let (a b) '(1 2) b)
 arc3 eval: ((lambda gs1261121 (let* ((a (ar-xcar (car gs1261121))) (b (ar-xcar (ar-xcdr (car gs1261121))))) b)) (quote (1 2 . nil)))
iter: 45,895  gc: 74  mem: -760126  diff: -1.47%

   ar eval: ((racket-lambda g39601 (racket-let ((g39600 (#<procedure:ar-r/list-toarc> g39601))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () b))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g39600))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g39600))))) ((racket-quote #<procedure>)))
iter: 660  gc: 19  mem: 1495796  diff: -6,853.79%

   nu eval: ((racket-lambda (g134210) (racket-let* ((a (#<procedure:car> g134210)) (g134210 (#<procedure:cdr> g134210)) (b (#<procedure:car> g134210))) b)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))
iter: 4,999  gc: 32  mem: 40372  diff: -818.08%

==============================================================================

> (idfn 'x)
 arc3 eval: (ar-funcall1 _idfn (quote x))
iter: 226,414  gc: 14  mem: 862603  diff: 79.73%

   ar eval: (#<procedure:ar-funcall1> idfn ((racket-quote #<procedure>)))
iter: 8,376  gc: 14  mem: 1696885  diff: -2,603.13%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> idfn) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))
iter: 10,504  gc: 5  mem: -16883  diff: -2,055.5%

==============================================================================

> (apply idfn '(x))
 arc3 eval: (ar-funcall2 _apply _idfn (quote (x . nil)))
iter: 185,687  gc: 13  mem: 880568  diff: -21.93%

   ar eval: (#<procedure:ar-funcall2> apply idfn ((racket-quote #<procedure>)))
iter: 5,984  gc: 16  mem: -840699  diff: -3,003.06%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> apply) (#<procedure:ac-lookup-global-arg> idfn) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))
iter: 6,924  gc: 4  mem: 1624359  diff: -2,581.79%

==============================================================================

> '(1 2 3 4 5)
 arc3 eval: (quote (1 2 3 4 5 . nil))
iter: 1,194,961  gc: 25  mem: -919166  diff: 84.46%

   ar eval: ((racket-quote #<procedure>))
iter: 27,972  gc: 14  mem: 1554222  diff: -4,171.99%

   nu eval: (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>))
iter: 29,959  gc: 10  mem: -1891574  diff: -3,888.65%

==============================================================================

> (list 1 2 3 4 5)
 arc3 eval: (ar-apply _list (list 1 2 3 4 5))
iter: 227,767  gc: 14  mem: -377682  diff: -424.64%

   ar eval: (#<procedure:ar-apply> list 1 2 3 4 5)
iter: 5,066  gc: 19  mem: 1860247  diff: -4,395.99%

   nu eval: (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> list) 1 2 3 4 5)
iter: 14,982  gc: 6  mem: -684146  diff: -1,420.27%

==============================================================================

> (apply list 1 2 3 4 (list 5))
 arc3 eval: (ar-apply _apply (list _list 1 2 3 4 (ar-funcall1 _list 5)))
iter: 112,897  gc: 13  mem: 79531  diff: -101.75%

   ar eval: (#<procedure:ar-apply> apply list 1 2 3 4 (#<procedure:ar-funcall1> list 5))
iter: 2,960  gc: 53  mem: -5772734  diff: -3,714.09%

   nu eval: (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> apply) (#<procedure:ac-lookup-global-arg> list) 1 2 3 4 (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> list) 5))
iter: 5,757  gc: 7  mem: -133407  diff: -1,861.04%

==============================================================================

> nil
 arc3 eval: (quote nil)
iter: 2,925,240  gc: 17  mem: 728903  diff: 96.14%

   ar eval: nil
iter: 31,893  gc: 20  mem: 701146  diff: -9,072.04%

   nu eval: (#<procedure:ac-lookup-global-arg> nil)
iter: 26,430  gc: 8  mem: -1449584  diff: -10,967.88%

==============================================================================

> (list)
 arc3 eval: (ar-funcall0 _list)
iter: 248,797  gc: 14  mem: 252004  diff: -1,075.75%

   ar eval: (#<procedure:ar-funcall0> list)
iter: 11,874  gc: 27  mem: 225960  diff: -1,995.31%

   nu eval: (#<procedure:ac-funcall0> (#<procedure:ac-lookup-global> list))
iter: 16,403  gc: 9  mem: -479659  diff: -1,416.78%

==============================================================================

> '(foo bar qux)
 arc3 eval: (quote (foo bar qux . nil))
iter: 1,272,903  gc: 26  mem: 689264  diff: 80.45%

   ar eval: ((racket-quote #<procedure>))
iter: 27,130  gc: 24  mem: -1337314  diff: -4,591.87%

   nu eval: (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>))
iter: 29,491  gc: 12  mem: 498936  diff: -4,216.24%

==============================================================================

> `(foo bar qux)
 arc3 eval: (quasiquote (foo bar qux))
iter: 904,793  gc: 63  mem: 239912  diff: -40.68%

   ar eval: (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))))
iter: 2,214  gc: 26  mem: -360486  diff: -40,766.89%

   nu eval: (#<procedure:cons> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)) (#<procedure:cons> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)) (#<procedure:cons> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)) nil)))
iter: 8,131  gc: 10  mem: -404993  diff: -11,027.7%

==============================================================================

> (list 'foo 'bar 'qux)
 arc3 eval: (ar-funcall3 _list (quote foo) (quote bar) (quote qux))
iter: 195,397  gc: 20  mem: 606587  diff: -363.05%

   ar eval: (#<procedure:ar-funcall3> list ((racket-quote #<procedure>)) ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))
iter: 5,196  gc: 26  mem: -1649700  diff: -3,660.53%

   nu eval: (#<procedure:ac-funcall3> (#<procedure:ac-lookup-global> list) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))
iter: 6,055  gc: 9  mem: 434757  diff: -3,127.04%

==============================================================================

> (obj foo 5)
 arc3 eval: (ar-funcall1 _listtab (ar-funcall1 _list (ar-funcall2 _list (quote foo) 5)))
iter: 44,780  gc: 19  mem: -639161  diff: -336.35%

   ar eval: (#<procedure:ar-funcall1> listtab (#<procedure:ar-funcall1> list (#<procedure:ar-funcall2> list ((racket-quote #<procedure>)) 5)))
iter: 2,656  gc: 26  mem: 1083860  diff: -1,585.99%

   nu eval: (#<procedure> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)) 5)
iter: 13,035  gc: 9  mem: 431789  diff: -243.54%

==============================================================================

> (let name (obj foo 5) name!foo)
 arc3 eval: ((lambda (name) (ar-funcall1 name (quote foo))) (ar-funcall1 _listtab (ar-funcall1 _list (ar-funcall2 _list (quote foo) 5))))
iter: 16,807  gc: 23  mem: -1275926  diff: -166.44%

   ar eval: ((racket-lambda (name) (#<procedure:ar-funcall1> name ((racket-quote #<procedure>)))) (#<procedure:ar-funcall1> listtab (#<procedure:ar-funcall1> list (#<procedure:ar-funcall2> list ((racket-quote #<procedure>)) 5))))
iter: 1,148  gc: 58  mem: -796978  diff: -1,364.02%

   nu eval: ((racket-lambda (name) (#<procedure:ac-funcall1> name (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))) (#<procedure> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)) 5))
iter: 1,310  gc: 12  mem: 234302  diff: -1,182.98%

==============================================================================

> (do1 10 20)
 arc3 eval: ((lambda (gs1720075) 20 gs1720075) 10)
iter: 43,079  gc: 59  mem: 4910034  diff: 60.99%

   ar eval: ((racket-lambda (g52812) 20 g52812) 10)
iter: 3,123  gc: 20  mem: 76174  diff: -1,279.41%

   nu eval: ((racket-lambda (g184473) 20 g184473) 10)
iter: 5,153  gc: 21  mem: 3522595  diff: -736%

==============================================================================

> (after 10 20)
 arc3 eval: (ar-funcall2 _protect (lambda () 10) (lambda () 20))
iter: 105,835  gc: 14  mem: -1925328  diff: 59.3%

   ar eval: (#<procedure:ar-funcall2> protect (racket-lambda () 10) (racket-lambda () 20))
iter: 2,868  gc: 18  mem: -530846  diff: -3,590.2%

   nu eval: (#<procedure> (racket-lambda () 10) (racket-lambda () 20))
iter: 15,486  gc: 95  mem: 1079842  diff: -583.42%

==============================================================================

> (rev (list 1 2 3 4 5))
 arc3 eval: (ar-funcall1 _rev (ar-apply _list (list 1 2 3 4 5)))
iter: 125,997  gc: 13  mem: -639957  diff: 16%

   ar eval: (#<procedure:ar-funcall1> rev (#<procedure:ar-apply> list 1 2 3 4 5))
iter: 3,594  gc: 19  mem: -514480  diff: -3,405.76%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> rev) (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> list) 1 2 3 4 5))
iter: 8,068  gc: 7  mem: 312674  diff: -1,461.69%

==============================================================================

> (join '(1 2) '(3 4))
 arc3 eval: (ar-funcall2 _join (quote (1 2 . nil)) (quote (3 4 . nil)))
iter: 204,117  gc: 15  mem: 139482  diff: 38.27%

   ar eval: (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))
iter: 6,473  gc: 19  mem: 822374  diff: -3,053.36%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> join) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1203:25>)))
iter: 7,750  gc: 6  mem: -1613846  diff: -2,533.77%

==============================================================================

> (join (list 1 2) (list 3 4))
 arc3 eval: (ar-funcall2 _join (ar-funcall2 _list 1 2) (ar-funcall2 _list 3 4))
iter: 90,780  gc: 13  mem: 381252  diff: -124.85%

   ar eval: (#<procedure:ar-funcall2> join (#<procedure:ar-funcall2> list 1 2) (#<procedure:ar-funcall2> list 3 4))
iter: 2,875  gc: 16  mem: 1900588  diff: -3,057.57%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> join) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 3 4))
iter: 5,368  gc: 6  mem: -618222  diff: -1,591.13%

==============================================================================

> (rand 50)
 arc3 eval: (ar-funcall1 _rand 50)
iter: 240,271  gc: 12  mem: 212242  diff: 62.22%

   ar eval: (#<procedure:ar-funcall1> rand 50)
iter: 9,425  gc: 20  mem: -971412  diff: -2,449.29%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> rand) 50)
iter: 16,277  gc: 6  mem: -1205119  diff: -1,376.14%

==============================================================================

