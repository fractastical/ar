==============================================================================

> (+ 1 2)
 arc3 eval: (ar-funcall2 _+ 1 2)
iter: 26,150  gc: 0  mem: -6481608  diff: 0%

   ar eval: (#<procedure:ar-funcall2> + 1 2)
iter: 1,050  gc: 4  mem: 4480320  diff: -95.98%

   nu eval: (#<procedure:ac-funcall2> + 1 2)
iter: 2,363  gc: 0  mem: 8359264  diff: -90.96%

==============================================================================

> (< 1 2)
 arc3 eval: (ar-funcall2 _< 1 2)
iter: 23,323  gc: 0  mem: -5604704  diff: -10.81%

   ar eval: (#<procedure:ar-funcall2> < 1 2)
iter: 842  gc: 0  mem: 3913736  diff: -96.39%

   nu eval: (#<procedure:ac-funcall2> < 1 2)
iter: 2,348  gc: 0  mem: 8306184  diff: -89.93%

==============================================================================

> (> 1 2)
 arc3 eval: (ar-funcall2 _> 1 2)
iter: 23,291  gc: 0  mem: -5608568  diff: -0.14%

   ar eval: (#<procedure:ar-funcall2> > 1 2)
iter: 837  gc: 4  mem: 3778408  diff: -96.41%

   nu eval: (#<procedure:ac-funcall2> > 1 2)
iter: 2,364  gc: 0  mem: 8362800  diff: -89.85%

==============================================================================

> (let a 5 a)
 arc3 eval: ((lambda (a) a) 5)
iter: 6,029  gc: 4  mem: 2738376  diff: -74.11%

   ar eval: ((racket-lambda (a) a) 5)
iter: 396  gc: 0  mem: 4997304  diff: -93.43%

   nu eval: ((racket-lambda (a) a) 5)
iter: 1,020  gc: 0  mem: 8402048  diff: -83.08%

==============================================================================

> (list 1 2)
 arc3 eval: (ar-funcall2 _list 1 2)
iter: 21,593  gc: 0  mem: -11137528  diff: 258.15%

   ar eval: (#<procedure:ar-funcall2> list 1 2)
iter: 746  gc: 4  mem: -2060376  diff: -96.55%

   nu eval: (#<procedure:ac-funcall2> list 1 2)
iter: 1,849  gc: 0  mem: 7694000  diff: -91.44%

==============================================================================

> (car nil)
 arc3 eval: (ar-funcall1 _car (quote nil))
iter: 22,085  gc: 0  mem: -10636848  diff: 2.28%

   ar eval: (#<procedure:ar-funcall1> car nil)
iter: 878  gc: 4  mem: -2302320  diff: -96.02%

   nu eval: (#<procedure:ac-funcall1> car (#<procedure:ac-lookup-global-arg> nil))
iter: 1,262  gc: 0  mem: 7334360  diff: -94.29%

==============================================================================

> (car ())
 arc3 eval: (ar-funcall1 _car ())
iter: 22,530  gc: 0  mem: -12981256  diff: 2.01%

   ar eval: (#<procedure:ar-funcall1> car nil)
iter: 879  gc: 4  mem: -3323168  diff: -96.1%

   nu eval: (#<procedure:ac-funcall1> car (racket-quote ()))
iter: 2,051  gc: 0  mem: 8090192  diff: -90.9%

==============================================================================

> (car '())
 arc3 eval: (ar-funcall1 _car (quote nil))
iter: 20,517  gc: 0  mem: -11543280  diff: -8.93%

   ar eval: (#<procedure:ar-funcall1> car ((racket-quote #<procedure>)))
iter: 860  gc: 4  mem: -2363976  diff: -95.81%

   nu eval: (#<procedure:ac-funcall1> car (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1220:25>)))
iter: 1,283  gc: 0  mem: 8633768  diff: -93.75%

==============================================================================

> (let a (list 1 2) (car a))
 arc3 eval: ((lambda (a) (ar-funcall1 _car a)) (ar-funcall2 _list 1 2))
iter: 3,955  gc: 4  mem: -4610880  diff: -80.72%

   ar eval: ((racket-lambda (a) (#<procedure:ar-funcall1> car a)) (#<procedure:ar-funcall2> list 1 2))
iter: 225  gc: 4  mem: -2799504  diff: -94.31%

   nu eval: ((racket-lambda (a) (#<procedure:ac-funcall1> car a)) (#<procedure:ac-funcall2> list 1 2))
iter: 500  gc: 0  mem: 7578456  diff: -87.36%

==============================================================================

> (let (a b) (list 1 2) a)
 arc3 eval: ((lambda gs478 (let* ((a (ar-xcar (car gs478))) (b (ar-xcar (ar-xcdr (car gs478))))) a)) (ar-funcall2 _list 1 2))
iter: 3,894  gc: 4  mem: -6475184  diff: -1.54%

   ar eval: ((racket-lambda g1335 (racket-let ((g1334 (#<procedure:ar-r/list-toarc> g1335))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () a))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g1334))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g1334))))) (#<procedure:ar-funcall2> list 1 2))
iter: 60  gc: 4  mem: 3608416  diff: -98.46%

   nu eval: ((racket-lambda (g481) (racket-let* ((a (#<procedure:car> g481)) (g481 (#<procedure:cdr> g481)) (b (#<procedure:car> g481))) a)) (#<procedure:ac-funcall2> list 1 2))
iter: 516  gc: 0  mem: 7783512  diff: -86.75%

==============================================================================

> (let (a b) (list 1 2) b)
 arc3 eval: ((lambda gs195644 (let* ((a (ar-xcar (car gs195644))) (b (ar-xcar (ar-xcdr (car gs195644))))) b)) (ar-funcall2 _list 1 2))
iter: 3,915  gc: 4  mem: -1506744  diff: 0.54%

   ar eval: ((racket-lambda g7279 (racket-let ((g7278 (#<procedure:ar-r/list-toarc> g7279))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () b))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g7278))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g7278))))) (#<procedure:ar-funcall2> list 1 2))
iter: 60  gc: 0  mem: -2685736  diff: -98.47%

   nu eval: ((racket-lambda (g26228) (racket-let* ((a (#<procedure:car> g26228)) (g26228 (#<procedure:cdr> g26228)) (b (#<procedure:car> g26228))) b)) (#<procedure:ac-funcall2> list 1 2))
iter: 534  gc: 0  mem: 8043720  diff: -86.36%

==============================================================================

> (let a '(1 2) (car a))
 arc3 eval: ((lambda (a) (ar-funcall1 _car a)) (quote (1 2 . nil)))
iter: 4,562  gc: 0  mem: -8042152  diff: 16.53%

   ar eval: ((racket-lambda (a) (#<procedure:ar-funcall1> car a)) ((racket-quote #<procedure>)))
iter: 293  gc: 0  mem: -7283968  diff: -93.58%

   nu eval: ((racket-lambda (a) (#<procedure:ac-funcall1> car a)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1220:25>)))
iter: 570  gc: 0  mem: 8210968  diff: -87.51%

==============================================================================

> (let (a b) '(1 2) a)
 arc3 eval: ((lambda gs391527 (let* ((a (ar-xcar (car gs391527))) (b (ar-xcar (ar-xcdr (car gs391527))))) a)) (quote (1 2 . nil)))
iter: 4,505  gc: 8  mem: -7580488  diff: -1.25%

   ar eval: ((racket-lambda g13315 (racket-let ((g13314 (#<procedure:ar-r/list-toarc> g13315))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () a))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g13314))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g13314))))) ((racket-quote #<procedure>)))
iter: 65  gc: 4  mem: -9378608  diff: -98.56%

   nu eval: ((racket-lambda (g51974) (racket-let* ((a (#<procedure:car> g51974)) (g51974 (#<procedure:cdr> g51974)) (b (#<procedure:car> g51974))) a)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1220:25>)))
