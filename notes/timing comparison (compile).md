==============================================================================

> (+ 1 2)
 arc3 eval: (ar-funcall2 _+ 1 2)
iter: 235,335  gc: 12  mem: -213224  diff: 0%

   ar eval: (#<procedure:ar-funcall2> + 1 2)
iter: 11,070  gc: 27  mem: -862933  diff: -2,025.88%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> +) 1 2)
iter: 15,907  gc: 14  mem: -18904  diff: -1,379.44%

==============================================================================

> (< 1 2)
 arc3 eval: (ar-funcall2 _< 1 2)
iter: 212,764  gc: 11  mem: 109053  diff: -10.61%

   ar eval: (#<procedure:ar-funcall2> < 1 2)
iter: 8,886  gc: 22  mem: 2638542  diff: -2,294.37%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> <) 1 2)
iter: 15,924  gc: 9  mem: -1804546  diff: -1,236.12%

==============================================================================

> (> 1 2)
 arc3 eval: (ar-funcall2 _> 1 2)
iter: 214,143  gc: 11  mem: 892413  diff: 0.64%

   ar eval: (#<procedure:ar-funcall2> > 1 2)
iter: 9,004  gc: 24  mem: 178998  diff: -2,278.31%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> >) 1 2)
iter: 15,914  gc: 10  mem: 19880  diff: -1,245.63%

==============================================================================

> (let a 5 a)
 arc3 eval: ((lambda (a) a) 5)
iter: 60,163  gc: 19  mem: -1424076  diff: -255.94%

   ar eval: ((racket-lambda (a) a) 5)
iter: 4,245  gc: 26  mem: 1056882  diff: -1,317.27%

   nu eval: ((racket-lambda (a) a) 5)
iter: 6,553  gc: 60  mem: 843566  diff: -818.1%

==============================================================================

> (list 1 2)
 arc3 eval: (ar-funcall2 _list 1 2)
iter: 195,520  gc: 12  mem: -928680  diff: 69.23%

   ar eval: (#<procedure:ar-funcall2> list 1 2)
iter: 7,894  gc: 22  mem: 24914  diff: -2,376.82%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2)
iter: 12,446  gc: 10  mem: 208706  diff: -1,470.95%

==============================================================================

> (car nil)
 arc3 eval: (ar-funcall1 _car (quote nil))
iter: 199,548  gc: 11  mem: -544564  diff: 2.02%

   ar eval: (#<procedure:ar-funcall1> car nil)
iter: 9,219  gc: 24  mem: -401  diff: -2,064.53%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) (#<procedure:ac-lookup-global-arg> nil))
iter: 8,505  gc: 10  mem: -294625  diff: -2,246.24%

==============================================================================

> (car '())
 arc3 eval: (ar-funcall1 _car (quote nil))
iter: 188,777  gc: 14  mem: 365653  diff: -5.71%

   ar eval: (#<procedure:ar-funcall1> car ((racket-quote #<procedure>)))
iter: 8,977  gc: 22  mem: -223778  diff: -2,002.9%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))
iter: 8,705  gc: 11  mem: -1122359  diff: -2,068.6%

==============================================================================

> (let a (list 1 2) (car a))
 arc3 eval: ((lambda (a) (ar-funcall1 _car a)) (ar-funcall2 _list 1 2))
iter: 40,048  gc: 20  mem: -429657  diff: -371.38%

   ar eval: ((racket-lambda (a) (#<procedure:ar-funcall1> car a)) (#<procedure:ar-funcall2> list 1 2))
iter: 2,393  gc: 18  mem: 2130495  diff: -1,573.55%

   nu eval: ((racket-lambda (a) (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) a)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 3,340  gc: 19  mem: 1427394  diff: -1,099.04%

==============================================================================

> (let (a b) (list 1 2) a)
 arc3 eval: ((lambda gs478 (let* ((a (ar-xcar (car gs478))) (b (ar-xcar (ar-xcdr (car gs478))))) a)) (ar-funcall2 _list 1 2))
iter: 38,974  gc: 45  mem: 193291  diff: -2.76%

   ar eval: ((racket-lambda g1326 (racket-let ((g1325 (#<procedure:ar-r/list-toarc> g1326))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () a))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g1325))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g1325))))) (#<procedure:ar-funcall2> list 1 2))
iter: 616  gc: 51  mem: -1864399  diff: -6,226.95%

   nu eval: ((racket-lambda (g497) (racket-let* ((a (#<procedure:car> g497)) (g497 (#<procedure:cdr> g497)) (b (#<procedure:car> g497))) a)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 3,647  gc: 27  mem: 1083294  diff: -968.66%

==============================================================================

> (let (a b) (list 1 2) b)
 arc3 eval: ((lambda gs390222 (let* ((a (ar-xcar (car gs390222))) (b (ar-xcar (ar-xcdr (car gs390222))))) b)) (ar-funcall2 _list 1 2))
iter: 38,876  gc: 46  mem: 1182333  diff: -0.25%

   ar eval: ((racket-lambda g13638 (racket-let ((g13637 (#<procedure:ar-r/list-toarc> g13638))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () b))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g13637))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g13637))))) (#<procedure:ar-funcall2> list 1 2))
iter: 640  gc: 21  mem: -799350  diff: -5,974.38%

   nu eval: ((racket-lambda (g36992) (racket-let* ((a (#<procedure:car> g36992)) (g36992 (#<procedure:cdr> g36992)) (b (#<procedure:car> g36992))) b)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 3,632  gc: 26  mem: 2791228  diff: -970.37%

==============================================================================

> (let a '(1 2) (car a))
 arc3 eval: ((lambda (a) (ar-funcall1 _car a)) (quote (1 2 . nil)))
iter: 46,488  gc: 20  mem: 389182  diff: 16.37%

   ar eval: ((racket-lambda (a) (#<procedure:ar-funcall1> car a)) ((racket-quote #<procedure>)))
iter: 3,082  gc: 19  mem: -732609  diff: -1,408.37%

   nu eval: ((racket-lambda (a) (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) a)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))
iter: 3,676  gc: 54  mem: -4296606  diff: -1,164.64%

==============================================================================

> (let (a b) '(1 2) a)
 arc3 eval: ((lambda gs778986 (let* ((a (ar-xcar (car gs778986))) (b (ar-xcar (ar-xcdr (car gs778986))))) a)) (quote (1 2 . nil)))
iter: 44,882  gc: 56  mem: 120110  diff: -3.58%

   ar eval: ((racket-lambda g26440 (racket-let ((g26439 (#<procedure:ar-r/list-toarc> g26440))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () a))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g26439))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g26439))))) ((racket-quote #<procedure>)))
iter: 675  gc: 20  mem: 1967655  diff: -6,549.19%

   nu eval: ((racket-lambda (g73358) (racket-let* ((a (#<procedure:car> g73358)) (g73358 (#<procedure:cdr> g73358)) (b (#<procedure:car> g73358))) a)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))
iter: 4,215  gc: 28  mem: 213735  diff: -964.82%

==============================================================================

> (let (a b) '(1 2) b)
 arc3 eval: ((lambda gs1227811 (let* ((a (ar-xcar (car gs1227811))) (b (ar-xcar (ar-xcdr (car gs1227811))))) b)) (quote (1 2 . nil)))
iter: 42,158  gc: 104  mem: -3829150  diff: -6.46%

   ar eval: ((racket-lambda g39946 (racket-let ((g39945 (#<procedure:ar-r/list-toarc> g39946))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () b))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g39945))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g39945))))) ((racket-quote #<procedure>)))
iter: 671  gc: 17  mem: -123152  diff: -6,182.86%

   nu eval: ((racket-lambda (g115528) (racket-let* ((a (#<procedure:car> g115528)) (g115528 (#<procedure:cdr> g115528)) (b (#<procedure:car> g115528))) b)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))
iter: 4,169  gc: 30  mem: 1494758  diff: -911.23%

==============================================================================

> (idfn 'x)
 arc3 eval: (ar-funcall1 _idfn (quote x))
iter: 182,460  gc: 8  mem: 833663  diff: 76.89%

   ar eval: (#<procedure:ar-funcall1> idfn ((racket-quote #<procedure>)))
iter: 8,593  gc: 18  mem: 34207  diff: -2,023.36%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> idfn) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))
iter: 8,200  gc: 9  mem: -6348  diff: -2,125.12%

==============================================================================

> (apply idfn '(x))
 arc3 eval: (ar-funcall2 _apply _idfn (quote (x . nil)))
iter: 152,940  gc: 12  mem: -115505  diff: -19.3%

   ar eval: (#<procedure:ar-funcall2> apply idfn ((racket-quote #<procedure>)))
iter: 6,064  gc: 19  mem: 720738  diff: -2,422.1%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> apply) (#<procedure:ac-lookup-global-arg> idfn) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))
iter: 5,489  gc: 8  mem: -2519824  diff: -2,686.3%

==============================================================================

> '(1 2 3 4 5)
 arc3 eval: (quote (1 2 3 4 5 . nil))
iter: 732,671  gc: 17  mem: -49587  diff: 79.13%

   ar eval: ((racket-quote #<procedure>))
iter: 27,941  gc: 16  mem: 1684009  diff: -2,522.21%

   nu eval: (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>))
iter: 22,705  gc: 10  mem: 584838  diff: -3,126.91%

==============================================================================

> (list 1 2 3 4 5)
 arc3 eval: (ar-apply _list (list 1 2 3 4 5))
iter: 183,117  gc: 10  mem: -1668314  diff: -300.11%

   ar eval: (#<procedure:ar-apply> list 1 2 3 4 5)
iter: 5,205  gc: 19  mem: 599954  diff: -3,418.1%

   nu eval: (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> list) 1 2 3 4 5)
iter: 11,843  gc: 9  mem: -604833  diff: -1,446.2%

==============================================================================

> (apply list 1 2 3 4 (list 5))
 arc3 eval: (ar-apply _apply (list _list 1 2 3 4 (ar-funcall1 _list 5)))
iter: 94,460  gc: 46  mem: -3368638  diff: -93.86%

   ar eval: (#<procedure:ar-apply> apply list 1 2 3 4 (#<procedure:ar-funcall1> list 5))
iter: 3,116  gc: 20  mem: 247825  diff: -2,931.45%

   nu eval: (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> apply) (#<procedure:ac-lookup-global-arg> list) 1 2 3 4 (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> list) 5))
iter: 4,573  gc: 9  mem: 592742  diff: -1,965.6%

==============================================================================

> nil
 arc3 eval: (quote nil)
iter: 1,533,277  gc: 9  mem: 177326  diff: 93.84%

   ar eval: nil
iter: 30,493  gc: 23  mem: -659724  diff: -4,928.29%

   nu eval: (#<procedure:ac-lookup-global-arg> nil)
iter: 21,510  gc: 8  mem: -266200  diff: -7,028.21%

==============================================================================

> (list)
 arc3 eval: (ar-funcall0 _list)
iter: 205,976  gc: 11  mem: -267295  diff: -644.4%

   ar eval: (#<procedure:ar-funcall0> list)
iter: 12,344  gc: 23  mem: -16498  diff: -1,568.63%

   nu eval: (#<procedure:ac-funcall0> (#<procedure:ac-lookup-global> list))
iter: 13,101  gc: 7  mem: -430223  diff: -1,472.22%

==============================================================================

> '(foo bar qux)
 arc3 eval: (quote (foo bar qux . nil))
iter: 826,142  gc: 18  mem: 452906  diff: 75.07%

   ar eval: ((racket-quote #<procedure>))
iter: 27,747  gc: 24  mem: -1187186  diff: -2,877.41%

   nu eval: (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>))
iter: 22,522  gc: 15  mem: 380805  diff: -3,568.16%

==============================================================================

> `(foo bar qux)
 arc3 eval: (quasiquote (foo bar qux))
iter: 637,577  gc: 49  mem: 1016468  diff: -29.58%

   ar eval: (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))))
iter: 2,333  gc: 20  mem: -324572  diff: -27,228.63%

   nu eval: (#<procedure:cons> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)) (#<procedure:cons> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)) (#<procedure:cons> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)) nil)))
iter: 6,414  gc: 13  mem: -1336005  diff: -9,840.4%

==============================================================================

> (list 'foo 'bar 'qux)
 arc3 eval: (ar-funcall3 _list (quote foo) (quote bar) (quote qux))
iter: 161,328  gc: 14  mem: 163287  diff: -295.21%

   ar eval: (#<procedure:ar-funcall3> list ((racket-quote #<procedure>)) ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))
iter: 5,416  gc: 25  mem: -518997  diff: -2,878.73%

   nu eval: (#<procedure:ac-funcall3> (#<procedure:ac-lookup-global> list) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))
iter: 4,785  gc: 14  mem: 444206  diff: -3,271.54%

==============================================================================

> (obj foo 5)
 arc3 eval: (ar-funcall1 _listtab (ar-funcall1 _list (ar-funcall2 _list (quote foo) 5)))
iter: 41,909  gc: 13  mem: 1389475  diff: -284.95%

   ar eval: (#<procedure:ar-funcall1> listtab (#<procedure:ar-funcall1> list (#<procedure:ar-funcall2> list ((racket-quote #<procedure>)) 5)))
iter: 2,655  gc: 20  mem: -334182  diff: -1,478.49%

   nu eval: (#<procedure> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)) 5)
iter: 10,742  gc: 8  mem: 564042  diff: -290.14%

==============================================================================

> (let name (obj foo 5) name!foo)
 arc3 eval: ((lambda (name) (ar-funcall1 name (quote foo))) (ar-funcall1 _listtab (ar-funcall1 _list (ar-funcall2 _list (quote foo) 5))))
iter: 16,488  gc: 15  mem: 784382  diff: -154.18%

   ar eval: ((racket-lambda (name) (#<procedure:ar-funcall1> name ((racket-quote #<procedure>)))) (#<procedure:ar-funcall1> listtab (#<procedure:ar-funcall1> list (#<procedure:ar-funcall2> list ((racket-quote #<procedure>)) 5))))
iter: 1,118  gc: 56  mem: -1895093  diff: -1,374.78%

   nu eval: ((racket-lambda (name) (#<procedure:ac-funcall1> name (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))) (#<procedure> (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)) 5))
iter: 1,125  gc: 14  mem: 76036  diff: -1,365.6%

==============================================================================

> (do1 10 20)
 arc3 eval: ((lambda (gs1649390) 20 gs1649390) 10)
iter: 41,925  gc: 53  mem: 4453294  diff: 60.67%

   ar eval: ((racket-lambda (g53363) 20 g53363) 10)
iter: 3,210  gc: 23  mem: -167960  diff: -1,206.07%

   nu eval: ((racket-lambda (g157493) 20 g157493) 10)
iter: 4,214  gc: 22  mem: 1442866  diff: -894.9%

==============================================================================

> (after 10 20)
 arc3 eval: (ar-funcall2 _protect (lambda () 10) (lambda () 20))
iter: 93,140  gc: 13  mem: -1065856  diff: 54.99%

   ar eval: (#<procedure:ar-funcall2> protect (racket-lambda () 10) (racket-lambda () 20))
iter: 2,967  gc: 18  mem: -1134138  diff: -3,039.2%

   nu eval: (#<procedure> (racket-lambda () 10) (racket-lambda () 20))
iter: 12,586  gc: 88  mem: 982020  diff: -640.03%

==============================================================================

> (rev (list 1 2 3 4 5))
 arc3 eval: (ar-funcall1 _rev (ar-apply _list (list 1 2 3 4 5)))
iter: 110,823  gc: 13  mem: -1630562  diff: 15.96%

   ar eval: (#<procedure:ar-funcall1> rev (#<procedure:ar-apply> list 1 2 3 4 5))
iter: 3,724  gc: 20  mem: 599590  diff: -2,875.91%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> rev) (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> list) 1 2 3 4 5))
iter: 6,501  gc: 10  mem: 297679  diff: -1,604.71%

==============================================================================

> (join '(1 2) '(3 4))
 arc3 eval: (ar-funcall2 _join (quote (1 2 . nil)) (quote (3 4 . nil)))
iter: 170,167  gc: 12  mem: -336219  diff: 34.87%

   ar eval: (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))
iter: 6,672  gc: 21  mem: 332420  diff: -2,450.46%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> join) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)) (#<procedure:ac-quote> (#<procedure:.../01 compiler.arc:1182:25>)))
iter: 6,004  gc: 9  mem: -1641961  diff: -2,734.23%

==============================================================================

> (join (list 1 2) (list 3 4))
 arc3 eval: (ar-funcall2 _join (ar-funcall2 _list 1 2) (ar-funcall2 _list 3 4))
iter: 80,336  gc: 14  mem: 203609  diff: -111.82%

   ar eval: (#<procedure:ar-funcall2> join (#<procedure:ar-funcall2> list 1 2) (#<procedure:ar-funcall2> list 3 4))
iter: 3,004  gc: 21  mem: 1065928  diff: -2,574.3%

   nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> join) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 3 4))
iter: 4,214  gc: 9  mem: -1226131  diff: -1,806.41%

==============================================================================

> (rand 50)
 arc3 eval: (ar-funcall1 _rand 50)
iter: 190,598  gc: 10  mem: -826814  diff: 57.85%

   ar eval: (#<procedure:ar-funcall1> rand 50)
iter: 9,520  gc: 18  mem: 1435253  diff: -1,902.08%

   nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> rand) 50)
iter: 12,660  gc: 9  mem: 301718  diff: -1,405.51%

==============================================================================

