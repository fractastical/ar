==============================================================================

> (+ 1 2)
   arc3 eval: (ar-funcall2 _+ 1 2)
iter: 28,049  gc: 0  mem: -10729400  diff: 0%

     ar eval: (#<procedure:ar-funcall2> + 1 2)
iter: 1,082  gc: 4  mem: -29456  diff: -96.14%

 old nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> +) 1 2)
iter: 1,846  gc: 0  mem: 6559464  diff: -93.42%

 new nu eval: (#<procedure:funcall2> (_+) 1 2)
iter: 32,142  gc: 0  mem: -9376568  diff: 14.59%

==============================================================================

> (< 1 2)
   arc3 eval: (ar-funcall2 _< 1 2)
iter: 24,661  gc: 0  mem: -9956488  diff: 0%

     ar eval: (#<procedure:ar-funcall2> < 1 2)
iter: 853  gc: 4  mem: -1031384  diff: -96.54%

 old nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> <) 1 2)
iter: 1,836  gc: 0  mem: 6523832  diff: -92.56%

 new nu eval: (#<procedure:funcall2> (_<) 1 2)
iter: 31,566  gc: 0  mem: -9563016  diff: 28%

==============================================================================

> (> 1 2)
   arc3 eval: (ar-funcall2 _> 1 2)
iter: 24,422  gc: 0  mem: -10185528  diff: 0%

     ar eval: (#<procedure:ar-funcall2> > 1 2)
iter: 831  gc: 4  mem: -1626728  diff: -96.6%

 old nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> >) 1 2)
iter: 1,801  gc: 0  mem: 6399472  diff: -92.63%

 new nu eval: (#<procedure:funcall2> (_>) 1 2)
iter: 32,117  gc: 0  mem: -9459408  diff: 31.51%

==============================================================================

> '(1 2)
   arc3 eval: (quote (1 2 . nil))
iter: 128,012  gc: 4  mem: -500792  diff: 0%

     ar eval: ((racket-quote #<procedure>))
iter: 2,693  gc: 4  mem: 185792  diff: -97.9%

 old nu eval: (#<procedure:.../01 compiler.arc:1230:25>)
iter: 3,996  gc: 0  mem: 10102984  diff: -96.88%

 new nu eval: (#<procedure:...ite nu/01 nu.rkt:1281:44>)
iter: 42,423  gc: 0  mem: 10651184  diff: -66.86%

==============================================================================

> (list 1 2)
   arc3 eval: (ar-funcall2 _list 1 2)
iter: 23,338  gc: 0  mem: -9992960  diff: 0%

     ar eval: (#<procedure:ar-funcall2> list 1 2)
iter: 755  gc: 4  mem: -1781720  diff: -96.76%

 old nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2)
iter: 1,428  gc: 0  mem: 5942296  diff: -93.88%

 new nu eval: (#<procedure:funcall2> (_list) 1 2)
iter: 28,418  gc: 0  mem: -10459488  diff: 21.77%

==============================================================================

> (car nil)
   arc3 eval: (ar-funcall1 _car (quote nil))
iter: 23,742  gc: 4  mem: -9697488  diff: 0%

     ar eval: (#<procedure:ar-funcall1> car nil)
iter: 900  gc: 4  mem: -1729648  diff: -96.21%

 old nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) (#<procedure:ac-lookup-global-arg> nil))
iter: 971  gc: 0  mem: 5594704  diff: -95.91%

 new nu eval: (#<procedure:funcall1> (_car) (_nil))
iter: 23,291  gc: 0  mem: -9507368  diff: -1.9%

==============================================================================

> (car ())
   arc3 eval: (ar-funcall1 _car ())
iter: 24,134  gc: 0  mem: -10963752  diff: 0%

     ar eval: (#<procedure:ar-funcall1> car nil)
iter: 900  gc: 4  mem: -1729600  diff: -96.27%

 old nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) (racket-quote ()))
iter: 1,606  gc: 0  mem: 6351520  diff: -93.35%

 new nu eval: (#<procedure:funcall1> (_car) (quote ()))
iter: 30,141  gc: 0  mem: -9354432  diff: 24.89%

==============================================================================

> (car '())
   arc3 eval: (ar-funcall1 _car (quote nil))
iter: 22,177  gc: 0  mem: -9320984  diff: 0%

     ar eval: (#<procedure:ar-funcall1> car ((racket-quote #<procedure>)))
iter: 869  gc: 4  mem: -1065880  diff: -96.08%

 old nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 1,156  gc: 0  mem: 6771072  diff: -94.79%

 new nu eval: (#<procedure:funcall1> (_car) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 19,476  gc: 0  mem: -11030568  diff: -12.18%

==============================================================================

> (let a 5 a)
   arc3 eval: ((lambda (a) a) 5)
iter: 6,126  gc: 4  mem: -59896  diff: 0%

     ar eval: ((racket-lambda (a) a) 5)
iter: 408  gc: 4  mem: 2573576  diff: -93.34%

 old nu eval: ((racket-lambda (a) a) 5)
iter: 761  gc: 0  mem: 6275600  diff: -87.58%

 new nu eval: ((lambda (a) a) 5)
iter: 5,328  gc: 0  mem: 13517048  diff: -13.03%

==============================================================================

> (let a (list 1 2) (car a))
   arc3 eval: ((lambda (a) (ar-funcall1 _car a)) (ar-funcall2 _list 1 2))
iter: 4,120  gc: 0  mem: -11048904  diff: 0%

     ar eval: ((racket-lambda (a) (#<procedure:ar-funcall1> car a)) (#<procedure:ar-funcall2> list 1 2))
iter: 219  gc: 4  mem: -10765376  diff: -94.68%

 old nu eval: ((racket-lambda (a) (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) a)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 371  gc: 0  mem: 5641824  diff: -91%

 new nu eval: ((lambda (a) (#<procedure:funcall1> (_car) a)) (#<procedure:funcall2> (_list) 1 2))
iter: 4,176  gc: 0  mem: 14116208  diff: 1.36%

==============================================================================

> (let (a b) (list 1 2) a)
   arc3 eval: ((lambda gs478 (let* ((a (ar-xcar (car gs478))) (b (ar-xcar (ar-xcdr (car gs478))))) a)) (ar-funcall2 _list 1 2))
iter: 3,986  gc: 8  mem: -9275656  diff: 0%

     ar eval: ((racket-lambda g1488 (racket-let ((g1487 (#<procedure:ar-r/list-toarc> g1488))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () a))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g1487))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g1487))))) (#<procedure:ar-funcall2> list 1 2))
iter: 61  gc: 4  mem: -9630928  diff: -98.47%

 old nu eval: ((racket-lambda (g7568) (racket-let* ((a (#<procedure:car> g7568)) (g7568 (#<procedure:cdr> g7568)) (b (#<procedure:car> g7568))) a)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 410  gc: 0  mem: 6096192  diff: -89.71%

 new nu eval: ((lambda (g27576) (let* ((a (#<procedure:car> g27576)) (g27576 (#<procedure:cdr> g27576)) (b (#<procedure:car> g27576))) a)) (#<procedure:funcall2> (_list) 1 2))
iter: 3,848  gc: 0  mem: 12865040  diff: -3.46%

==============================================================================

> (let (a b) (list 1 2) b)
   arc3 eval: ((lambda gs202310 (let* ((a (ar-xcar (car gs202310))) (b (ar-xcar (ar-xcdr (car gs202310))))) b)) (ar-funcall2 _list 1 2))
iter: 3,993  gc: 8  mem: -2967072  diff: 0%

     ar eval: ((racket-lambda g206019 (racket-let ((g206018 (#<procedure:ar-r/list-toarc> g206019))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () b))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g206018))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g206018))))) (#<procedure:ar-funcall2> list 1 2))
iter: 60  gc: 4  mem: -4629024  diff: -98.5%

 old nu eval: ((racket-lambda (g212015) (racket-let* ((a (#<procedure:car> g212015)) (g212015 (#<procedure:cdr> g212015)) (b (#<procedure:car> g212015))) b)) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2))
iter: 406  gc: 0  mem: 6036176  diff: -89.83%

 new nu eval: ((lambda (g231818) (let* ((a (#<procedure:car> g231818)) (g231818 (#<procedure:cdr> g231818)) (b (#<procedure:car> g231818))) b)) (#<procedure:funcall2> (_list) 1 2))
iter: 4,034  gc: 0  mem: 13790568  diff: 1.03%

==============================================================================

> (let a '(1 2) (car a))
   arc3 eval: ((lambda (a) (ar-funcall1 _car a)) (quote (1 2 . nil)))
iter: 4,674  gc: 0  mem: -9563584  diff: 0%

     ar eval: ((racket-lambda (a) (#<procedure:ar-funcall1> car a)) ((racket-quote #<procedure>)))
iter: 284  gc: 4  mem: -10123144  diff: -93.92%

 old nu eval: ((racket-lambda (a) (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> car) a)) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 454  gc: 0  mem: 6144536  diff: -90.29%

 new nu eval: ((lambda (a) (#<procedure:funcall1> (_car) a)) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 4,120  gc: 0  mem: 13945488  diff: -11.85%

==============================================================================

> (let (a b) '(1 2) a)
   arc3 eval: ((lambda gs403284 (let* ((a (ar-xcar (car gs403284))) (b (ar-xcar (ar-xcdr (car gs403284))))) a)) (quote (1 2 . nil)))
iter: 4,474  gc: 8  mem: -7760032  diff: 0%

     ar eval: ((racket-lambda g416144 (racket-let ((g416143 (#<procedure:ar-r/list-toarc> g416144))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () a))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g416143))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g416143))))) ((racket-quote #<procedure>)))
iter: 62  gc: 4  mem: -10412888  diff: -98.61%

 old nu eval: ((racket-lambda (g422422) (racket-let* ((a (#<procedure:car> g422422)) (g422422 (#<procedure:cdr> g422422)) (b (#<procedure:car> g422422))) a)) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 502  gc: 0  mem: 6643640  diff: -88.78%

 new nu eval: ((lambda (g446587) (let* ((a (#<procedure:car> g446587)) (g446587 (#<procedure:cdr> g446587)) (b (#<procedure:car> g446587))) a)) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 4,185  gc: 0  mem: 14192488  diff: -6.46%

==============================================================================

> (let (a b) '(1 2) b)
   arc3 eval: ((lambda gs629095 (let* ((a (ar-xcar (car gs629095))) (b (ar-xcar (ar-xcdr (car gs629095))))) b)) (quote (1 2 . nil)))
iter: 4,538  gc: 8  mem: -7345040  diff: 0%

     ar eval: ((racket-lambda g639048 (racket-let ((g639047 (#<procedure:ar-r/list-toarc> g639048))) ((racket-lambda (a) ((racket-lambda (b) ((racket-lambda () b))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> cdr (#<procedure:ar-funcall1> car g639047))))) (#<procedure:ar-funcall1> car (#<procedure:ar-funcall1> car g639047))))) ((racket-quote #<procedure>)))
iter: 63  gc: 0  mem: -10044656  diff: -98.61%

 old nu eval: ((racket-lambda (g645340) (racket-let* ((a (#<procedure:car> g645340)) (g645340 (#<procedure:cdr> g645340)) (b (#<procedure:car> g645340))) b)) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 501  gc: 0  mem: 6630560  diff: -88.96%

 new nu eval: ((lambda (g669893) (let* ((a (#<procedure:car> g669893)) (g669893 (#<procedure:cdr> g669893)) (b (#<procedure:car> g669893))) b)) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 4,186  gc: 0  mem: 14374120  diff: -7.76%

==============================================================================

> (idfn 'x)
   arc3 eval: (ar-funcall1 _idfn (quote x))
iter: 21,359  gc: 0  mem: -9257960  diff: 0%

     ar eval: (#<procedure:ar-funcall1> idfn ((racket-quote #<procedure>)))
iter: 829  gc: 4  mem: -1084496  diff: -96.12%

 old nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> idfn) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 1,087  gc: 0  mem: 6595656  diff: -94.91%

 new nu eval: (#<procedure:funcall1> (_idfn) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 18,982  gc: 0  mem: -10860128  diff: -11.13%

==============================================================================

> (apply idfn '(x))
   arc3 eval: (ar-funcall2 _apply _idfn (quote (x . nil)))
iter: 17,347  gc: 0  mem: -7525256  diff: 0%

     ar eval: (#<procedure:ar-funcall2> apply idfn ((racket-quote #<procedure>)))
iter: 593  gc: 4  mem: -972808  diff: -96.58%

 old nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> apply) (#<procedure:ac-lookup-global-arg> idfn) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 691  gc: 0  mem: 5806216  diff: -96.02%

 new nu eval: (#<procedure:funcall2> (_apply) (_idfn) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 15,173  gc: 0  mem: -9665080  diff: -12.53%

==============================================================================

> '(1 2 3 4 5)
   arc3 eval: (quote (1 2 3 4 5 . nil))
iter: 108,836  gc: 4  mem: 6045256  diff: 0%

     ar eval: ((racket-quote #<procedure>))
iter: 2,741  gc: 4  mem: 628040  diff: -97.48%

 old nu eval: (#<procedure:.../01 compiler.arc:1230:25>)
iter: 3,968  gc: 0  mem: 10035528  diff: -96.35%

 new nu eval: (#<procedure:...ite nu/01 nu.rkt:1281:44>)
iter: 41,810  gc: 0  mem: 10924088  diff: -61.58%

==============================================================================

> (list 1 2 3 4 5)
   arc3 eval: (ar-apply _list (list 1 2 3 4 5))
iter: 20,821  gc: 4  mem: -8325328  diff: 0%

     ar eval: (#<procedure:ar-apply> list 1 2 3 4 5)
iter: 490  gc: 4  mem: -1611304  diff: -97.65%

 old nu eval: (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> list) 1 2 3 4 5)
iter: 1,370  gc: 0  mem: 5966224  diff: -93.42%

 new nu eval: (#<procedure:ac-apply> (_list) 1 2 3 4 5)
iter: 27,858  gc: 4  mem: -9084568  diff: 33.8%

==============================================================================

> (apply list 1 2 3 4 (list 5))
   arc3 eval: (ar-apply _apply (list _list 1 2 3 4 (ar-funcall1 _list 5)))
iter: 10,665  gc: 4  mem: -8451656  diff: 0%

     ar eval: (#<procedure:ar-apply> apply list 1 2 3 4 (#<procedure:ar-funcall1> list 5))
iter: 294  gc: 4  mem: -2857520  diff: -97.24%

 old nu eval: (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> apply) (#<procedure:ac-lookup-global-arg> list) 1 2 3 4 (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> list) 5))
iter: 526  gc: 0  mem: 5385816  diff: -95.07%

 new nu eval: (#<procedure:ac-apply> (_apply) (_list) 1 2 3 4 (#<procedure:funcall1> (_list) 5))
iter: 12,904  gc: 4  mem: -8623320  diff: 20.99%

==============================================================================

> ()
   arc3 eval: ()
iter: 323,975  gc: 0  mem: 1792  diff: 0%

     ar eval: nil
iter: 3,030  gc: 4  mem: -2833600  diff: -99.06%

 old nu eval: (racket-quote ())
iter: 216,658  gc: 0  mem: -12206880  diff: -33.13%

 new nu eval: (quote ())
iter: 315,624  gc: 4  mem: -5889984  diff: -2.58%

==============================================================================

> nil
   arc3 eval: (quote nil)
iter: 305,309  gc: 4  mem: -6614048  diff: 0%

     ar eval: nil
iter: 2,990  gc: 4  mem: -3141464  diff: -99.02%

 old nu eval: (#<procedure:ac-lookup-global-arg> nil)
iter: 2,383  gc: 0  mem: 4462464  diff: -99.22%

 new nu eval: (_nil)
iter: 71,100  gc: 4  mem: -11281440  diff: -76.71%

==============================================================================

> (list)
   arc3 eval: (ar-funcall0 _list)
iter: 24,311  gc: 4  mem: -11984336  diff: 0%

     ar eval: (#<procedure:ar-funcall0> list)
iter: 1,199  gc: 4  mem: -2748280  diff: -95.07%

 old nu eval: (#<procedure:ac-funcall0> (#<procedure:ac-lookup-global> list))
iter: 1,531  gc: 0  mem: 6174720  diff: -93.7%

 new nu eval: (#<procedure:funcall0> (_list))
iter: 30,238  gc: 4  mem: -12636576  diff: 24.38%

==============================================================================

> '(foo bar qux)
   arc3 eval: (quote (foo bar qux . nil))
iter: 123,739  gc: 4  mem: 3025832  diff: 0%

     ar eval: ((racket-quote #<procedure>))
iter: 2,753  gc: 4  mem: 1796304  diff: -97.78%

 old nu eval: (#<procedure:.../01 compiler.arc:1230:25>)
iter: 4,014  gc: 0  mem: 10152096  diff: -96.76%

 new nu eval: (#<procedure:...ite nu/01 nu.rkt:1281:44>)
iter: 43,375  gc: 0  mem: 11333704  diff: -64.95%

==============================================================================

> `(foo bar qux)
   arc3 eval: (quasiquote (foo bar qux))
iter: 91,230  gc: 4  mem: 1876928  diff: 0%

     ar eval: (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))))
iter: 226  gc: 4  mem: -150152  diff: -99.75%

 old nu eval: (#<procedure:cons> (#<procedure:.../01 compiler.arc:1230:25>) (#<procedure:cons> (#<procedure:.../01 compiler.arc:1230:25>) (#<procedure:cons> (#<procedure:.../01 compiler.arc:1230:25>) (racket-quote ()))))
iter: 1,051  gc: 0  mem: 9435648  diff: -98.85%

 new nu eval: (#<procedure:cons> (#<procedure:...ite nu/01 nu.rkt:1281:44>) (#<procedure:cons> (#<procedure:...ite nu/01 nu.rkt:1281:44>) (#<procedure:cons> (#<procedure:...ite nu/01 nu.rkt:1281:44>) (quote ()))))
iter: 19,819  gc: 4  mem: 1191080  diff: -78.28%

==============================================================================

> (list 'foo 'bar 'qux)
   arc3 eval: (ar-funcall3 _list (quote foo) (quote bar) (quote qux))
iter: 18,125  gc: 0  mem: -5847744  diff: 0%

     ar eval: (#<procedure:ar-funcall3> list ((racket-quote #<procedure>)) ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))
iter: 523  gc: 4  mem: -62544  diff: -97.11%

 old nu eval: (#<procedure:ac-funcall3> (#<procedure:ac-lookup-global> list) (#<procedure:.../01 compiler.arc:1230:25>) (#<procedure:.../01 compiler.arc:1230:25>) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 702  gc: 0  mem: 7113088  diff: -96.13%

 new nu eval: (#<procedure:funcall3> (_list) (#<procedure:...ite nu/01 nu.rkt:1281:44>) (#<procedure:...ite nu/01 nu.rkt:1281:44>) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 10,780  gc: 0  mem: -10710504  diff: -40.52%

==============================================================================

> (obj foo 5)
   arc3 eval: (ar-funcall1 _listtab (ar-funcall1 _list (ar-funcall2 _list (quote foo) 5)))
iter: 4,365  gc: 4  mem: -6090704  diff: 0%

     ar eval: (#<procedure:ar-funcall1> listtab (#<procedure:ar-funcall1> list (#<procedure:ar-funcall2> list ((racket-quote #<procedure>)) 5)))
iter: 268  gc: 4  mem: -866936  diff: -93.86%

 old nu eval: (#<procedure> (#<procedure:.../01 compiler.arc:1230:25>) 5)
iter: 1,486  gc: 0  mem: 7562480  diff: -65.96%

 new nu eval: (#<procedure:funcall1> (_listtab) (#<procedure:funcall1> (_list) (#<procedure:funcall2> (_list) (#<procedure:...ite nu/01 nu.rkt:1281:44>) 5)))
iter: 5,237  gc: 0  mem: -11744144  diff: 19.98%

==============================================================================

> (let name (obj foo 5) name!foo)
   arc3 eval: ((lambda (name) (ar-funcall1 name (quote foo))) (ar-funcall1 _listtab (ar-funcall1 _list (ar-funcall2 _list (quote foo) 5))))
iter: 1,676  gc: 4  mem: -4021656  diff: 0%

     ar eval: ((racket-lambda (name) (#<procedure:ar-funcall1> name ((racket-quote #<procedure>)))) (#<procedure:ar-funcall1> listtab (#<procedure:ar-funcall1> list (#<procedure:ar-funcall2> list ((racket-quote #<procedure>)) 5))))
iter: 117  gc: 4  mem: -2243136  diff: -93.02%

 old nu eval: ((racket-lambda (name) (#<procedure:ac-funcall1> name (#<procedure:.../01 compiler.arc:1230:25>))) (#<procedure> (#<procedure:.../01 compiler.arc:1230:25>) 5))
iter: 133  gc: 0  mem: 4961296  diff: -92.06%

 new nu eval: ((lambda (name) (#<procedure:funcall1> name (#<procedure:...ite nu/01 nu.rkt:1281:44>))) (#<procedure:funcall1> (_listtab) (#<procedure:funcall1> (_list) (#<procedure:funcall2> (_list) (#<procedure:...ite nu/01 nu.rkt:1281:44>) 5))))
iter: 1,675  gc: 0  mem: -10294400  diff: -0.06%

==============================================================================

> (do1 10 20)
   arc3 eval: ((lambda (gs856407) 20 gs856407) 10)
iter: 4,205  gc: 8  mem: -13155984  diff: 0%

     ar eval: ((racket-lambda (g863219) 20 g863219) 10)
iter: 305  gc: 4  mem: -8624696  diff: -92.75%

 old nu eval: ((racket-lambda (g836) 20 g836) 10)
iter: 520  gc: 0  mem: 6035072  diff: -87.63%

 new nu eval: ((lambda (g428) 20 g428) 10)
iter: 4,301  gc: 0  mem: 14558784  diff: 2.28%

==============================================================================

> (after 10 20)
   arc3 eval: (ar-funcall2 _protect (lambda () 10) (lambda () 20))
iter: 9,963  gc: 4  mem: -11696728  diff: 0%

     ar eval: (#<procedure:ar-funcall2> protect (racket-lambda () 10) (racket-lambda () 20))
iter: 288  gc: 4  mem: -8785168  diff: -97.11%

 old nu eval: (#<procedure> (racket-lambda () 10) (racket-lambda () 20))
iter: 1,704  gc: 0  mem: 11671472  diff: -82.9%

 new nu eval: (#<procedure:funcall2> (_protect) (lambda () 10) (lambda () 20))
iter: 4,456  gc: 24  mem: -8999056  diff: -55.27%

==============================================================================

> (rev '(1 2 3 4 5))
   arc3 eval: (ar-funcall1 _rev (quote (1 2 3 4 5 . nil)))
iter: 20,091  gc: 0  mem: -12827664  diff: 0%

     ar eval: (#<procedure:ar-funcall1> rev ((racket-quote #<procedure>)))
iter: 854  gc: 4  mem: -6726536  diff: -95.75%

 old nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> rev) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 1,138  gc: 0  mem: 6665696  diff: -94.34%

 new nu eval: (#<procedure:funcall1> (_rev) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 19,798  gc: 0  mem: 14076000  diff: -1.46%

==============================================================================

> (rev (list 1 2 3 4 5))
   arc3 eval: (ar-funcall1 _rev (ar-apply _list (list 1 2 3 4 5)))
iter: 12,327  gc: 0  mem: -13237784  diff: 0%

     ar eval: (#<procedure:ar-funcall1> rev (#<procedure:ar-apply> list 1 2 3 4 5))
iter: 354  gc: 0  mem: -6905240  diff: -97.13%

 old nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> rev) (#<procedure:...t/private/kw.rkt:188:14> (#<procedure:ac-lookup-global> list) 1 2 3 4 5))
iter: 757  gc: 0  mem: 5816200  diff: -93.86%

 new nu eval: (#<procedure:funcall1> (_rev) (#<procedure:ac-apply> (_list) 1 2 3 4 5))
iter: 16,055  gc: 0  mem: -13194464  diff: 30.24%

==============================================================================

> (join '(1 2) '(3 4))
   arc3 eval: (ar-funcall2 _join (quote (1 2 . nil)) (quote (3 4 . nil)))
iter: 19,041  gc: 0  mem: -10713296  diff: 0%

     ar eval: (#<procedure:ar-funcall2> join ((racket-quote #<procedure>)) ((racket-quote #<procedure>)))
iter: 645  gc: 4  mem: -5561136  diff: -96.61%

 old nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> join) (#<procedure:.../01 compiler.arc:1230:25>) (#<procedure:.../01 compiler.arc:1230:25>))
iter: 860  gc: 0  mem: 6964392  diff: -95.48%

 new nu eval: (#<procedure:funcall2> (_join) (#<procedure:...ite nu/01 nu.rkt:1281:44>) (#<procedure:...ite nu/01 nu.rkt:1281:44>))
iter: 13,691  gc: 0  mem: 14190624  diff: -28.1%

==============================================================================

> (join (list 1 2) (list 3 4))
   arc3 eval: (ar-funcall2 _join (ar-funcall2 _list 1 2) (ar-funcall2 _list 3 4))
iter: 8,905  gc: 0  mem: -14136752  diff: 0%

     ar eval: (#<procedure:ar-funcall2> join (#<procedure:ar-funcall2> list 1 2) (#<procedure:ar-funcall2> list 3 4))
iter: 286  gc: 4  mem: -8109184  diff: -96.79%

 old nu eval: (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> join) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 1 2) (#<procedure:ac-funcall2> (#<procedure:ac-lookup-global> list) 3 4))
iter: 497  gc: 0  mem: 5647640  diff: -94.42%

 new nu eval: (#<procedure:funcall2> (_join) (#<procedure:funcall2> (_list) 1 2) (#<procedure:funcall2> (_list) 3 4))
iter: 11,067  gc: 0  mem: -14422952  diff: 24.28%

==============================================================================

> (rand 50)
   arc3 eval: (ar-funcall1 _rand 50)
iter: 22,911  gc: 0  mem: 14006760  diff: 0%

     ar eval: (#<procedure:ar-funcall1> rand 50)
iter: 896  gc: 4  mem: -8818632  diff: -96.09%

 old nu eval: (#<procedure:ac-funcall1> (#<procedure:ac-lookup-global> rand) 50)
iter: 1,500  gc: 0  mem: 6136960  diff: -93.45%

 new nu eval: (#<procedure:funcall1> (_rand) 50)
iter: 30,326  gc: 0  mem: 14571752  diff: 32.36%

==============================================================================

