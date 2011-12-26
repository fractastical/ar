==============================================================================

> (+ 1 2)
   arc3 eval: 3
iter: 1,783  gc: 0  mem: -8445400  diff: 0%

     ar eval: 3
iter: 717  gc: 0  mem: -3536496  diff: -59.79%

 old nu eval: 3
iter: 920  gc: 0  mem: 11152912  diff: -48.4%

 new nu eval: 3
iter: 1,881  gc: 0  mem: -8786696  diff: 5.5%

==============================================================================

> (< 1 2)
   arc3 eval: t
iter: 1,761  gc: 0  mem: -8473040  diff: 0%

     ar eval: t
iter: 608  gc: 4  mem: -3744736  diff: -65.47%

 old nu eval: t
iter: 907  gc: 0  mem: 10994128  diff: -48.5%

 new nu eval: t
iter: 1,856  gc: 0  mem: -8995632  diff: 5.39%

==============================================================================

> (> 1 2)
   arc3 eval: nil
iter: 1,742  gc: 0  mem: -9688416  diff: 0%

     ar eval: nil
iter: 593  gc: 0  mem: -5294664  diff: -65.96%

 old nu eval: ()
iter: 904  gc: 0  mem: 10959288  diff: -48.11%

 new nu eval: ()
iter: 1,861  gc: 0  mem: -9998816  diff: 6.83%

==============================================================================

> '(1 2)
   arc3 eval: (1 2 . nil)
iter: 7,271  gc: 0  mem: -4157624  diff: 0%

     ar eval: {1 2 . nil}
iter: 1,373  gc: 0  mem: -5069144  diff: -81.12%

 old nu eval: (1 2)
iter: 1,831  gc: 0  mem: -12634600  diff: -74.82%

 new nu eval: (1 2)
iter: 3,494  gc: 0  mem: -10429088  diff: -51.95%

==============================================================================

> (list 1 2)
   arc3 eval: (1 2 . nil)
iter: 1,700  gc: 0  mem: -9492072  diff: 0%

     ar eval: {1 2 . nil}
iter: 549  gc: 4  mem: -4588544  diff: -67.71%

 old nu eval: (1 2)
iter: 822  gc: 0  mem: 10518128  diff: -51.65%

 new nu eval: (1 2)
iter: 1,867  gc: 0  mem: -9697720  diff: 9.82%

==============================================================================

> (car nil)
   arc3 eval: nil
iter: 3,246  gc: 0  mem: -10353352  diff: 0%

     ar eval: nil
iter: 622  gc: 4  mem: -5247720  diff: -80.84%

 old nu eval: ()
iter: 584  gc: 0  mem: 9622144  diff: -82.01%

 new nu eval: ()
iter: 1,455  gc: 0  mem: -11681352  diff: -55.18%

==============================================================================

> (car ())
   arc3 eval: nil
iter: 3,231  gc: 0  mem: -11470216  diff: 0%

     ar eval: nil
iter: 633  gc: 4  mem: -5913960  diff: -80.41%

 old nu eval: ()
iter: 872  gc: 0  mem: 11045928  diff: -73.01%

 new nu eval: ()
iter: 1,881  gc: 0  mem: -10510360  diff: -41.78%

==============================================================================

> (car '())
   arc3 eval: nil
iter: 3,162  gc: 0  mem: -11382384  diff: 0%

     ar eval: nil
iter: 563  gc: 4  mem: -5797800  diff: -82.19%

 old nu eval: ()
iter: 667  gc: 0  mem: 10977728  diff: -78.91%

 new nu eval: ()
iter: 1,498  gc: 4  mem: -7128144  diff: -52.62%

==============================================================================

> (let a 5 a)
   arc3 eval: 5
iter: 1,629  gc: 0  mem: -2529816  diff: 0%

     ar eval: 5
iter: 294  gc: 4  mem: -1133528  diff: -81.95%

 old nu eval: 5
iter: 489  gc: 0  mem: 9294448  diff: -69.98%

 new nu eval: 5
iter: 1,184  gc: 8  mem: -10606088  diff: -27.32%

==============================================================================

> (let a (list 1 2) (car a))
   arc3 eval: 1
iter: 848  gc: 0  mem: -15353216  diff: 0%

     ar eval: 1
iter: 148  gc: 4  mem: -14326096  diff: -82.55%

 old nu eval: 1
iter: 212  gc: 0  mem: 8459568  diff: -75%

 new nu eval: 1
iter: 532  gc: 0  mem: 13545984  diff: -37.26%

==============================================================================

> (let (a b) (list 1 2) a)
   arc3 eval: 1
iter: 535  gc: 0  mem: 16680088  diff: 0%

     ar eval: 1
iter: 43  gc: 4  mem: -13929640  diff: -91.96%

 old nu eval: 1
iter: 180  gc: 0  mem: 9165552  diff: -66.36%

 new nu eval: 1
iter: 395  gc: 4  mem: -8492208  diff: -26.17%

==============================================================================

> (let (a b) (list 1 2) b)
   arc3 eval: 2
iter: 505  gc: 4  mem: -11572576  diff: 0%

     ar eval: 2
iter: 43  gc: 4  mem: -7588224  diff: -91.49%

 old nu eval: 2
iter: 178  gc: 0  mem: 9094304  diff: -64.75%

 new nu eval: 2
iter: 406  gc: 0  mem: 15457504  diff: -19.6%

==============================================================================

> (let a '(1 2) (car a))
   arc3 eval: 1
iter: 1,320  gc: 0  mem: -14195720  diff: 0%

     ar eval: 1
iter: 178  gc: 4  mem: -15724240  diff: -86.52%

 old nu eval: 1
iter: 246  gc: 0  mem: 8373296  diff: -81.36%

 new nu eval: 1
iter: 622  gc: 0  mem: 13301800  diff: -52.88%

==============================================================================

> (let (a b) '(1 2) a)
   arc3 eval: 1
iter: 677  gc: 4  mem: -16211528  diff: 0%

     ar eval: 1
iter: 45  gc: 4  mem: -13984632  diff: -93.35%

 old nu eval: 1
iter: 216  gc: 0  mem: 9654048  diff: -68.09%

 new nu eval: 1
iter: 450  gc: 4  mem: -9610032  diff: -33.53%

==============================================================================

> (let (a b) '(1 2) b)
   arc3 eval: 2
iter: 681  gc: 4  mem: -11909672  diff: 0%

     ar eval: 2
iter: 45  gc: 4  mem: -9733480  diff: -93.39%

 old nu eval: 2
iter: 215  gc: 0  mem: 9643728  diff: -68.43%

 new nu eval: 2
iter: 470  gc: 0  mem: 15963832  diff: -30.98%

==============================================================================

> (idfn 'x)
   arc3 eval: x
iter: 3,224  gc: 0  mem: 15220368  diff: 0%

     ar eval: x
iter: 547  gc: 0  mem: -13005376  diff: -83.03%

 old nu eval: x
iter: 648  gc: 0  mem: 10799984  diff: -79.9%

 new nu eval: x
iter: 1,500  gc: 0  mem: 15018272  diff: -53.47%

==============================================================================

> (apply idfn '(x))
   arc3 eval: x
iter: 2,782  gc: 0  mem: 15627600  diff: 0%

     ar eval: x
iter: 417  gc: 0  mem: -12740800  diff: -85.01%

 old nu eval: x
iter: 447  gc: 0  mem: 9896528  diff: -83.93%

 new nu eval: x
iter: 1,161  gc: 0  mem: 14927832  diff: -58.27%

==============================================================================

> '(1 2 3 4 5)
   arc3 eval: (1 2 3 4 5 . nil)
iter: 6,685  gc: 4  mem: -10389768  diff: 0%

     ar eval: {1 2 3 4 5 . nil}
iter: 1,353  gc: 0  mem: -13720496  diff: -79.76%

 old nu eval: (1 2 3 4 5)
iter: 1,896  gc: 0  mem: 12969848  diff: -71.64%

 new nu eval: (1 2 3 4 5)
iter: 3,597  gc: 0  mem: 15167200  diff: -46.19%

==============================================================================

> (list 1 2 3 4 5)
   arc3 eval: (1 2 3 4 5 . nil)
iter: 911  gc: 0  mem: -16993608  diff: 0%

     ar eval: {1 2 3 4 5 . nil}
iter: 371  gc: 0  mem: -12478104  diff: -59.28%

 old nu eval: (1 2 3 4 5)
iter: 759  gc: 0  mem: 11429032  diff: -16.68%

 new nu eval: (1 2 3 4 5)
iter: 1,664  gc: 0  mem: -16569752  diff: 82.66%

==============================================================================

> (apply list 1 2 3 4 (list 5))
   arc3 eval: (1 2 3 4 5 . nil)
iter: 766  gc: 0  mem: 15192624  diff: 0%

     ar eval: {1 2 3 4 5 . nil}
iter: 229  gc: 4  mem: -12256536  diff: -70.1%

 old nu eval: (1 2 3 4 5)
iter: 333  gc: 0  mem: 10407256  diff: -56.53%

 new nu eval: (1 2 3 4 5)
iter: 826  gc: 0  mem: -17019944  diff: 7.83%

==============================================================================

> ()
   arc3 eval: nil
iter: 8,970  gc: 0  mem: -12806616  diff: 0%

     ar eval: nil
iter: 1,875  gc: 4  mem: -13730032  diff: -79.1%

 old nu eval: ()
iter: 6,835  gc: 0  mem: -12986272  diff: -23.8%

 new nu eval: ()
iter: 8,771  gc: 4  mem: -13268480  diff: -2.22%

==============================================================================

> nil
   arc3 eval: nil
iter: 8,928  gc: 4  mem: -12907584  diff: 0%

     ar eval: nil
iter: 1,882  gc: 0  mem: -13740856  diff: -78.92%

 old nu eval: ()
iter: 6,766  gc: 0  mem: -13198112  diff: -24.22%

 new nu eval: ()
iter: 8,777  gc: 4  mem: -13252976  diff: -1.69%

==============================================================================

> (list)
   arc3 eval: nil
iter: 3,593  gc: 0  mem: 14051304  diff: 0%

     ar eval: nil
iter: 794  gc: 4  mem: -13640296  diff: -77.9%

 old nu eval: ()
iter: 889  gc: 0  mem: 10429400  diff: -75.26%

 new nu eval: ()
iter: 2,079  gc: 0  mem: 15120344  diff: -42.14%

==============================================================================

> '(foo bar qux)
   arc3 eval: (foo bar qux . nil)
iter: 7,069  gc: 4  mem: -11772176  diff: 0%

     ar eval: {foo bar qux . nil}
iter: 1,371  gc: 4  mem: -2999736  diff: -80.61%

 old nu eval: (foo bar qux)
iter: 1,857  gc: 0  mem: -10362088  diff: -73.73%

 new nu eval: (foo bar qux)
iter: 3,518  gc: 0  mem: -8229224  diff: -50.23%

==============================================================================

> `(foo bar qux)
   arc3 eval: (foo bar qux)
iter: 3,277  gc: 0  mem: -8084448  diff: 0%

     ar eval: {foo bar qux . nil}
iter: 159  gc: 4  mem: -1444040  diff: -95.15%

 old nu eval: (foo bar qux)
iter: 472  gc: 0  mem: -9117760  diff: -85.6%

 new nu eval: (foo bar qux)
iter: 871  gc: 4  mem: -6310240  diff: -73.42%

==============================================================================

> (list 'foo 'bar 'qux)
   arc3 eval: (foo bar qux . nil)
iter: 2,481  gc: 0  mem: -6342520  diff: 0%

     ar eval: {foo bar qux . nil}
iter: 338  gc: 4  mem: -2817504  diff: -86.38%

 old nu eval: (foo bar qux)
iter: 428  gc: 0  mem: 11492608  diff: -82.75%

 new nu eval: (foo bar qux)
iter: 1,002  gc: 0  mem: -8471224  diff: -59.61%

==============================================================================

> (obj foo 5)
   arc3 eval: #hash((foo . 5))
iter: 1,017  gc: 0  mem: -8990392  diff: 0%

     ar eval: #hash((foo . 5))
iter: 189  gc: 4  mem: -3754688  diff: -81.42%

 old nu eval: #hash((foo . 5))
iter: 760  gc: 0  mem: 10229832  diff: -25.27%

 new nu eval: #hash((foo . 5))
iter: 594  gc: 0  mem: -9936864  diff: -41.59%

==============================================================================

> (let name (obj foo 5) name!foo)
   arc3 eval: 5
iter: 528  gc: 4  mem: -8526392  diff: 0%

     ar eval: 5
iter: 77  gc: 4  mem: -6793280  diff: -85.42%

 old nu eval: 5
iter: 96  gc: 0  mem: 5904800  diff: -81.82%

 new nu eval: 5
iter: 269  gc: 0  mem: 11963024  diff: -49.05%

==============================================================================

> (do1 10 20)
   arc3 eval: 10
iter: 1,145  gc: 4  mem: -9755888  diff: 0%

     ar eval: 10
iter: 228  gc: 4  mem: -7037744  diff: -80.09%

 old nu eval: 10
iter: 377  gc: 0  mem: 8736176  diff: -67.07%

 new nu eval: 10
iter: 1,041  gc: 0  mem: 14784528  diff: -9.08%

==============================================================================

> (after 10 20)
   arc3 eval: 10
iter: 1,019  gc: 16  mem: -10406760  diff: 0%

     ar eval: 10
iter: 180  gc: 8  mem: -12032392  diff: -82.34%

 old nu eval: 10
iter: 656  gc: 0  mem: 14047640  diff: -35.62%

 new nu eval: 10
iter: 807  gc: 20  mem: -10499440  diff: -20.8%

==============================================================================

> (rev '(1 2 3 4 5))
   arc3 eval: (5 4 3 2 1 . nil)
iter: 2,633  gc: 0  mem: -16335752  diff: 0%

     ar eval: {5 4 3 2 1 . nil}
iter: 548  gc: 0  mem: -13350488  diff: -79.19%

 old nu eval: (5 4 3 2 1)
iter: 618  gc: 0  mem: 10305008  diff: -76.53%

 new nu eval: (5 4 3 2 1)
iter: 1,470  gc: 0  mem: 14996184  diff: -44.17%

==============================================================================

> (rev (list 1 2 3 4 5))
   arc3 eval: (5 4 3 2 1 . nil)
iter: 808  gc: 0  mem: -16919312  diff: 0%

     ar eval: {5 4 3 2 1 . nil}
iter: 264  gc: 4  mem: -12581080  diff: -67.33%

 old nu eval: (5 4 3 2 1)
iter: 416  gc: 0  mem: 10524632  diff: -48.51%

 new nu eval: (5 4 3 2 1)
iter: 993  gc: 0  mem: 16327872  diff: 22.9%

==============================================================================

> (join '(1 2) '(3 4))
   arc3 eval: (1 2 3 4 . nil)
iter: 2,248  gc: 0  mem: -14371824  diff: 0%

     ar eval: {1 2 3 4 . nil}
iter: 415  gc: 4  mem: -12489344  diff: -81.54%

 old nu eval: (1 2 3 4)
iter: 519  gc: 0  mem: 11274368  diff: -76.91%

 new nu eval: (1 2 3 4)
iter: 1,217  gc: 0  mem: 15619064  diff: -45.86%

==============================================================================

> (join (list 1 2) (list 3 4))
   arc3 eval: (1 2 3 4 . nil)
iter: 847  gc: 0  mem: -16913312  diff: 0%

     ar eval: {1 2 3 4 . nil}
iter: 217  gc: 0  mem: -12684424  diff: -74.38%

 old nu eval: (1 2 3 4)
iter: 310  gc: 0  mem: 10415208  diff: -63.4%

 new nu eval: (1 2 3 4)
iter: 761  gc: 0  mem: 15904152  diff: -10.15%

==============================================================================

> (rand 50)
   arc3 eval: 49
iter: 2,287  gc: 0  mem: 14657080  diff: 0%

     ar eval: 42
iter: 631  gc: 0  mem: -13786448  diff: -72.41%

 old nu eval: 43
iter: 840  gc: 0  mem: 10276496  diff: -63.27%

 new nu eval: 8
iter: 1,949  gc: 0  mem: 15062728  diff: -14.78%

==============================================================================

