==============================================================================

> (+ 1 2)
   arc3 eval: 3
iter: 182,952  gc: 0  mem: 11275936  diff: 0%

     ar eval: 3
iter: 175,642  gc: 0  mem: 10604320  diff: -4%

 old nu eval: 3
iter: 179,321  gc: 0  mem: 1472  diff: -1.98%

 new nu eval: 3
iter: 263,679  gc: 0  mem: 1792  diff: 44.12%

==============================================================================

> (< 1 2)
   arc3 eval: t
iter: 222,825  gc: 0  mem: -9752160  diff: 0%

     ar eval: t
iter: 182,554  gc: 0  mem: 10844480  diff: -18.07%

 old nu eval: t
iter: 184,066  gc: 0  mem: 512  diff: -17.39%

 new nu eval: t
iter: 276,212  gc: 0  mem: 1632  diff: 23.96%

==============================================================================

> (> 1 2)
   arc3 eval: nil
iter: 229,170  gc: 0  mem: -9275968  diff: 0%

     ar eval: nil
iter: 197,041  gc: 0  mem: -11394672  diff: -14.02%

 old nu eval: ()
iter: 163,999  gc: 0  mem: 992  diff: -28.44%

 new nu eval: ()
iter: 272,722  gc: 0  mem: 1632  diff: 19%

==============================================================================

> '(1 2)
   arc3 eval: (1 2 . nil)
iter: 345,839  gc: 0  mem: 1784  diff: 0%

     ar eval: {1 2 . nil}
iter: 332,847  gc: 0  mem: 1784  diff: -3.76%

 old nu eval: (1 2)
iter: 334,470  gc: 0  mem: 1784  diff: -3.29%

 new nu eval: (1 2)
iter: 332,070  gc: 0  mem: 1464  diff: -3.98%

==============================================================================

> (list 1 2)
   arc3 eval: (1 2 . nil)
iter: 74,866  gc: 4  mem: -1184192  diff: 0%

     ar eval: {1 2 . nil}
iter: 197,031  gc: 4  mem: 56608  diff: 163.18%

 old nu eval: (1 2)
iter: 156,833  gc: 0  mem: 10028064  diff: 109.48%

 new nu eval: (1 2)
iter: 216,603  gc: 0  mem: -11214080  diff: 189.32%

==============================================================================

> (car nil)
   arc3 eval: nil
iter: 319,867  gc: 0  mem: 832  diff: 0%

     ar eval: nil
iter: 319,507  gc: 0  mem: 832  diff: -0.11%

 old nu eval: ()
iter: 119,329  gc: 0  mem: 1152  diff: -62.69%

 new nu eval: ()
iter: 297,291  gc: 0  mem: 1632  diff: -7.06%

==============================================================================

> (car ())
   arc3 eval: nil
iter: 318,657  gc: 0  mem: 1792  diff: 0%

     ar eval: nil
iter: 318,925  gc: 0  mem: 1472  diff: 0.08%

 old nu eval: ()
iter: 184,869  gc: 0  mem: 512  diff: -41.98%

 new nu eval: ()
iter: 310,038  gc: 0  mem: 672  diff: -2.7%

==============================================================================

> (car '())
   arc3 eval: nil
iter: 320,446  gc: 0  mem: 832  diff: 0%

     ar eval: nil
iter: 307,379  gc: 0  mem: 672  diff: -4.08%

 old nu eval: ()
iter: 185,104  gc: 0  mem: 832  diff: -42.24%

 new nu eval: ()
iter: 294,580  gc: 0  mem: 832  diff: -8.07%

==============================================================================

> (let a 5 a)
   arc3 eval: 5
iter: 346,687  gc: 0  mem: 984  diff: 0%

     ar eval: 5
iter: 347,988  gc: 0  mem: 984  diff: 0.38%

 old nu eval: 5
iter: 347,461  gc: 0  mem: 1144  diff: 0.22%

 new nu eval: 5
iter: 349,330  gc: 0  mem: 1144  diff: 0.76%

==============================================================================

> (let a (list 1 2) (car a))
   arc3 eval: 1
iter: 72,542  gc: 0  mem: -1940368  diff: 0%

     ar eval: 1
iter: 197,885  gc: 4  mem: 165584  diff: 172.79%

 old nu eval: 1
iter: 112,248  gc: 0  mem: 7086400  diff: 54.74%

 new nu eval: 1
iter: 204,534  gc: 0  mem: -11657288  diff: 181.95%

==============================================================================

> (let (a b) (list 1 2) a)
   arc3 eval: 1
iter: 70,758  gc: 4  mem: -229920  diff: 0%

     ar eval: 1
iter: 150,083  gc: 4  mem: 3572304  diff: 112.11%

 old nu eval: 1
iter: 119,756  gc: 0  mem: 6971168  diff: 69.25%

 new nu eval: 1
iter: 230,969  gc: 0  mem: -10304128  diff: 226.42%

==============================================================================

> (let (a b) (list 1 2) b)
   arc3 eval: 2
iter: 70,383  gc: 4  mem: -270560  diff: 0%

     ar eval: 2
iter: 149,562  gc: 4  mem: 2715120  diff: 112.5%

 old nu eval: 2
iter: 125,755  gc: 0  mem: 8000288  diff: 78.67%

 new nu eval: 2
iter: 227,148  gc: 0  mem: -11440456  diff: 222.73%

==============================================================================

> (let a '(1 2) (car a))
   arc3 eval: 1
iter: 318,026  gc: 0  mem: 1792  diff: 0%

     ar eval: 1
iter: 303,670  gc: 0  mem: 1632  diff: -4.51%

 old nu eval: 1
iter: 174,756  gc: 0  mem: 512  diff: -45.05%

 new nu eval: 1
iter: 287,469  gc: 0  mem: 1792  diff: -9.61%

==============================================================================

> (let (a b) '(1 2) a)
   arc3 eval: 1
iter: 287,511  gc: 0  mem: 8954072  diff: 0%

     ar eval: 1
iter: 202,205  gc: 0  mem: -13059120  diff: -29.67%

 old nu eval: 1
iter: 210,825  gc: 0  mem: 664  diff: -26.67%

 new nu eval: 1
iter: 330,730  gc: 0  mem: 1624  diff: 15.03%

==============================================================================

> (let (a b) '(1 2) b)
   arc3 eval: 2
iter: 290,482  gc: 0  mem: 9265696  diff: 0%

     ar eval: 2
iter: 202,787  gc: 0  mem: -13139984  diff: -30.19%

 old nu eval: 2
iter: 211,443  gc: 0  mem: 1144  diff: -27.21%

 new nu eval: 2
iter: 328,832  gc: 0  mem: 1784  diff: 13.2%

==============================================================================

> (idfn 'x)
   arc3 eval: x
iter: 318,038  gc: 0  mem: 1792  diff: 0%

     ar eval: x
iter: 314,168  gc: 0  mem: 1472  diff: -1.22%

 old nu eval: x
iter: 189,093  gc: 0  mem: 832  diff: -40.54%

 new nu eval: x
iter: 293,689  gc: 0  mem: 832  diff: -7.66%

==============================================================================

> (apply idfn '(x))
   arc3 eval: x
iter: 205,043  gc: 0  mem: 12806496  diff: 0%

     ar eval: x
iter: 155,808  gc: 0  mem: -11135488  diff: -24.01%

 old nu eval: x
iter: 68,971  gc: 0  mem: 8813024  diff: -66.36%

 new nu eval: x
iter: 102,285  gc: 0  mem: 5758016  diff: -50.12%

==============================================================================

> '(1 2 3 4 5)
   arc3 eval: (1 2 3 4 5 . nil)
iter: 351,280  gc: 0  mem: 824  diff: 0%

     ar eval: {1 2 3 4 5 . nil}
iter: 331,316  gc: 0  mem: 984  diff: -5.68%

 old nu eval: (1 2 3 4 5)
iter: 334,763  gc: 0  mem: 1784  diff: -4.7%

 new nu eval: (1 2 3 4 5)
iter: 329,655  gc: 0  mem: 1784  diff: -6.16%

==============================================================================

> (list 1 2 3 4 5)
   arc3 eval: (1 2 3 4 5 . nil)
iter: 39,385  gc: 4  mem: 7796288  diff: 0%

     ar eval: {1 2 3 4 5 . nil}
iter: 109,181  gc: 4  mem: 7532744  diff: 177.21%

 old nu eval: (1 2 3 4 5)
iter: 65,769  gc: 4  mem: -7981336  diff: 66.99%

 new nu eval: (1 2 3 4 5)
iter: 126,341  gc: 4  mem: -3607288  diff: 220.78%

==============================================================================

> (apply list 1 2 3 4 (list 5))
   arc3 eval: (1 2 3 4 5 . nil)
iter: 27,914  gc: 4  mem: -4602912  diff: 0%

     ar eval: {1 2 3 4 5 . nil}
iter: 43,320  gc: 4  mem: -6400744  diff: 55.19%

 old nu eval: (1 2 3 4 5)
iter: 25,596  gc: 4  mem: 6420728  diff: -8.3%

 new nu eval: (1 2 3 4 5)
iter: 51,070  gc: 4  mem: -1585784  diff: 82.95%

==============================================================================

> ()
   arc3 eval: ()
iter: 345,818  gc: 0  mem: 984  diff: 0%

     ar eval: nil
iter: 344,072  gc: 0  mem: 992  diff: -0.5%

 old nu eval: ()
iter: 347,111  gc: 0  mem: 664  diff: 0.37%

 new nu eval: ()
iter: 348,383  gc: 0  mem: 824  diff: 0.74%

==============================================================================

> nil
   arc3 eval: nil
iter: 350,597  gc: 0  mem: 984  diff: 0%

     ar eval: nil
iter: 345,870  gc: 0  mem: 992  diff: -1.35%

 old nu eval: ()
iter: 171,596  gc: 0  mem: 992  diff: -51.06%

 new nu eval: ()
iter: 325,785  gc: 0  mem: 1632  diff: -7.08%

==============================================================================

> (list)
   arc3 eval: nil
iter: 173,483  gc: 0  mem: 9807712  diff: 0%

     ar eval: nil
iter: 307,869  gc: 0  mem: 672  diff: 77.46%

 old nu eval: ()
iter: 190,467  gc: 0  mem: 832  diff: 9.79%

 new nu eval: ()
iter: 278,730  gc: 0  mem: 832  diff: 60.67%

==============================================================================

> '(foo bar qux)
   arc3 eval: (foo bar qux . nil)
iter: 345,141  gc: 0  mem: 824  diff: 0%

     ar eval: {foo bar qux . nil}
iter: 331,038  gc: 0  mem: 824  diff: -4.09%

 old nu eval: (foo bar qux)
iter: 332,574  gc: 0  mem: 824  diff: -3.64%

 new nu eval: (foo bar qux)
iter: 333,373  gc: 0  mem: 824  diff: -3.41%

==============================================================================

> `(foo bar qux)
   arc3 eval: (foo bar qux)
iter: 345,951  gc: 0  mem: 984  diff: 0%

     ar eval: {foo bar qux . nil}
iter: 56,561  gc: 4  mem: 2752672  diff: -83.65%

 old nu eval: (foo bar qux)
iter: 288,294  gc: 4  mem: 4647720  diff: -16.67%

 new nu eval: (foo bar qux)
iter: 286,506  gc: 4  mem: 4550872  diff: -17.18%

==============================================================================

> (list 'foo 'bar 'qux)
   arc3 eval: (foo bar qux . nil)
iter: 59,663  gc: 4  mem: 3640360  diff: 0%

     ar eval: {foo bar qux . nil}
iter: 175,269  gc: 4  mem: 5266632  diff: 193.76%

 old nu eval: (foo bar qux)
iter: 142,833  gc: 0  mem: -8028400  diff: 139.4%

 new nu eval: (foo bar qux)
iter: 196,477  gc: 4  mem: -3126552  diff: 229.31%

==============================================================================

> (obj foo 5)
   arc3 eval: #hash((foo . 5))
iter: 11,792  gc: 4  mem: 993496  diff: 0%

     ar eval: #hash((foo . 5))
iter: 12,039  gc: 4  mem: -3648880  diff: 2.09%

 old nu eval: #hash((foo . 5))
iter: 7,873  gc: 0  mem: 5670264  diff: -33.23%

 new nu eval: #hash((foo . 5))
iter: 12,663  gc: 0  mem: -9678256  diff: 7.39%

==============================================================================

> (let name (obj foo 5) name!foo)
   arc3 eval: 5
iter: 10,597  gc: 4  mem: -2021424  diff: 0%

     ar eval: 5
iter: 10,828  gc: 4  mem: -4773520  diff: 2.18%

 old nu eval: 5
iter: 6,156  gc: 0  mem: 5615808  diff: -41.91%

 new nu eval: 5
iter: 11,835  gc: 0  mem: -10585592  diff: 11.68%

==============================================================================

> (do1 10 20)
   arc3 eval: 10
iter: 345,063  gc: 0  mem: 984  diff: 0%

     ar eval: 10
iter: 342,957  gc: 0  mem: 1144  diff: -0.61%

 old nu eval: 10
iter: 340,527  gc: 0  mem: 984  diff: -1.31%

 new nu eval: 10
iter: 351,565  gc: 0  mem: 984  diff: 1.88%

==============================================================================

> (after 10 20)
   arc3 eval: 10
iter: 84,939  gc: 0  mem: -10735896  diff: 0%

     ar eval: 10
iter: 82,172  gc: 0  mem: -10861832  diff: -3.26%

 old nu eval: 10
iter: 85,693  gc: 0  mem: -10447456  diff: 0.89%

 new nu eval: 10
iter: 82,086  gc: 0  mem: 10357392  diff: -3.36%

==============================================================================

> (rev '(1 2 3 4 5))
   arc3 eval: (5 4 3 2 1 . nil)
iter: 45,603  gc: 4  mem: 4302616  diff: 0%

     ar eval: {5 4 3 2 1 . nil}
iter: 39,060  gc: 4  mem: 375928  diff: -14.35%

 old nu eval: (5 4 3 2 1)
iter: 9,560  gc: 0  mem: 2066040  diff: -79.04%

 new nu eval: (5 4 3 2 1)
iter: 48,211  gc: 0  mem: 10416768  diff: 5.72%

==============================================================================

> (rev (list 1 2 3 4 5))
   arc3 eval: (5 4 3 2 1 . nil)
iter: 22,138  gc: 4  mem: 9106704  diff: 0%

     ar eval: {5 4 3 2 1 . nil}
iter: 31,848  gc: 4  mem: -11312064  diff: 43.86%

 old nu eval: (5 4 3 2 1)
iter: 8,593  gc: 0  mem: 6531848  diff: -61.18%

 new nu eval: (5 4 3 2 1)
iter: 39,187  gc: 4  mem: -2056536  diff: 77.01%

==============================================================================

> (join '(1 2) '(3 4))
   arc3 eval: (1 2 3 4 . nil)
iter: 17,913  gc: 4  mem: 8009360  diff: 0%

     ar eval: {1 2 3 4 . nil}
iter: 102,897  gc: 4  mem: -2041584  diff: 474.43%

 old nu eval: (1 2 3 4)
iter: 122,256  gc: 4  mem: -7493776  diff: 582.5%

 new nu eval: (1 2 3 4)
iter: 192,203  gc: 0  mem: -10585776  diff: 972.98%

==============================================================================

> (join (list 1 2) (list 3 4))
   arc3 eval: (1 2 3 4 . nil)
iter: 13,238  gc: 4  mem: 9111144  diff: 0%

     ar eval: {1 2 3 4 . nil}
iter: 77,398  gc: 4  mem: 7671656  diff: 484.67%

 old nu eval: (1 2 3 4)
iter: 71,289  gc: 0  mem: -4790488  diff: 438.52%

 new nu eval: (1 2 3 4)
iter: 131,049  gc: 4  mem: 2091336  diff: 889.95%

==============================================================================

> (rand 50)
   arc3 eval: 43
iter: 166,911  gc: 0  mem: 832  diff: 0%

     ar eval: 38
iter: 84,801  gc: 0  mem: 10631464  diff: -49.19%

 old nu eval: 6
iter: 125,851  gc: 0  mem: 992  diff: -24.6%

 new nu eval: 43
iter: 162,464  gc: 0  mem: 832  diff: -2.66%

==============================================================================
