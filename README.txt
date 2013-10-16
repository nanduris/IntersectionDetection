
========READ ME========

1. The coding implementation is done in DrRacket. The following is the link to download DrRacket, "http://racket-lang.org/"

2. Once downloaded, the setting at the left bottom should be changed to 'Determine language from custom'.

3. Open the source file, and run the program. 

4. The input is given at the end of the source code for testing. There is no separate input file.

5. The program should be run as (main (list (make-struct x1 y1)
					    (make-struct x2 y2)
					    ...
					    ...) 
				      (list (make-struct x5 y5)
					    (make-struct x6 y6)
					    ...
					    ...)
                                      (list (list (make-struct x1 y1) (make-struct x2 y2) (make-line-eq a1 b1 c1))
					    (list (make-struct x3 y3) (make-struct x4 y4) (make-line-eq a2 b2 c2))
					    ...
					    ...) 
				      (list (list (make-struct x5 y5) (make-struct x6 y6) (make-line-eq a3 b3 c3))
					    (list (make-struct x7 y7) (make-struct x8 y8) (make-line-eq a4 b4 c4))
					    ...
					    ...))


where the first 'list' gives the vertices of 1st polyhedra
          second 'list' gives the vertices of 2nd polyhedra
          third 'list gives the vertices in increasing order of their x-coordinate and the line-equation formed by them for the 1st polyhedra
          fourth 'list gives the vertices in increasing order of their x-coordinate and the line-equation formed by them for the 1st polyhedra