#lang racket

;; INTERSECTION-DETECTION OF 2 POLYHEDRA
;; This program is the implementation to detect the intersection between 2
;; polyhedra. It uses the Bentley-Ottman Algorithm of intersection detection 
;; of 2 line segments. The polyhedra has vertices and sides. The sides are 
;; considered as line segments and the vertices are considered as ends of
;; line-segments.
;; This implementation only detects if there is an intersection or not, but
;; it doesnot store the intersection points.
;; The runtime of this implementation is O(n).

;; The inputs for this implementation are the vertices and the already 
;; calculated line-equations of polyhedra in the structural format as given
;; in the examples below.
;; The program should be run with the "main" function.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IMPLEMENTATION

(require rackunit)
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS
;; These inculde the data structures required for the implementation of the
;; algorithm and the templates to be followed.

;; A structure to store the vertex of polyhedra.
(define-struct vertex(x y))
;; Interp: 
;; vertex has 2 fields
;; -x -It is the x-coordinate of the vertex
;; -y -It is the y-coordinate of the vertex

;;TEMPLATE:
;;(define (struct-vertex-fn v)
;;  (... (vertex-x v)
;;       (vertex-y v)))

;; A structure to store the line-equations.
(define-struct line-eq(a b c))

;; Interp:
;; line has 3 fields
;; - a -It is the x-coefficient of line equation
;; - b -It is the y-coefficient of line equation
;; - c -It is the constant of line equation

;; TEMPLATE:
;;(define (struct-line-fn v)
;;  (... (vertex-x v)
;;       (vertex-y v)))

;; A List is required to store the vertices of each polyhedra.
;; A ListOf<vertex> is one of
;; - empty
;; - (cons vertex empty)

;; TEMPLATE:
;; (define (list-vertex-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (struct-fn (first lst))
;;                (list-fn (rest lst)))]))

;; A list is required to store the line equations along with the vertices 
;; which formed the line/side of polyhedra.
;; A side is a list of vertices and the line equation formed by the vertices,
;; ie, ListOf< vertex vertex line-eq>

;; A list is required to store the sides of a polyhedra
;; A Poly is a ListOf<side>, which is one of
;; - empty
;; - (cons side empty)

;; TEMPLATE:
;; (define (list-line-fn lst)
;;   (cond
;;     [(empty? lst) ...]
;;     [else (... (struct-fn (first lst))
;;                (list-fn (rest lst)))]))

;; An intersection of 2 polyhedra is one of the following types:
;; -vertex-vertex intersection
;; -vertex-line intersection
;; -line-line intersection

;; We assume a sweep-line which is a vertical line of the equation x=constant
;; which sweeps through the polyhedra and finds if they are intersecting each
;; other.When this line sweeps over, the following 3 events may occur.
;; - sweepline may intersect a vertex of a polyhedra
;; - sweepline may intersect a side of a polyhedra
;; - sweepline doesnot intersect any part of polyhedra

;; TEMPLATE:
;(define (event-fn x poly)
;  (cond
;    [(vertex? x poly)...]
;    [(line? x poly)...]
;    [else ...]

;; Examples:
;; The following examples include the events of sweepline which are further
;; tested at the end.

;; line-line intersection
(define vertices1 (list (make-vertex 1 1)
                    (make-vertex 3 1)
                    (make-vertex 2 3)))

(define vertices2 (list (make-vertex 2 2)
                    (make-vertex 4 2)
                    (make-vertex 4 4)))

(define poly1 (list (list (make-vertex 1 1) (make-vertex 3 1) 
                                    (make-line-eq 0 1 1))
                (list (make-vertex 2 3) (make-vertex 3 1) 
                                    (make-line-eq 2 1 7))
                (list (make-vertex 1 1) (make-vertex 2 3) 
                                    (make-line-eq -2 1 -1))))

(define poly2 (list (list (make-vertex 2 2) (make-vertex 4 2) 
                                    (make-line-eq 0 1 2))
                (list (make-vertex 4 2) (make-vertex 4 4) 
                                    (make-line-eq 1 0 4))
                (list (make-vertex 2 2) (make-vertex 4 4) 
                                    (make-line-eq 1 -1 0))))

;; no intersection
(define vertices3 (list (make-vertex 0 1)
                    (make-vertex 1 2)
                    (make-vertex 2 3)))

(define vertices4 (list (make-vertex 5 6)
                    (make-vertex 6 7)
                    (make-vertex 7 8)))

(define poly3 (list (list (make-vertex 0 1) (make-vertex 1 2) 
                                    (make-line-eq 1 -1 1))
                (list (make-vertex 1 2) (make-vertex 2 3) 
                                    (make-line-eq 1 -1 1))
                (list (make-vertex 0 1) (make-vertex 2 3) 
                                    (make-line-eq 1 -1 1))))

(define poly4 (list (list (make-vertex 5 6) (make-vertex 6 7) 
                                    (make-line-eq 1 -1 1))
                (list (make-vertex 6 7) (make-vertex 7 8) 
                                    (make-line-eq 1 -1 -1))
                (list (make-vertex 5 6) (make-vertex 7 8) 
                                    (make-line-eq 1 -1 1))))

;; line-vertex intersection
(define vertices5 (list (make-vertex 1 1)
                    (make-vertex 4 1)
                    (make-vertex 3 3)))

(define vertices6 (list (make-vertex 2 2)
                    (make-vertex 1 2)
                    (make-vertex 2 3)))

(define poly5 (list (list (make-vertex 1 1) (make-vertex 4 1) 
                                    (make-line-eq 0 1 1))
                (list (make-vertex 3 3) (make-vertex 4 1) 
                                    (make-line-eq 2 1 9))
                (list (make-vertex 1 1) (make-vertex 3 3) 
                                    (make-line-eq 1 -1 0))))

(define poly6 (list (list (make-vertex 1 2) (make-vertex 2 2) 
                                    (make-line-eq 0 1 2))
                (list (make-vertex 1 2) (make-vertex 2 3) 
                                    (make-line-eq 1 -1 -1))
                (list (make-vertex 2 2) (make-vertex 2 3) 
                                    (make-line-eq 1 0 2))))

;; 2 lines intersection
(define vertices7 (list (make-vertex 1 1)
                    (make-vertex 3 1)))

(define vertices8 (list (make-vertex 2 1)
                    (make-vertex 2 2)))

(define poly7 (list (list (make-vertex 1 1) (make-vertex 3 1) 
                                    (make-line-eq 0 1 1))))

(define poly8 (list (list (make-vertex 2 1) (make-vertex 2 2) 
                                    (make-line-eq 1 0 2))))

;; non-intersecting lines
(define vertices9 (list (make-vertex 1 1)
                    (make-vertex 2 2)))

(define vertices10 (list (make-vertex 3 3)
                    (make-vertex 4 4)))

(define poly9 (list (list (make-vertex 1 1) (make-vertex 2 2) 
                                    (make-line-eq 1 -1 0))))

(define poly10 (list (list (make-vertex 3 3) (make-vertex 4 4) 
                                    (make-line-eq 1 -1 0))))

;; square and triangle intersecting at vertex
(define vertices11 (list (make-vertex 1 1)
                    (make-vertex 3 1)
                    (make-vertex 3 3)
                    (make-vertex 1 3)))

(define vertices12 (list (make-vertex 3 1)
                    (make-vertex 5 1)
                    (make-vertex 4 3)))

(define poly11 (list (list (make-vertex 1 1) (make-vertex 3 1) 
                                    (make-line-eq 1 0 1))
                (list (make-vertex 3 1) (make-vertex 3 3) 
                                    (make-line-eq 1 0 3))
                (list (make-vertex 1 3) (make-vertex 3 3) 
                                    (make-line-eq 0 1 3))
                (list (make-vertex 1 1) (make-vertex 1 3)
                      (make-line-eq 1 0 1))))

(define poly12 (list (list (make-vertex 3 1) (make-vertex 5 1) 
                                    (make-line-eq 1 0 3))
                (list (make-vertex 4 3) (make-vertex 5 1) 
                                    (make-line-eq 2 1 5))
                (list (make-vertex 3 1) (make-vertex 4 3) 
                                    (make-line-eq 2 -1 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CODE

;; main: vertex vertex poly poly -> Boolean
;; This is the main function which takes the vertices and line equations
;; of polyhedra and checks whether they are intersecting or not
;; Strategy: Function Composition
(define (main vertices-a vertices-b poly-a poly-b)
  (intersection-detection? (get-max (get-x-vertices vertices-a)) 
                           vertices-a 
                           vertices-b
                           poly-a
                           poly-b))

;; get-max: List<Number> -> Number
;; Takes a list of numbers and gives the maximum value from the list, which
;; is assigned to the sweep line.
;; Strategy: Structural Decomposition [lst]
(define (get-max lst)
  (cond
    [(empty? lst) 0]
    [else (max (first lst)
               (get-max (rest lst)))]))

;; get-min: List<Number> -> Number
;; Takes a list of numbers and gives the minimum value from the list, which
;; is assigned to the sweep line.
;; Strategy: Structural Decomposition [lst]
(define (get-min lst)
  (cond
    [(empty? lst) 0]
    [else (min (first lst)
               (get-min (rest lst)))]))

;; get-x-vertices: ListOf<vertex> -> ListOf<Number>
;; Take the list of vertices of 1st polyhedra and gives the list of the 
;; x-coordinates of the first polyhedra from which the maximum value is
;; assigned to the sweep line.
;; Strategy: Structural Decompostion [vertices]
(define (get-x-vertices vertices)
  (local
    ((define (abstract-function v)
       (get-x v)))
    (map abstract-function vertices)))

;; get-v: vertex -> Number
;; Given a vertex, gives the x-coordinate of the vertex.
;;Strategy: Structural Decomposition [vertex]
(define (get-x v)
  (vertex-x v))

;; intersection-detection?: Number List<vertex> List<vertex> Poly Poly
;;                           -> Boolean
;; Given the sweepline, vertices and line equations of Poly, it checks
;; and gives true if there is intersection, else gives a false
;; Strategy: Generative Recursion
;; TERMINATION ARGUMENT: This recursion ends when the sweepline is x=0.
;; As the value of sweepline reaches 0, we have less area of the polyhedra
;; to be checked for intersection detection.
(define (intersection-detection? x vertices-a vertices-b poly-a poly-b)
  (cond
    [(<= x (get-min (get-x-vertices vertices-a))) false]
    [(sweep-line? x vertices-a vertices-b poly-a poly-b) true]
    [else (intersection-detection? (- x 1)
                                   vertices-a vertices-b poly-a poly-b)]))

;; sweep-line?:Number List<vertex> List<vertex> Poly Poly
;;                           -> Boolean
;; Checks if the sweepline has intersected a vertex or side of the
;; 1st polyhedra
;; Strategy: Structural Decomposition on the event of x [sweepline]
(define (sweep-line? x vertices-a vertices-b poly-a poly-b)
  (cond
    [(vertex-in-p1? x vertices-a) (send-one-from-y 
                                  x 
                                  (get-vertex-y-for-p1 vertices-a)
                                  vertices-b poly-b)]
    [(line-in-p1? x poly-a) (calculate-y-for-p1 x poly-a vertices-b poly-b)]
    [else false]))

;; vertex-in-p1?: Number List<vertex> -> Boolean
;; Takes the sweepline and list of vertices of 1st polyhedra, checks if the 
;; sweepline is intersecting the vertex of 1st polyhedra.
;; Strategy: Structural Decomposition [vertices-a]
(define (vertex-in-p1? x vertices-a)
  (cond
    [(empty? vertices-a) false]
    [else (or (check-if-vertex? x (first vertices-a))
              (vertex-in-p1? x (rest vertices-a)))]))  

;; check-if-vertex?: Number vertex -> Boolean
;; Checks if the sweep line is in that particular vertex.
;; Strategy: Structural Decomposition on vertex [v]
(define (check-if-vertex? x v)
  (= x (vertex-x v)))

;; get-vertex-y-for-p1: ListOf<vertex> -> ListOf<Number>
;; Given the list of vertices of a polyhedra, gives the y-coordinates of them.
;; Strategy: Structural Decomposition [vertices-a]
(define (get-vertex-y-for-p1 vertices-a)
  (cond
    [(empty? vertices-a) empty]
    [else (cons (get-y (first vertices-a))
                (get-vertex-y-for-p1 (rest vertices-a)))]))  

;; get-y: vertex -> Number
;; Given a vertex, gives the y-coordinate of it.
;; Strategy: Structural Decomposition [vertex]
(define (get-y v)
  (vertex-y v))

;; send-one-from-y: Number List<Number> List<vertex> Poly -> Boolean
;; Takes sweepline and the list of y-coordinates of 1st polyhedra, and 
;; the 2nd polyhedra and its vertices and checks for intersection of the 
;; the vertex of 1st polyhedra with the 2nd polyhedra.
;; Strategy: Structural Decomposition Using Abstraction on ListOf<Number> [y]
(define (send-one-from-y x y vertices-b poly-b)
  (local
    ((define (send-one-abstract y1)
       (present-in-p2? x y1 vertices-b poly-b)))
    (ormap send-one-abstract y)))

;; line-in-p1?: Number Poly -> Boolean
;; Given the sweepline and polyhedra, checks if the sweepline is passing
;; through and of the side of polyhedra
;; Strategy: Structural Decomposition [Poly]
(define (line-in-p1? x poly-a)
  (cond
    [(empty? poly-a) false]
    [else (or (belongs? x (first poly-a))
              (line-in-p1? x (rest poly-a)))]))

;; belongs?: Number side -> Boolean
;; Given the sweepline and the vertex, checks if the point of intersection of
;; the sweepline and the side, lies between the vertices formed by the side.
;; Strategy: Structural Decomposition [side]
(define (belongs? x v)
  (and (>= x (vertex-x (first v)))
       (<= x (vertex-x (second v)))
       (>= (calculate-y x (third v)) (vertex-y (first v)))
       (<= (calculate-y x (third v)) (vertex-y (second v)))))

;; calculate-y: Number line-eq -> Number
;; Given the sweepline, and the line-eq, finds the y-coordinate of their 
;; intersection
;; Strategy: Domain Knowledge
(define (calculate-y x l)
  (if (= (line-eq-b l) 0) 0
  (/ (- (line-eq-c l) (* (line-eq-a l) x)) (line-eq-b l)))) 

;; calculate-y-for-p1: Number Poly List<vertex> List<vertex>
;; Given sweepline, 1st polyhedra , 2nd polyhedra and its vertices,
;; checks if the particular sweepline and 1st polyhedra are intersecting 
;; the 2nd polyhedra at the same point.
;; Strategy: Structural Decomposition Using Abstraction [Poly]
(define (calculate-y-for-p1 x poly-a vertices-b poly-b)
  (local
    ((define (abstract-function side)
       (present-in-p2? x (calculate-helper x side) vertices-b poly-b)))
    (ormap abstract-function poly-a)))

;; calculate-helper: Number side -> Number
;; Calculates the y-coordinate for a given x and the line equation of the side
;; Strategy: Structural Decomposition [side]
(define (calculate-helper x p)
    (calculate-y x (third p)))
 
;; present-in-p2?: Number Number List<vertex> Poly -> Boolean
;; Given the x and y coordinates of sweepline intersecting the 1st polyhedra,
;; checks if the point is intersecting with the vertex or line of 2nd 
;; polyhedra.
;; Strategy: Structural Decomposition on the sweepline-event of x
(define (present-in-p2? x y vertices-b poly-b)
    (cond
      [(belongs-to-vertex-p2? x y vertices-b) true]
      [(belongs-to-line-p2? x y poly-b) true]
      [else false])) 

;; belongs-to-vertex-p2? Number Number List<vertex> -> Boolean
;; checks if the given coordinates are intersecting with the vertices of the
;; 2nd polyhedra
;; Strategy: Structural Decomposition Using Abstraction [ListOf<vertex>]
(define (belongs-to-vertex-p2? x y vertices-b)
  (local
    ((define (abstract-vertex-helper v)
       (check-vertex x y v)))
    (ormap abstract-vertex-helper vertices-b)))

;; check-vertex: Number Number vertex -> Boolean
;; Given the x-coordinates, checks with that particular vertex if there is
;; an intersection.
;; Strategy: Structural Decomposition [vertex]
(define (check-vertex x y v)
  (and (= x (vertex-x v))
       (= y (vertex-y v))))

;; belongs-to-line-p2? Number Number Poly -> Boolean
;; Given the coordinates and the 2nd polyhedra, checks if the coordinates are
;; lying on any of the sides of the 2nd polyhedra
;; Strategy: Structural Decomposition Using Abstraction [Poly] and [side]
(define (belongs-to-line-p2? x y poly-b)
  (local
    ((define (abstract-helper p)
       (and (check-if-on-line? x y (third p))
              (belongs? x p))))
    (ormap abstract-helper poly-b))) 

;; check-if-on-line?: Number Number line-eq -> Boolean
;; Checks if the coordinates lie on the line-eq.
;; Strategy: Domain Knowledge
(define (check-if-on-line? x y l)
  (= (line-eq-c l) (+ (* (line-eq-a l) x)
                            (* (line-eq-b l) y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TESTS for the given examples 

;; the sides of the polyhedra are
(check-equal? (main vertices1 vertices2 poly1 poly2) 
              true
              "when the sides of 2 triangles are intersecting")

;; polyhedra are not intersecting
(check-equal? (main vertices3 vertices4 poly3 poly4)
              false
              "when 2 triangles are not intersecting")

;; side of a polyhedra intersects with vertex of another polyhedra
(check-equal? (main vertices5 vertices6 poly5 poly6)
              true
              "when the side of 1st triangle intersects with the vertex of
               the 2nd triangle")

;; intersection-detection of 2 line segments
(check-equal? (main vertices7 vertices8 poly7 poly8) 
              true
              "when 2 line segments are intersecting")

;; line segments are not intersecting
(check-equal? (main vertices9 vertices10 poly9 poly10) 
              false
              "when 2 line segments are not intersecting")

;; when 2 different polyhedra are intersecting at the vertex
(check-equal? (main vertices11 vertices12 poly11 poly12)
              true
              "when a square and a triangle are intersecting
               each other at the vertex")

