(in-package :gambol)

(clear-rules)  ; start with a clean slate

(*- (member ?x (?x . ??)))
(*- (member ?x (?? . ?l)) (member ?x ?l))


(*- (is-symbol? ?form)
    (is symbol (lisp (type-of '?form))))

(*- (is-integer? ?form)
    (lisp (typep '?form 'integer)))

;;;; Quicksort, translated from lips-test.l
(*- (partition (?x . ?l) ?y (?x . ?l1) ?l2)
    (lisp (< ?x ?y)) (cut)
    (partition ?l ?y ?l1 ?l2))

(*- (partition (?x . ?l) ?y ?l1 (?x . ?l2))
    (partition ?l ?y ?l1 ?l2))

(*- (partition nil ?QQQQ nil nil))


(*- (qsort (?x . ?l) ?r ?r0)
    (partition ?l ?x ?l1 ?l2)
    (qsort ?l2 ?r1 ?r0)
    (qsort ?l1 ?r (?x . ?r1)))

(*- (qsort nil ?r ?r))

(*- (list50 (27 74 17 33 94 18 46 83 65 2 32 53 28 85 99 47 28 82 6 11 55 29 39
	    81 90 37 10 0 66 51 7 21 85 27 31 63 75 4 95 99 11 28 61 74 18 92
	    40 53 59 8)))

(??- (list50 ?l) (qsort ?l ?x nil) (lisp-always (pprint '?x)))
(??- (list50 ?l) (qsort ?l ?x nil))
