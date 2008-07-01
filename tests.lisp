(asdf:oos 'asdf:load-op :gambol)
(asdf:oos 'asdf:load-op :lift)

(defpackage :gambol-tests
  (:use :cl :gambol :lift))

(in-package :gambol-tests)

(deftestsuite gambol-tests () ()
  ;; start and end with a clean slate
  (:setup (clear-rules))
  (:teardown (clear-rules)))

(defun get-unified-value (item environment)
  "yank the value of a logical variable out of a SOLVE'd environment"
  (cdr (assoc item (car environment))))

(defun get-unified-values (items environment)
  (mapcar #'(lambda (item) (get-unified-value item environment)) items))


;;; Let's start with basic list manipulations.
(addtest (gambol-tests)
  head-1
  (ensure-same
   'A
   (progn
     (*- (head ?h (?h . ??)))
     (get-unified-value '?h (pl-solve-all '((head ?h (a b c))))))))

(addtest (gambol-tests)
  head-2
  (ensure-same
   '(A B)
   (progn
     (*- (head ?h (?h . ??)))
     (get-unified-value '?h (pl-solve-all '((head ?h ((a b) c))))))))

(addtest (gambol-tests)
  head-3
  (ensure-same
   '(A B)
   (progn
     (*- (head ?h (?h . ??)))
     (get-unified-values '(?h1 ?h2)
                         (pl-solve-all '((head (?h1 ?h2) ((a b) c d))))))))

(addtest (gambol-tests)
  tail
  (ensure-same
   '(B C)
   (progn
     (*- (tail ?t (?? . ?t)))
     (get-unified-value '?t (pl-solve-all '((tail ?t (a b c))))))))

(addtest (gambol-tests)
  member
  (ensure-same
   '(T)
   (progn
     (*- (member ?x (?x . ??)))
     (*- (member ?x (?? . ?l))
         (member ?x ?l))
     (pl-solve-all '((member 3 (1 2 3 4 5)))))))

(addtest (gambol-tests)
  append-1
  (ensure-same
   '(a b c d)
   (progn
     (*- (append (?x . ?xs) ?ys (?x . ?zs))
         (append ?xs ?ys ?zs))
     (*- (append nil ?ys ?ys))
     (get-unified-value '?a (pl-solve-all '((append (a b) (c d) ?a)))))))

(addtest (gambol-tests)
  append-2
  (ensure-same
   '(c d)
   (progn
     (*- (append (?x . ?xs) ?ys (?x . ?zs))
         (append ?xs ?ys ?zs))
     (*- (append nil ?ys ?ys))
     (get-unified-value '?a (pl-solve-all '((append (a b) ?a (a b c d))))))))

;;; More fun to verify: (pl-solve-all '((append ?a ?b (a b c d))))

(addtest (gambol-tests)
  append-3-reverse
  (ensure-same
   '(19 5 24 21 21 11 0 5 6 3 3 0 20 19 15 8)
   (progn
     (*- (append (?x . ?xs) ?ys (?x . ?zs))
         (append ?xs ?ys ?zs))
     (*- (append nil ?ys ?ys))
     (*- (reverse (?x . ?xs) ?zs)
         (reverse ?xs ?ys)
         (append ?ys (?x) ?zs))
     (*- (reverse nil nil))
     (get-unified-value '?a (pl-solve-all '((reverse (8 15 19 20 0 3 3 6 5 0 11 21 21 24 5 19) ?a)))))))

;;; Negation as Failure is weird.
(addtest (gambol-tests)
  closed-world
  (ensure-null
   (progn
     (*- (member ?x (?x . ??)))
     (*- (member ?x (?? . ?l))
         (member ?x ?l))
     (*- (not ?p)      ; assuming a closed world...
         ?p
         (cut)
         (fail))
     (*- (not ?p))
     (pl-solve-all '((not (member 5 (1 2 3 4 5))))))))

;;; Yanking values from Lisp.
(addtest (gambol-tests)
  lop
  (ensure-same
   6
   (progn
     (*- (length nil 0))
     (*- (length (?? . ?t) ?n)
         (length ?t ?n1)
         (is ?n (lop (1+ ?n1))))
     (get-unified-value '?l (pl-solve-all '((length (a b c 1 2 3) ?l)))))))

;;; Make sure IS handles multiple values correctly.
(addtest (gambol-tests)
  multiple-value-is
  (ensure-same
   '(1 2)
   (progn
     (*- (values-test ?a ?b ?l)
         (is ?a ?b (lop (apply values ?l))))
     (get-unified-values '(?a ?b) (pl-solve-all '((values-test ?a ?b (1 2))))))))

;;; Quicksort, translated from lips-test.l
(addtest (gambol-tests)
  quicksort
  (ensure
    (progn
      (*- (partition (?x . ?l) ?y (?x . ?l1) ?l2)
          (lop (< ?x ?y))
          (cut)
          (partition ?l ?y ?l1 ?l2))
      
      (*- (partition (?x . ?l) ?y ?l1 (?x . ?l2))
          (partition ?l ?y ?l1 ?l2))
      
      (*- (partition nil ?QQQQ nil nil))
      
      (*- (qsort (?x . ?l) ?r ?r0)
          (partition ?l ?x ?l1 ?l2)
          (qsort ?l2 ?r1 ?r0)
          (qsort ?l1 ?r (?x . ?r1)))
      
      (*- (qsort nil ?r ?r))
      
      ;;; Produce random list, then compare sorted results.
      (let* ((rands (loop for a from 0 to 100 collecting (random 100)))
             (sorted (sort (copy-seq rands) #'<)))
        (pl-assert `((list100 ,rands)))
        (equalp sorted (get-unified-value '?x (pl-solve-all '((list100 ?l) (qsort ?l ?x nil)))))))))

(addtest (gambol-tests)
  factorial
  (ensure-same
   (labels ((fac (n)
              (if (<= n 1)
                  1
                  (* n (fac (1- n))))))
     (fac 33))
   (progn
      ;;; Factorial: the slow version.
     (*- (factorial 0 1))
     (*- (factorial ?n ?f)
         (lop (> ?n 0))
         (is ?n1 (lop (1- ?n)))
         (factorial ?n1 ?f1)
         (is ?f (lop (* ?n ?f1))))
     (get-unified-value '?f (pl-solve-all '((factorial 33 ?f)))))))


;;; tests.lisp ends here