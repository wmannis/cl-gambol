### Sum all the Numbers in a List ###

```
(*- (sumlist () 0))
(*- (sumlist (?head . ?tail) ?n)
    (sumlist ?tail ?n1)
    (is ?n (lop (+ ?n1 ?head))))

(?- (sumlist (1 2 3 4 5) ?n))
```

### Towers of Hanoi ###

```
(*- (move 1 ?x ?y ??)
    (lisp (format t "Move top disk from ~A to ~A.~%" '?x '?y)))
(*- (move ?n ?x ?y ?z)
    (lop (> ?n 1))
    (is ?m (lop (- ?n 1)))
    (move ?m ?x ?z ?y)
    (move 1 ?x ?y ??)
    (move ?m ?z ?y ?x))

(?- (move 3 left right center))
```

### Fibonacci Numbers: Blow your Stack ###

A naive implementation —

```
(*- (fib 0 1))
(*- (fib 1 1))
(*- (fib ?n ?f)
    (lop (> ?n 1))
    (is ?n1 (lop (1- ?n)))
    (is ?n2 (lop (- ?n 2)))
    (fib ?n1 ?f1)
    (fib ?n2 ?f2)
    (is ?f (lop (+ ?f1 ?f2))))

(?- (fib 10 ?f))
```

### Fibonacci Numbers: Memoization ###

This one adds a new fact to the database for each new integer for which it finds the fibonacci number.

```
(*- (is-fib 0 1))
(*- (is-fib 1 1))

(*- (fib ?n ?f)
    (is-fib ?n ?f)
    (cut))
(*- (fib ?n ?f)
    (is ?n1 (lop (- ?n 1)))
    (is ?n2 (lop (- ?n 2)))
    (fib ?n1 ?f1)
    (fib ?n2 ?f2)
    (is ?f (lop (+ ?f1 ?f2)))
    (assertz (is-fib ?n ?f)))
```