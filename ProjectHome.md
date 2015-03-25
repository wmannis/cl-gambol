Gambol provides basic propositional unification and search capabilities for Common Lisp.  The code is a modification of an older library from the CMU AI repository, [Frolic](http://www.cs.cmu.edu/afs/cs.cmu.edu/project/ai-repository/ai/lang/prolog/impl/prolog/frolic/).

I have removed from Frolic some code specific to the Cray, as well as references to the "Frobs" system which gives Frolic its name.  See AlterationsFromFrolic for all the changes I've made.

A small taste:

```
GAMBOL> (*- (mortal ?x) (human ?x))
((MORTAL ?X) (HUMAN ?X))
GAMBOL> (*- (human ?x) (man ?x))
((HUMAN ?X) (MAN ?X))
GAMBOL> (*- (human ?x) (woman ?x))
((HUMAN ?X) (WOMAN ?X))
GAMBOL> (*- (woman gertrude.stein))
((WOMAN GERTRUDE.STEIN))
GAMBOL> (*- (man socrates))
((MAN SOCRATES))
GAMBOL> (do-solve-all (?who) '((mortal ?who))
          (format t "~A is mortal~%" ?who))
SOCRATES is mortal
GERTRUDE.STEIN is mortal
NIL
GAMBOL> 
```

You can drop out to Lisp functions, `LOP` for operators you care about the results for, `LISP` for side effects with no truth value to worry about.

```
GAMBOL> (*- (length nil 0))
((LENGTH NIL 0))
GAMBOL> (*- (length (?? . ?t) ?n)
            (length ?t ?n1)
            (is ?n (lop (1+ ?n1))))
((LENGTH (?? . ?T) ?N) (LENGTH ?T ?N1) (IS ?N (LOP (1+ ?N1))))
GAMBOL> (??- (length (a b c 1 2 3) ?l))

?L = 6
NO
GAMBOL> 
```

See the [API](API.md) documentation for further details, and [Examples](Examples.md) for a few common Prolog examples in Gambol.

Third release (0.03) on December 5, 2009.  Changes from 0.02:
  * removed code designed to coddle an implementation of lisp almost no one uses (and isn't even Common Lisp, to boot)
  * more minor style adjustments
  * made it possible to work with multiple rulebases

Second release (0.02) on July 10, 2008.  Changes from 0.01:
  * some irritations with symbol packages in the operators dealt with (i.e., you don't have to state the package on `is`)
  * added `pl-asserta` to add rules to the head of the database
  * made `pl-assert` and `pl-asserta` available to the Gambol language with Prologesque names `asserta` and `assertz`.
  * added `pl-retract` to the Lisp API and `retract` to Gambol
  * `(fail)` is now a hard-coded part of Gambol, so you can't accidentally declare it to be true