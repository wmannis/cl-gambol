Gambol is basic logic programming system written in pure Lisp, using Lisp-like syntax.  Most of it will seem familiar to people who know Prolog, but apart from core functionality it is nowhere near a complete system — i.e., it doesn't provide the mass of built-in operations and predicates of an ISO-compliant Prolog.  The semantics of some operations (like `retract`, for example) differ from a compliant Prolog, too.

Gambol is heavy conser at the moment.

## The Package ##

The package name is **`:gambol`**.  There are no nicknames.

It depends on no other libraries.

Note that because Gambol is just working with ordinary Lisp symbols most of the time you have to keep in mind what package you define facts and rules in.  Except for core operators (`is`, `assertz`, etc.) you'll have to state the package on terms if you define rules in a different package from where you'll be querying the database.


## Logic Programming ##

Like Prolog, Gambol deals with facts, rules, procedures and queries.

**Facts** are represented so:

```
(*- (mortal socrates))
(*- (eat cats mice))
```

The **`*-`** macro adds new facts and rules to the database.  A predicate may have an arbitrary number of arguments.

**Rules** are represented so:

> (**`*-`** head b<sub>1</sub> b<sub>2</sub> ... b<sub>n</sub>)

Where the _b<sub>n</sub>_ terms are called the _body_ of the rule.  Within rules (and facts) you can use logical variables.  These are simply lisp symbols that start with a question mark.  This represents, "if something is human, it is mortal" —

```
(*- (mortal ?x) (human ?x))
```

Notice that a fact is just a degenerate sort of rule which is always true.

If you don't care about the term in a particular rule you can use **`??`** as a special placeholder variable.

**Procedures** are collections of rules which all have the same predicate:

```
(*- (append (?x . ?xs) ?ys (?x . ?zs))
    (append ?xs ?ys ?zs))
(*- (append nil ?ys ?ys))
```

Notice that the Gambol unifier understands Lisp lists.  Where Prolog uses **`|`** to separate the head from the rest of the list, Gambol uses the usual Lispy dotted-pair notation.

Interactive **queries** are called with one of the **`?-`** or **`??-`** macros.  **`?-`** will present one answer after the other, asking you if you wish to continue after each.  **`??-`** will find all solutions immediately.

None of the macros presented so far gives you a way to pass Lisp values into the database of facts and rules.  See the API section below for function calls to interact with the database.

### Special Predicates and Operators ###

**`(cut)`**
> Is equivalent to the Prolog cut (**`!`**) operator.

**`(fail)`**
> is an immediate fail.  Combined with `(cut)` this can be used to implement the usual Prolog
> [negation as failure](http://en.wikipedia.org/wiki/Negation_as_failure):

```
(*- (not ?p)
    ?p
    (cut)
    (fail))
```

**`(= ?x ?x)`**
> Performs unification.

**`(asserta ?p)`**
> Adds a new fact to the _head_ of the database.  `pl-asserta` is the function equivalent.  Always succeeds.

**`(assertz ?p)`**
> Adds a new fact to the _end_ of the database, as though via the **`*-`** macro `pl-assert`.  These three
> Lisp expressions will accomplish the same thing:

```
(*- (fibonacci 0 1))
(?- (assert (fibonacci 0 1)))
(pl-assert '((fibonacci 0 1)))
```

> Always succeeds.

**`(retract ?p)`**
> removes a _fact_ from the database.  It is not currently smart enough to remove rules.  It will retract a fact that
> matches perfectly, or, if logical variables are in the expression, the _first_ solution found.  If it finds a fact to
> remove, it succeeds.  If it finds nothing, it fails.

**`(is`** _`logical-variable(s) (lop (lisp-expression))`_**`)`**
> binds one or more logical variables to the result of a lisp expression:

```
GAMBOL> (*- (1- ?n ?result)
            (is ?result (lop (1- ?n))))
((1- ?N ?RESULT) (IS ?RESULT (LOP (1- ?N))))
GAMBOL> (??- (1- 3 ?result))

?RESULT = 2
```

> Several logical variables may come between `is` and the lisp expression if that lisp expression will return
> multiple values.

**`(lop (lisp-expression))`**
> allows Gambol to invoke arbitrary Lisp functions.  Any logical variables in the expression will be correctly
> substituted before the function is evaluated.  Note that the lisp expression _must_ be an applyable function,
> not a macro or special form.  If the lisp expression returns non-nil the term is counted as a success.

```
GAMBOL> (*- (greater-than ?x ?y) (lop (> ?x ?y)))
((GREATER-THAN ?X ?Y) (LOP (> ?X ?Y)))
GAMBOL> (??- (greater-than 5 7))
NO
GAMBOL> (??- (greater-than 7 5))
YES
```

**`(lisp (lisp-expression))`**
> evaluates the lisp expression, but the term always counts as a success.  This is used to get Lisp side-effects:

```
GAMBOL> (*- (write ?x) (lisp (format t "~A" ?x)))
((WRITE ?X) (LISP (FORMAT T "~A" ?X)))
GAMBOL> (?- (write (tasty pizza)))
(TASTY PIZZA)
YES
More?  (y or n) n

NIL
```

## API ##

_special variable_
**`*TRACING*`**
> if set to `t` will cause queries to print out trace information.

_special variable_
**`*LIPS*`**
> keeps track of how many logical inferences have been performed.  You reset this and combine it with timing
> output to assess how quickly Gambol's doing its work.

_macro_
**`*-`** _`rule`_
> adds new rules and facts to the database.  They are inserted in the order they arrive.

_macro_
**`?-`** _`goal`_
> interactively tries to solve the goal, asking you if you want to continue after each solution found.

_macro_
**`??-`** _`goal`_
> immediately finds all solutions to the goal.

_function_
**`pl-assert`** _`rule`_
> is the function counterpart to the `*-` macro.  The rule must therefore be quoted (or constructed).  Also, keep
> in mind that a fact is a rule missing a body.  This results in extra list embedding:

```
(pl-assert '((mortal socrates)))         ; same as: (*- (mortal socrates))
(pl-assert '((mortal ?x) (human ?x)))    ; same as: (*- (mortal ?x) (human ?x))
```

_function_
**`pl-asserta`** _`rule`_
> adds a new rule to the database.  If the predicate already exists the new rule goes at the _head_ of the list
> of rules for that predicate.

_function_
**`pl-retract`** _`rule`_
> removes a fact from the database.  The fact may be literal or contain logical variables.  In all cases, it only removes
> the first matching fact it finds.  Like `pl-assert`, take care with the list form:

```
(pl-retract '((mortal socrates)))
```

_function_
**`pl-solve-one`** _`goals`_
> returns the first set of bindings (an association list) which satisfies the goal.  If there are no variables necessary
> to satisfy the goal, then `t` is simply returned.  `nil` is returned if the goal cannot be satisfied.

_function_
**`pl-solve-next`**
> returns the next set of bindings which satisfy the goal set in the last `pl-solve-one` call.

_function_
**`pl-solve-rest`**
> returns a list of bindings of the remaining solutions to the last `pl-solve-one` call.

_function_
**`pl-solve-all`** _`goals`_
> immediately returns all bindings that satisfy the goal.

_macro_
**`do-solve-all`** _`(bindings) goals`_
> is a convenience wrapper around `pl-solve-all` which iterates over all the solutions.  The bindings should be the
> logical variables you're interested in.  These will be successively bound to the values returned by `pl-solve-all`:

```
GAMBOL> (do-solve-all (?who) '((mortal ?who))
          (format t "~A is mortal~%" ?who))
SOCRATES is mortal
GERTRUDE.STEIN is mortal
NIL
GAMBOL> 
```

_function_
**`clear-rules`** _`&optional (predicates nil)`_
> with no arguments empties the rules database.  Or you may specify a list of predicates to clear.

_function_
**`print-rules`** _`&optional (predicates nil)`_
> with no arguments prints out all rules.  Or you may specify a list of predicates.

_function_
**`print-rule`** _`predicate`_
> prints all the rules for predicate.

### Multiple Rulebases ###

As of the 0.03 release it is possible to have multiple, independent rulebases.

_function_
**`current-rulebase`**
> returns a reference to the current rulebase.  This is useful if you want to keep the current global rulebase before wiping it.

_function_
**`make-rulebase`**
> creates a new, blank rulebase.

_macro_
**`with-rulebase`** _`rulebase &body body`_
> all prolog operations in the body will work with the given rulebase instead of the default.  Because Gambol's rulebase and state are contained in a bunch of special variables, you can nest this macro safely.  Given a blank default rulebase, this should return `nil`:
```
   (let ((default-rulebase (current-rulebase)))
     (with-rulebase (make-rulebase)
       (*- (fibonacci 0 1))
       (*- (fibonacci 1 1))
       (with-rulebase default-rulebase
         (pl-solve-one '((fibonacci 0 1))))))
```