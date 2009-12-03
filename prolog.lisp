;;; -*- Mode: LISP; Syntax: Common-lisp; Package: gambol; Lowercase: Yes -*-
;;; $Id: prolog.lisp,v 1.1 2008-06-15 16:49:34-05 annis Exp annis $
;;;
;;; See README for copyright information about this modification of FROLIC.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; File:         prolog.l
; Description:  prolog-style search procedure with continuations
; Author:       Jed Krohnfeldt / Craig Steury, University of Utah
; Created:      5-Aug-86
; Language:     Lisp
; Package:      FROLIC
;
; (c) Copyright 1986, 1987, University of Utah, all rights reserved.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This program combines propositional search and unification to achieve a
; predicate-calculus search capability as in Prolog.
; This version of the program implements backtracking and the cut operator
; using a tail-recursive algorithm, implemented with continuations.
; Continuations capture the current state of a computation in such a form
; that it can be restarted at a later time, such as for backtracking to 
; Prolog choice points.
;
; Prolog rules are created with:  (*- head body body ...) - body may be empty
; Prolog queries are posed with:  (?- goal goal ...)
;
; * logical variables are preceeded with a question mark (?)
; * a lisp predicate in the body of a rule is wrapped in (lop (pred))
;   a lisp predicate succeeds if it returns non-nil, and fails otherwise
; * logical variables may be used in lisp predicates provided they are bound
;   by a prolog clause prior to their use - 
;          example (?- (foo ?x) (lop (equal ?x 2)))
; * use pl-solve-one, pl-solve-next, pl-solve-rest and pl-solve-all to
;   return prolog bindings to lisp
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :gambol)


(setf *print-circle* t)
(proclaim '(optimize (speed 3)))

;;; Constants, really.
(defvar *impossible*          'no "make impossible look nice")
(defvar *solved*             'yes "make solved look nice")

;;; Controllable/accessible by the user.
(defvar *tracing*             nil "if t, tracing is turned on")
(defvar *lips*                  0 "logical inferences per second")

;;; Altered in the course of finding solutions.
(defvar *interactive*           t "true iff interacting with user")
(defvar *auto-backtrack*      nil "return all solutions if true")
(defvar *last-continuation*   nil "saved state of the system")
(defvar *trail*               nil "the trail, for backtracking")
(defvar *x-env*               nil "env for goals")
(defvar *y-env*               nil "env for rules")
(defvar *top-level-envs*      nil "saves top-level environments")
(defvar *top-level-vars*      nil "saves top-level variable names")
(defvar *num-slots*            -1 "number of logical variables in a query")
(defvar *prolog-rules*  (make-hash-table) "hash table for prolog rule heads")

;;; Multiple rulebases might be convenient.  In nice lisps, special
;;; variables are thread-safe, to boot.
(defun make-rulebase ()
  (list t nil nil nil nil nil nil nil -1 (make-hash-table)))

(defun current-rulebase ()
  (list *interactive* *auto-backtrack* *last-continuation* *trail*
        *x-env* *y-env* *top-level-envs* *top-level-vars* *num-slots*
        *prolog-rules*))

(defmacro with-rulebase (rulebase &body body)
  `(destructuring-bind (*interactive* *auto-backtrack* *last-continuation*
                        *trail* *x-env* *y-env*
                        *top-level-envs* *top-level-vars*
                        *num-slots* *prolog-rules*) ,rulebase
     ,@body))


;; rule selector functions
(defmacro head (rule)
  `(car ,rule))

(defmacro body (rule)
  `(cdr ,rule))

(defmacro functor (term)
  `(cond ((consp ,term) (car ,term))
	 ((vectorp ,term) (svref ,term 1))
	 (t ,term)))

(defmacro principal-functor (rule)
  `(functor (head ,rule)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Contunuations (vector version - faster than defstructs).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-cont (goals rules level back trail trace)
  `(vector 'cont ,goals ,rules ,level ,back ,trail ,trace))

(defmacro cont-goals (cont)
  `(svref ,cont 1))

(defmacro cont-rules (cont)
  `(svref ,cont 2))

(defmacro cont-level (cont)
  `(svref ,cont 3))

(defmacro cont-back (cont)
  `(svref ,cont 4))

(defmacro cont-trail (cont)
  `(svref ,cont 5))

(defmacro cont-trace (cont)
  `(svref ,cont 6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Molecules - a molecule consists of a skeleton and an environment
; occurences of logical variables in the skeleton point to the environment
; (vector version - faster than defstructs).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-molecule (skel env)
  `(vector 'molecule ,skel ,env))

(defmacro molecule-p (exp)
  `(and (vectorp ,exp)
	(not (stringp ,exp))
	(eq (svref ,exp 0) 'molecule)))

(defmacro mol-skel (molecule)
  `(svref ,molecule 1))

(defmacro mol-env (molecule)
  `(svref ,molecule 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random macros.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Take bindings apart.
(defun lhs (binding)
  (car binding))

(defun rhs (binding)
  (cdr binding))

;; Predicates for variables, atoms, and failure conditions.
(defmacro var-name? (x)
  `(char= #\? (char (symbol-name ,x) 0)))

;; Test for anonymous logical variable - assumes logical variable
(defmacro anon-var-name? (x)
  `(char= #\? (char (symbol-name ,x) 1)))

;; A var looks like (*var* name index env) where name is the logical variable
;; name, and index is the index of the variable's value in the environment
;; (vars are contained in skeletons).
(defmacro var? (x)
  `(and (consp ,x) (eq (car ,x) '*var*)))

(defmacro var-name (x)
  `(cadr ,x))

(defmacro var-index (x)
  `(caddr ,x))

(defmacro var-env (x)
  `(cadddr ,x))

(defmacro anon-var? (x)
  `(string= (var-name ,x) '??))

(defmacro lookup-var (var env)
  `(svref ,env (var-index ,var)))

(defmacro make-empty-environment (size)
  `(make-array ,size :initial-element '*undefined*))

(defmacro impossible? (env)
  `(eq ,env *impossible*))

(defmacro cut? (goal)
  `(and (molecule-p ,goal) (string= (functor (mol-skel ,goal)) 'cut)))

(defmacro unify? (goal)
  `(and (molecule-p ,goal) (string= (functor (mol-skel ,goal)) '=)))

(defmacro level-tag? (goal)
  `(eq ,goal '*level-tag*))

(defmacro pl-bound? (x)
  `(not (eq ,x '*undefined*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Binding and the trail.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Bind logical variable x to y and push it on the trail.
(defun pl-bind (x x-env y y-env)
  (or (anon-var? x)
      (progn
	(push (cons x-env (var-index x)) *trail*)
	(setf (lookup-var x x-env)
	      (if (atom y) y (make-molecule y y-env))))))

;; Undo the trail bindings back to the last choice point (mark).
(defun unbind-var (binding)
  (setf (svref (car binding) (cdr binding)) '*undefined*))

(defun untrail (mark)
  #-PCLS
  (declare (inline unbind-var))
  (loop
     (if (eq *trail* mark)
         (return)
         (unbind-var (pop *trail*)))))

(defmacro rule-head (molecule)
  `(head (mol-skel ,molecule)))

(defmacro rule-body (molecule)
  `(body (mol-skel ,molecule)))

(defmacro rule-env (molecule)
  `(mol-env ,molecule))

(defmacro goal-env (goal)
  `(mol-env ,goal))

(defmacro goal-body (goal)
  `(mol-skel ,goal))

(defmacro goal-functor (goal)
  `(if ,goal (functor (mol-skel ,goal)) ,goal))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Hooks to common lisp.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lisp-hook? (goal)
  (and (consp goal) (string= (car goal) 'lisp)))

(defmacro lisp-hook-mol? (goal)
  `(and (molecule-p ,goal) (lisp-hook? (mol-skel ,goal))))

(defun lisp-query? (goal)
  (and (consp goal) (string= (car goal) 'lop)))

(defmacro lisp-query-mol? (goal)
  `(and (molecule-p ,goal) (lisp-query? (mol-skel ,goal))))

;; Scan the form, replacing all logical variables with their values in the
;; given environment.  The optional variable "query" is true if we are
;; expanding in order to print the top level query solution.  In this case,
;; we don't want to print the internal representation of logical variables.
(defun expand-logical-vars (exp env &optional (query nil))
  (cond ((null exp) nil)
	((var? exp)
	 (if (anon-var? exp)
	   '??
	   ; deref in goal environment
	   (let ((val (x-view-var exp env)))
	     (if (eq val exp)
	       (if query
		 ; pretty-print logical vars
		 (cadr exp)
		 exp)
	       ; use new environment
	       (expand-logical-vars val *x-env* query)))))
	((molecule-p exp) (expand-logical-vars (mol-skel exp) (mol-env exp)))
	((stringp exp) exp)
	((vectorp exp)
         ;; Added because in debugging mode we get different answers
         ;; than in non debugging mode.
         (if query (setq exp (copy-seq exp)))
	 (dotimes (i (length exp) exp)
	   (setf (svref exp i)
		 (expand-logical-vars (svref exp i) env query))))
	((atom exp) exp)
	(t (cons (expand-logical-vars (car exp) env query)
		 (expand-logical-vars (cdr exp) env query)))))

;; Execute a lisp hook form and return the environment handles multiple
;; values returned from the Lisp expression.
(defun do-lisp-hook (molecule)
  (let* ((skel (mol-skel molecule))
	 (env (mol-env molecule))
	 (expanded-form (expand-logical-vars (cadr skel) env)))
    (if (null (apply (symbol-function (first expanded-form))
                     (rest expanded-form)))
      *impossible*
      t)))

(defun get-lisp-hook-values (hook env)
  (let* ((expanded-form (expand-logical-vars (cadr hook) env))
         (values (multiple-value-list
                  (apply (symbol-function (first expanded-form))
                         (rest expanded-form)))))
    (if (member *impossible* values)
        *impossible*
        values)))

;; The IS clause - unification on variables returned from calls to Lisp.
;; The general form is (is ?v1 ... ?vn (lop (lisp-hook))).
;; Binds the ?vi variables to the values returned from (lisp-hook).
(defmacro is? (goal)
  `(and (molecule-p ,goal) (string= (functor (mol-skel ,goal)) 'is)))

(defun do-is (molecule)
  (let ((goal (mol-skel molecule))
	(env (mol-env molecule)) 
	(hook nil)
        (vars nil)
	(retvals nil)
        (return-env t))
    ; collect all of the logical variables
    (dolist (elt (cdr goal))
      (if (lisp-query? elt)
	(setf hook elt)
	(push elt vars)))
    ; run the lisp hook function
    (if (lisp-query? hook)
      (setf retvals (get-lisp-hook-values hook env))
      (error "IS clause must have a LOP hook (~s)" goal))
    ; unify the results with the IS arguments 
    (cond ((member *impossible* retvals)
	   (setf return-env *impossible*))
          ((< (length retvals) (length vars))
	   (error "IS: LOP returns too few values (~s)" goal))
	  (t (dolist (var (nreverse vars))
	       (setf return-env (unify var env (pop retvals) env))
	       (if (impossible? return-env) (return return-env)))))
    return-env))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Continuations - a continuation captures the state of prolog, saving the
; binding environment (env), goal list (goals), rule list (rules),
; unification level (level), the current continuation or choice point (back)
; and the goal trace (trace) for debugging purposes.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Predicates for dealing with functors.
(defmacro get-prolog-rules (functor)
  `(gethash ,functor *prolog-rules*))

(defmacro get-env (functor)
  `(cdr (get ,functor 'environment)))

(defmacro get-env-size (functor)
  `(car (get ,functor 'environment)))

(defmacro set-env (functor env)
  `(setf (get ,functor 'environment) (if ,env (cons (length ,env) ,env))))

(defmacro put-prolog-rules (functor rules)
  `(setf (gethash ,functor *prolog-rules*) ,rules))

(defmacro remove-prolog-rules (functor)
  `(progn
     (remhash ,functor *prolog-rules*)
     (set-env ,functor nil)))

(defmacro all-prolog-rules ()
  `(let ((result nil))
     (maphash #'(lambda (key val)
		  (declare (ignore key))
		  (setf result (append val result))) *prolog-rules*)
     result))

(defmacro remove-all-prolog-rules ()
  `(progn
     (maphash #'(lambda (key val)
		  (declare (ignore val))
		  (set-env key nil)) *prolog-rules*)
     (clrhash *prolog-rules*)))

;; Rule indexing.
(defun index-rule (skeleton num-vars)
  (let ((func (principal-functor skeleton)))
    (if (symbolp func)
      (add-rule-to-index skeleton func num-vars))))

(defun push-rule (skeleton num-vars)
  (let ((func (principal-functor skeleton)))
    (if (symbolp func)
      (push-rule-to-index skeleton func num-vars))))

;; Add a rule to prolog.  Each skeleton is paired with the number of
;; variables in its environment, so new environments can be built easily.
(defun add-rule-to-index (skeleton functor num-vars)
  "add a rule to the end of the database for the functor"
  (put-prolog-rules functor (append (get-prolog-rules functor)
				    (list (cons skeleton num-vars))))
  skeleton)

(defun push-rule-to-index (skeleton functor num-vars)
  "add a rule to the beginning of the database for the functor"
  (put-prolog-rules functor (cons (cons skeleton num-vars)
                                  (get-prolog-rules functor)))
  skeleton)

(defun rule-part (rule-pair) (car rule-pair))
(defun num-vars (rule) (cdr rule))

;; Construct environments for top-level goals.
(defun build-molecule (skeleton)
  (make-molecule (rule-part skeleton)
		  (make-empty-environment (num-vars skeleton))))

(defun make-goals (goals)
  (setf *num-slots* -1)
  (do ((goal-list goals (cdr goal-list))
       (acc-env nil)
       (result nil))
      ((null goal-list)
       (progn
	 (setf *top-level-vars* (nreverse acc-env))
	 (let ((g-env (make-empty-environment (1+ *num-slots*))))
	   (setf *top-level-envs* g-env)
	   (nreverse
	    (mapcar #'(lambda (x)
			(make-molecule x g-env))
		    result)))))
      ;; make goal skeleton
      (let* ((env (list acc-env))
	     (skel (calcify (first goal-list) env)))
	(setf acc-env (car env))
	(push skel result))))


;; Attempt to solve a list of goals with respect to rule-base.
(defun pl-solve (goals)
  (setf *top-level-vars* nil)
  (setf *top-level-envs* nil)
  (setf *trail* nil)
  (pl-search (make-goals goals) 0 nil))

;; Search to solve goals in possible environment.
(defun pl-search (goals level back)
  (search-rules
    goals
    (get-rule-molecules (goal-functor (first goals)))
    level
    back))


;; Called when a goal successfully matched a rule or fact in the database
;; (used for I/O and debugging).
(defun succeed-trace (goal rule back)
  (declare (ignore back))
  (if *tracing*
      (progn
        (format t "Goal: ")
        (pprint (expand-logical-vars (mol-skel goal) (mol-env goal) t))
        (if rule
            (if (molecule-p rule)
                (progn
                  (format t "Rule: (*- ")
                  (pprint (expand-logical-vars (mol-skel rule) (mol-env rule) t)))
                (format t "Fact: ~S~%" rule))))))

(defun succeed-continue (goal goals rule level back)
  ;(declare (ignore goal rule))
  (succeed-trace goal rule back)
  ; pop level tags off top of goal stack and adjust level accordingly
  (loop
     (if (level-tag? (car goals))
         (progn
           (decf level)
           (pop goals))
         (return)))
  (pl-search goals level back))

;; Called when a goal fails to match a rule or fact in the database
;; (used for I/O, debugging).
(defun fail-trace (goal)
  (when *tracing*
    (format t "Goal:")
    (pprint (expand-logical-vars (mol-skel goal) (mol-env goal) t))
    (format t " fails...~%")))

(defmacro fail-continue (goal back)
  `(progn (fail-trace ,goal)
          (continue-on ,back)))

;;; Helpers for SEARCH-RULES:
(defun asserta? (goal)
  (and (molecule-p goal) (string= (functor (mol-skel goal)) 'asserta)))

(defun assertz? (goal)
  (and (molecule-p goal) (string= (functor (mol-skel goal)) 'assertz)))

(defun retract? (goal)
  (and (molecule-p goal) (string= (functor (mol-skel goal)) 'retract)))

(defun fail? (goal)
  (and (molecule-p goal) (string= (functor (mol-skel goal)) 'fail)))

;; Attempt to match a goal against rules in the database.
(defun search-rules (goals rules level back)
  #-PCLS
  (declare (inline succeed-continue))
  (let ((goal (first goals)))
    (cond ((null goals) (succeed back))
	  ; goal is a call to unify (=)
	  ((unify? goal)
	   (if (impossible? (do-unify goal))
               (fail-continue goal back)
               (succeed-continue goal (rest goals) nil level back)))
	  ; goal is a prolog "cut" clause
	  ((cut? goal)
	   (succeed-continue goal (rest goals) nil level (do-cut back level)))
	  ; goal is an "is" clause
	  ((is? goal)
	   (if (impossible? (do-is goal))
               (fail-continue goal back)
               (succeed-continue goal (rest goals) nil level back)))
          ;; adding new data to the database - always succeeds
          ((asserta? goal)
           (pl-asserta (list (expand-logical-vars
                              (second (mol-skel goal))
                              (mol-env goal))))
           (succeed-continue goal (rest goals) nil level back))
          ((assertz? goal)
           (pl-assert (list (expand-logical-vars
                             (second (mol-skel goal))
                             (mol-env goal))))
           (succeed-continue goal (rest goals) nil level back))
          ;; Retract succeeds if it finds something to yank out.
          ((retract? goal)
           ;; FILTER-VARS because you can retract facts with logical
           ;; variables, though not ??.
           (if (not (pl-retract (list (filter-vars
                                       (expand-logical-vars
                                        (second (mol-skel goal))
                                        (mol-env goal))))))
               (fail-continue goal back)
               (succeed-continue goal (rest goals) nil level back)))
          ;;
          ;; Need abolish/1
          ;;
          ((fail? goal)
           (fail-continue goal back))
	  ; goal is a common lisp hook - always succeeds
	  ((lisp-hook-mol? goal)
	   (do-lisp-hook goal)
	   (succeed-continue goal (rest goals) nil level back))
	  ; goal is a common lisp query
	  ((lisp-query-mol? goal)
	   (if (impossible? (do-lisp-hook goal))
               (fail-continue goal back)
               (succeed-continue goal (rest goals) nil level back)))
          ; goal is a variable, check to see if it is bound to a
          ; molecule and if so, try to solve it instead.
          ; RRK -- I am not sure that this is right.  It makes not
          ; seem to work correctly, but it really seems like an
          ; improper fix.  Oh well.  Hope it doesn't break something
          ; else. 
          ((and (molecule-p goal) (var? (mol-skel goal)))
           (let ((var-val (lookup-var (mol-skel goal) (mol-env goal))))
             (if (molecule-p var-val)
                 (pl-search (cons var-val (cdr goals)) level back)
                 (fail-continue goal back))))
	  ; otherwise, goal is a general prolog goal
	  ((match-rule-head goal goals rules level back)))))

;; Match a goal against the pending rules.  NOTE that the result of this
;; function is nconc'ed in match-rule-head.
(defun new-goals (molecule)
  (let ((env (rule-env molecule)))
    (do ((goals (rule-body molecule) (cdr goals))
	 (result nil))
	((null goals) (nreverse result))
      (push (make-molecule (first goals)
			   env)
	    result))))

(defun match-rule-head (goal goals pending-rules level back)
  #-PCLS
  (declare (inline succeed-continue))
  (do ((rules pending-rules (rest rules))
       (old-trail *trail*))
      ((null rules) (fail-continue goal back))
    (if (not (impossible? (unify (goal-body goal) (goal-env goal)
				 (rule-head (first rules))
				 (rule-env (first rules)))))
        (let ((new-goals (new-goals (first rules))))
          (incf *lips*)
          (return
            (succeed-continue
             goal
             (nconc new-goals (cons '*level-tag* (rest goals)))
             (first rules)
             (1+ level)
             (make-cont goals		    ; goals
                        (rest rules)	    ; rules
                        level		    ; level
                        back		    ; back
                        old-trail	    ; trail
                        nil)))))))	    ; trace

;; Continue searching with continuation - used to backtrack to a choice
;; point and continue executing.
(defun continue-on (cont)
  (if (null cont)
    *impossible*
    (if (null (cont-goals cont))
      (continue-on (cont-back cont))
      (progn
	; wrap trail back to last choice point, undoing bindings
	(untrail (cont-trail cont))
	; resume search
	(search-rules (cont-goals cont) (cont-rules cont)
		      (cont-level cont) (cont-back cont))))))

;; Remove alternatives from a continuation - used to strip away pending
;; goals when a cut operator is executed.
(defun remove-alternatives (cont level)
  (if cont
      (make-cont (cont-goals cont)	    ; goals
                 nil			    ; rules
                 level			    ; level 
                 (cont-back cont)	    ; back
                 (cont-trail cont)	    ; trail
                 nil)))			    ; trace

;; Perform a cut operation.
(defun do-cut (cont level)
  (if cont
      (if (= (- level 1) (cont-level cont))
          (remove-alternatives cont level)
          (do-cut (cont-back cont) level))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unification functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Explicit call to unify (= lhs rhs) - unify lhs with rhs.
(defun do-unify (goal)
  (let* ((skel (mol-skel goal))
	 (env (mol-env goal))
	 (lhs (cadr skel))
	 (rhs (caddr skel)))
    (unify (expand-lisp-hooks lhs env) env (expand-lisp-hooks rhs env) env)))

;; If term is a lisp-query, it is evaluated and its result returned; if not,
;; it is simply returned.
(defun expand-lisp-hooks (term env)
  (if (lisp-query? term)
      (let ((expanded-form (expand-logical-vars (cadr term) env)))
        (apply (symbol-function (first expanded-form)) (rest expanded-form)))
      term))

;; Dereference to find ultimate binding of a logical variable in the goal
;; environment.
(defun x-view-var (x env)
  (cond ((var? x)
	 (if (anon-var? x)
             x
             (let ((val (lookup-var x env)))
               (if (pl-bound? val)
                   (x-view-var val env)
                   x))))
	((molecule-p x)
	 (x-view-var (mol-skel x) (setq *x-env* (mol-env x))))
	(t x)))

(defun y-view-var (y env)
  (cond ((var? y)
	 (if (anon-var? y)
             y
             (let ((val (lookup-var y env)))
               (if (pl-bound? val)
                   (y-view-var val env)
                   y))))
	((molecule-p y)
	 (y-view-var (mol-skel y) (setq *y-env* (mol-env y))))
	(t y)))

;; Unify - unification, returns environment in which x and y are unified.
;; Unify sets up environments and trail, and cleans up on failure.
(defun unify (x x-env y y-env)
  (let ((save-trail *trail*) (ans nil))
    (setf *x-env* x-env)			  ;goal environment
    (setf *y-env* y-env)			  ;rule environment
    (if (impossible? (setf ans (unify1 x y))) (untrail save-trail))
    ans))

;; Unify1 dereferences variables in their environments.
(defun unify1 (x y)
  (unify2 (x-view-var x *x-env*) (y-view-var y *y-env*)))

;; Unify2 is the main unification routine.
(defun unify2 (x y)
  #-PCLS
  (declare (inline pl-bind))
  (cond ((var? x) (pl-bind x *x-env* y *y-env*))
        ; bind variables if distinct
        ((var? y) (pl-bind y *y-env* x *x-env*))
        ; unify atoms 
        ((atom x) (if (equalp x y) t *impossible*))
        ((atom y) *impossible*)
        ; both terms complex
        ((let ((x-env *x-env*)
	       (y-env *y-env*))
	   (if (impossible? (unify1 (car x) (car y)))
               *impossible*
               (progn
                 (setf *x-env* x-env)
                 (setf *y-env* y-env)
                 (unify1 (cdr x) (cdr y))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Success and failure display functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Success - report solutions to user.
(defun succeed (back)
  (when *interactive*
    (show-bindings *top-level-vars*
		   *top-level-envs*))
  (setf *last-continuation* back)
  ; query the user if more
  (if (or *auto-backtrack* (and *interactive* (y-or-n-p "More? ")))
    ; force failure to get more solutions
      (let* ((save-binding-list
              (build-binding-list *top-level-vars* *top-level-envs*))
             (ans (continue-on back)))
        (if (impossible? ans)
            (if *interactive* 
                ans
                (list save-binding-list))
            (append (list save-binding-list)
                    ans)))
      (if (not *interactive*) 
          (build-binding-list *top-level-vars* *top-level-envs*))))

;; Build a list of the bindings.
(defun build-binding-list (vars env)
  (let ((result nil))
    (mapc #'(lambda (x)
	      (push (cons (car x) (expand-logical-vars (cdr x) env t)) result))
	  vars)
    (if result (nreverse result) t)))

;; Show result bindings (bindings of goal variables).
(defun show-bindings (vars envs)
  (let ((bindings (build-binding-list vars envs)))
    (terpri)
    (if (atom bindings)
      (format t "~s~%" *solved*)
      (mapc #'show-one-binding bindings))))

(defun show-one-binding (binding)
  (format t "~s = ~s~%" (lhs binding) (rhs binding)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Molecule and skeleton building forms - for rules.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-rule-molecules (functor)
  (let ((skeletons (get-prolog-rules functor)))
    (mapcar #'build-molecule skeletons)))

(defmacro add-new-var (var vars)
  `(append ,vars (list (cons ,var (incf *num-slots*)))))


;;; Calcify returns form with all logical variables replaced with a vectorized
;;; representation.  env is destructively modified.  It is expected to be input
;;; in the form ((env-a-list)).  Note that the input environment is not
;;; necessarily nil, as in the case when a series of input goals are calcified
;;; and must share an environment.
(defun calcify (form alist)
  (cond ((null form) nil)
	((symbolp form)
         (if (var-name? form)
             (if (anon-var-name? form)
                 '(*var* ?? -1)
                 (let ((slot (assoc form (car alist))))
                   (if (not slot)
                       (let ((nv `(,form . (*var* ,form ,(incf *num-slots*)))))
                         ; destructively modify alist
                         (push nv (car alist))
                         (setf slot nv)))
		   ; return new rep for var
                   (cdr slot)))
             form))
	((stringp form) form)
	((vectorp form)
	 (dotimes (i (length form) form)
	   (setf (svref form i) (calcify (svref form i) alist))))
	((atom form) form)
	(t (cons (calcify (car form) alist)
		 (calcify (cdr form) alist)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Assert and solve - lisp calls to prolog.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add a rule to the database.

;; Destructively modify a rule to produce a skeleton rule.  Each logical
;; variable is converted to a pointer into the environment.
;; Calcify returns the environment structure created for the rule.
(defun pl-assert (rule)
  "add a rule to the end of the database for this functor"
  (let ((env (list nil)))
    (let ((*num-slots* -1))
      (index-rule (calcify rule env) (1+ *num-slots*))
      rule)))

(defun pl-asserta (rule)
  "add a rule to the beginning of the database for this functor"
  (let ((env (list nil)))
    (let ((*num-slots* -1))
      (push-rule (calcify rule env) (1+ *num-slots*))
      rule)))

;; Makes sure lisp gets nil on failure instead of *impossible*.
(defmacro filter-no (value)
  `(let ((retval ,value))
     (if (impossible? retval) nil retval)))

;; Return the first solution to the query, setting *last-continuation* so
;; that subsequent calls to solve-next can get other solutions - the return
;; value is an environment, an alist with (var . binding) pairs.
(defun pl-solve-one (goals)
  (setf *interactive* nil)
  (setf *auto-backtrack* nil)
  (filter-no (pl-solve goals)))

;; Return the next solution, using *last-continuation* (the continuation
;; from the most recent pl-solve-one or pl-solve-cc) or the optional
;; continuation provided.
(defun pl-solve-next (&optional (cont *last-continuation*))
  (setf *interactive* nil)
  (setf *auto-backtrack* nil)
  (filter-no (continue-on cont)))

;; Return the rest of the solutions, using *last-continuation* (the
;; continuation from the most recent pl-solve-one or pl-solve-cc) or the
;; optional continuation provided.
(defun pl-solve-rest (&optional (cont *last-continuation*))
  (setf *interactive* nil)
  (setf *auto-backtrack* t)
  (filter-no (continue-on cont)))

;; Return all solutions to the query - the return value is a list of
;; environments (env env ...) where each environment is a
;; ((var . binding)...) alist.
(defun pl-solve-all (goals)
  (let ((*interactive* nil)
        (*auto-backtrack* t))
    (filter-no (pl-solve goals))))

;;; (do-solve-all (?who) '((mortal ?who))
;;;   (print ?who))
(defmacro do-solve-all ((&rest vars) goals &body body)
  (let* ((env (gensym "ENV"))
         (binding-list (loop for var in vars
                             collecting (list var `(cdr (assoc ',var ,env))))))
    `(dolist (,env (pl-solve-all ,goals))
       (let ,binding-list
         ,@body))))

;;; "I didn't mean that."  The next three functions handle the delicate
;;; matter of rule retraction.  It is at least as robust as some commercial
;;; prologs, in that this doesn't fiddle with any facts currently in the
;;; backtracking chain.  It's not smart enough to retract rules, only facts.

;;; Take the results of PL-SOLVE-ONE and produce a version of the query
;;; filling in the logical variables with the association list.
(defun subst-alist (alist tree &key (test #'eql))
  (let ((new-tree tree))
    (loop for (old . new) in alist
          do (setf new-tree (subst new old new-tree :test test))
          finally (return new-tree))))

(defun retract-fact (fact)
  ;; facts can be: ((mortal ?x)) but also (fred-exists)
  (let* ((functor (if (atom (first fact))
                      (first fact)
                      (caar fact)))
         (functor-rules (get-prolog-rules functor)))
    ;; make variables look good and yank out arity data to help REMOVE-IF
    (labels ((rule-filter (rule)
               (first (filter-vars rule))))
      (when functor-rules
        (put-prolog-rules functor (remove-if #'(lambda (r) (equalp r fact))
                                             functor-rules
                                             :key #'rule-filter))))))

(defun pl-retract (goals)
  ;; Paranoia - since retraction will run during on-going solution searches,
  ;; we have to protect all the specials involved in that search while we do
  ;; a new search for the rule to retract.
  (let* ((*interactive* nil)
         (*auto-backtrack* nil)
         (*last-continuation* nil)
         (*trail* nil)
         (*x-env* nil)
         (*y-env* nil)
         (*top-level-envs* nil)
         (*top-level-vars* nil)
         (clause (filter-no (pl-solve goals))))
    (cond ((null clause) nil)  ; no match
          ((eq clause t)       ; literal match
           (retract-fact goals)
           t)
          ((listp clause)      ; unified match
           (retract-fact (subst-alist clause goals))
           t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; User interaction.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Interactive version of assert
;; (used to be called :- but common lisp thinks :- is a keyword).
(defmacro *- (&rest rule)
  `(pl-assert ',rule))

;; Interactive version of pl-solve-one.
(defmacro ?- (&rest goals)
  `(progn
     (setf *interactive* t)
     (setf *auto-backtrack* nil)
     (pl-solve ',goals)))

;; Interactive version of pl-solve-all.
(defmacro ??- (&rest goals)
  `(progn
     (setf *interactive* t)
     (setf *auto-backtrack* t)
     (pl-solve ',goals)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Rule and database manipulation and printing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remove all rules from the database.
(defun clear-rules (&optional (functors nil))
  (if functors
      (dolist (functor functors)
        (remove-prolog-rules functor))
      ; clear rule index
      (remove-all-prolog-rules))
  t)

;; Print all rules, or rules with the given principal functor.
(defun print-rules (&optional (functors nil))
  (if (null functors)
      (maphash #'(lambda (key value)
                   (declare (ignore value))
                   (print-rule key))
               *prolog-rules*)
      (dolist (functor functors)
        (print-rule functor))))

(defun print-rule (functor)
  (let ((rules (get-prolog-rules functor)))
    (when rules
      (format t "~s:~%" functor)
      (dolist (rule rules)
	(pp-rule (car rule))))))

;; Prettyprint a single rule.
(defun pp-rule (rule)
  (let ((head (head rule))
	(body (body rule)))
    (if body
        (progn
          (format t "  (*- ~s" (filter-vars head))
          (dolist (form body)
            (format t "~%~6t~s" (filter-vars form)))
          (format t ")~%"))
        (format t "  (*- ~s)~%" (filter-vars head)))))

;; Change (*var* ?x 0) to ?x for rule printing.
(defun filter-vars (exp)
  (cond ((null exp) nil)
	((var? exp) (cadr exp))
	((stringp exp) exp)
	((vectorp exp)
         (setf exp (copy-seq exp))
	 (dotimes (i (length exp) exp)
	   (setf (svref exp i) (filter-vars (svref exp i)))))
	((atom exp) exp)
	(t (cons (filter-vars (car exp))
		 (filter-vars (cdr exp))))))

;; end of prolog.lisp
