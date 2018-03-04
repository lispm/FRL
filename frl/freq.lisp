(include declar)
;;; (version)
;;; (declare (pa-readtable))
;;;************************************************************************
;;;		              CONSTRAINT CHECKING
;;;************************************************************************

;; There is an implicit "and" relation over multiple entries in the
;; $REQUIRE and $PREFER slots.  Constraints are currently inherited from the
;; entire heritage of frames, although one usually assumes constraints higher in
;; the AKO hierarchy are more general than those below.  A project still
;; remaining is a language to guide the merging of properties inherited from
;; several frames.

;; Slots may have multiple values, and several properties under the $REQUIRE
;; key.  At present, do means exists to pair some constraints with potential
;; values.  For semantic reasons, however, some slots are intended to have but a
;; single value.  It is not the prerogative of the Frame system to know this.
;; Consequently, the issue of applying constraints to all the values of a slot,
;; or of mapping the requirements over the properties is left to the $REQUIRE
;; properties.  By convention, /:VALUE and /:VALUES can be used in $REQUIRE
;; properties to refer respectively to the value (if there is but one) and list
;; of values.  Hence, any functions which purport to evaluate requirements have
;; the responsibility for establishing this environment, as well as the proper
;; frame (in /:FRAME) slot (in /:SLOT) and facet (/:FACET).


(defun FCHECK args
       ;; (FCHECK frame slot {values})
       ;;
       ;; returns a POLL representing the consistency of the value(s) of the slot
       ;; or those optionally supplied.
       ;; It applies the constraints assigned to the slot /:SLOT of frame /:FRAME.
       ;; /:FRAME, /:SLOT and /:FACET provide the default environment for any references to other
       ;; slot values occurring in the constraints.
       ;; The free variables /:VALUE and /:VALUES are bound as well.
       ;; The entire heritage of predicates is used.
       (for (:frame (arg 1)   :slot (arg 2)   :facet '$require
	     values (cond ((> args 2)(arg 3)) (t (fvalues-only (arg 1) (arg 2)))))
        (fapply-constraints (fdata-heritage-only :frame :slot :facet) values)))

;; >>> Record consistency check on the property itself.  May need to add
;; information like the time as well so can later tell when necessary to
;; recompute the check.  Any process that needs the check done should notice
;; when one already exists and use it.  A serious problem with this is
;; maintaining the support for a given check such that one can know when it is
;; obsolete.<<<

(defun FCHECKPREFERENCES args
       ;; (FCHECKPREFERENCES frame slot {values})
       ;;
       ;; >> Differs from FCHECK only in the use of the $PREFER facet instead of $REQUIRE. <<
       (for (:frame (arg 1)   :slot (arg 2)   :facet '$PREFER
	     values (cond ((> args 2)(arg 3)) (t (fvalues-only (arg 1) (arg 2)))))
        (fapply-constraints (fdata-heritage-only :frame :slot :facet) values)))

(defun FAPPLY-CONSTRAINTS (predicates valuelist)
       ;; Predicates is a list of predicates.
       ;; Valuelist is a list of values which will be bound to /:VALUES while
       ;; each predicates is being evaluated.  /:VALUE is bound
       ;; to NIL; except if Valuelist has but one value, /:VALUE will be bound
       ;; to this.   If Valuelist (the second argument) is atomic, it is bound to /:VALUE
       ;; and /:VALUES is set to (list /:VALUE).
       (prog (:value :values /:v /:vs)
             (cond((atom valuelist) (setq :value valuelist)
                                 (setq :values (list :value)))
                  (t (setq :values valuelist)
                     (ifnot (cdr :values)
			    (setq :value (car :values)))))
	     (setq /:v :value /:vs :values)		       ; A convenience.
             (return (fpoll predicates))))

(defun FAPPLY-CONSTRAINT (Predicate Values)
       (fapply-constraints (list predicate) values))

(defun FAPPLY-CONSTRAINTS1 (Predicates Value)
       ;; Like FAPPLY-CONSTRAINTS except accepts a single value.
       (prog (:value :values /:v /:vs)
         (setq :value value :values (list value)
	       /:v :value /:vs :values)			; A convenience.
             (return (fpoll predicates))))

(defun FAPPLY-CONSTRAINT1 (Predicate Value)
       (fapply-constraints1 (list predicate) value))

;;; Just like the previous four functions except do not do a POLL, but only a summary.

(defun FAPPLY-CONSTRAINTS-Summary (predicates valuelist)
       (prog (:value :values /:v /:vs)
             (cond((atom valuelist) (setq :value valuelist)
                                 (setq :values (list :value)))
                  (t (setq :values valuelist)
                     (ifnot (cdr :values)
			    (setq :value (car :values)))))
	     (setq /:v :value /:vs :values)
             (return (fpoll-summary predicates))))

(defun FAPPLY-CONSTRAINT-Summary (Predicate Values)
       (fapply-constraints-summary (list predicate) values))

(defun FAPPLY-CONSTRAINTS1-Summary (Predicates Value)
       (prog (:value :values /:v /:vs)
         (setq :value value :values (list value)
	       /:v :value /:vs :values)
             (return (fpoll-summary predicates))))

(defun FAPPLY-CONSTRAINT1-Summary (Predicate Value)
       (fapply-constraints1-summary (list predicate) value))

(defun FCLASSIFY-VALUES (Predicate Values)
       ;; Returns (<summary> (t <True>) (nil <false>) (/? <error>))
       ;;  which classifies Values according to Predicate.  The predicate must refer
       ;;  to the value by /:VALUE (/:VALUES is set to (list /:VALUE)).
       (do ((:values)
	    (:value    (car values) (car remaining))
	    (remaining (cdr values) (cdr remaining))
	    (truelist)(falselist)(errorlist))
	   ((null :value) `(,(COND((and (null falselist)(null errorlist)) 'T)
				  ((null errorlist) 'NIL)
				  (t '/?))
			    (T ,truelist)(NIL ,falselist)(/? ,errorlist)))
	   (setq :values (list :value))
	   (caseq (fpoll1 predicate)
	     (/?   (push :value Errorlist))
	     (NIL (push :value Falselist))
	     ; T must be last, since acts as "otherwise" in CASEQ statement.
	     (T   (push :value Truelist)))))

(defun FCLASSIFY-VALUE (predicate :value)
       (for (:values (list :value))
        (fpoll1 predicate)))


;;;             	Utility functions for predicates


;;; THREE-VALUED LOGIC TESTS.

(declare (special /?))
(setq /? '/?)		; use /? like T and NIL

(defun TRUE/? (x)
       (not (memq x '(nil /?))))

(defun FALSE/? (x)
       (null x))

(defun UNKNOWN/? (x)
       (eq x '/?))


;; POLL does not know anything about frames.  It is solely concerned with
;; constructing a POLL-DATATYPE out of the list of predicates given it.
;; Any environment must be set up outside of the call to POLL.
;;; The POLL-DATATYPE looks like/: 
;;;                (<summary> <list of true predicates>
;;;                           <list of false predicates>
;;;                           <list of error-producing predicates>)
;;; where the SUMMARY is T iff all are true (none /?, none NIL),
;;;                      NIL iff any are false and none produce errors, or
;;;                      /? otherwise.

(defun FPOLL ( predicates ) 
   (DO ((predicate (car predicates) (car remaining))
        (remaining (cdr predicates) (cdr remaining))
        (TRUELIST)
        (FALSELIST)
        (ERRORLIST)) 
       ((NULL predicate) `(,(COND((and (null falselist)(null errorlist)) T)
				 ((null errorlist) NIL)
				 (t '/?))
			   (T ,TRUELIST) (NIL ,FALSELIST) (/? ,ERRORLIST)))
       (caseq (fpoll1 predicate)
	 (NIL (push predicate falselist))
	 (/?   (push predicate errorlist))
	 (T   (push predicate truelist)))))
;; Do not use FRUN instead of EVAL in order to allow Flags as constraints.


(defun FPOLL-SUMMARY ( predicates ) 
   ;; Like FPOLL except computes the summary only and doesn't produce a POLL-DATATYPE.
   (DO ((preds predicates (cdr preds)) (FALSES) (ERRORS)) 
       ((NULL preds) (COND((and (null falses)(null errors)) T)
			  ((null errors) NIL)
			  (t '/?)))
       (ifnot (errset (COND ((EVAL (car preds)))
			    ((setq falses t)))
		      nil)
	      (setq errors t))))

(defun FPOLL1 (predicate)
       ;; Returns T, NIL or /? upon evaluating Predicate.
       (for (result (errset (eval predicate) nil))
	(cond ((null result) '/?)			; Can't compute.
	      ((null (car result)) NIL)			; NIL
	      (T))))

(declare (special *say*))
(or (boundp '*say*) (setq *say* T))

(defun DISCUSS-POLL (poll)
       (prog (falsehoods errors)

        (setq falsehoods  (cadr (assq nil poll)))
        (setq errors (cadr (assq '/? poll)))

        (cond((not (null falsehoods))
	      (cond (*say*				; NOTE/: Flag to enable Saying!
		     (printc '|The following constraints are currently violated/:|)
		     (describe-constraints falsehoods))
		    (t
		     (printc '|The following constraints are violated/:|)
		     (princ-list falsehoods)))))

        (cond((not (null errors))
	      (cond (*say*)
		    (t
		     (printc '|The following constraints are not yet computable/:|)
		     (princ-list errors)))))
        (terpri)

        (return poll)))


(defun DESCRIBE-CONSTRAINTS (constraints)
       ;; Prints constraints as prose using SAY generator.
       ;; Requires use of SAY package; in PA;SAY FASL.
       (if constraints
	   (do ((out (say-generate (car constraints))
		     (nconc out (say-generate (car rest))))
		(rest (cdr constraints) (cdr rest)))
	       ((cond ((null rest) (shout1 out) t)	; end test succeeds
		      (t (setq out (nconc out (ncons '/;)))       nil))))))

(defun DESCRIBE-CONSTRAINT-STRUCTURES (constraint-structures)
       (describe-constraints (fextract-data constraint-structures)))

;;;		        REQUIREMENT VOCABULARY

;(defun SINGLE-VALUE-ONLY ()
;       ;; An $If-Added procedure to INSURE that a slot contains only the value most
;       ;; recently added.
;       (do-foreach old-p (delete :value (*fdata-only :frame :slot '$value))
;            (if *verbose* (shout (list '|Removing old value from the| :slot
;				       '|slot of| (fname :frame) '|...|)))
;            (fremove-value :frame :slot old-p)))


