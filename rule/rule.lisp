From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:10:40 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08216; Thu, 2 Jun 88 14:10:39 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04429; Thu, 2 Jun 88 13:34:25 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08174; Thu, 2 Jun 88 12:46:30+0900
Date: Thu, 2 Jun 88 12:46:30+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020346.AA08174@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: rule.l.frl
Status: RO


(include declar)
;;; A context provides an environment for rules and goals to operate
;;; in. Rules are active in a context. This context might be some
;;; portion of the real world database. However, it can also be a 
;;; particular context used to express goals and information relevant
;;; to a particular problem. Goals are handled in frames based reasoning
;;; by treating them as the assertion of information into a context.
;;; Rules which can handle those goals need to have triggers in that
;;; context. Each context has a list of rules relevant to that context.
;;; These rules do not necessarily have to be active, but can be activated.
;;; Activation means that the rule frame is interpreted under the state
;;; "rule" (other forms such as demon, if-ever, etc. in the future/?)
;;; causing the corresponding sentinel to be run. This causes the active
;;; rule to place triggers in the context. 

;;; This scheme allows consequent reasoning. It allows contexts of information
;;; and rules to be active. In turn, this allows metaknowledge to manipulate
;;; these contexts, allowing simple control structures through the control
;;; of hierarchy.

;;; When a goal succeeds, the asserted information is removed. The active goals
;;; can be examined, although currently, one cannot follow the problem solving
;;; process directly. The contexts function as blackboards, but ones which
;;; are highly semanticly organized. They are not buffers, in the sense that
;;; noting gets erased, and there is no size limit, as everything has a place.
;;; corresponding sentinel.
;;; Thus pattern matching either against a database of assertions (as in
;;; Planner), or against a database of rules (as in Production system rule
;;; stacks), is not required. The same result is obtained by using contexts
;;; and semantic organization to allow all rules access to locations where
;;; relevent information is contained. The use of contexts and semantic
;;; organization allows control of the process.

(setq *context nil)

(fassert rule
         (ako ($value (thing)))
         (context)
         (type)
         (variables)
         (condition)
         (action))

(fassert context
         (ako ($value (thing)))
         (rules))

(defun context () (show *context))

;; The goal function searches for a context for a goal and asserts
;; the goal into that context if it exists.

(defun goal (x y)
   (cond ((fget *context x)
          (space 2)
          (shout [asserting goal into default context @*context])
          (space 2)
          (shout [GOAL/: @x @y])
          (space 2)
          (shout [@(context)])
          (terpri)
          (fput *context x '$value y)) 
         (t (shout [searching for a context for this goal])
            (for (/:context
                  (exists z (fdescendants 'context 'instance)
                           (fget z x)))
                 (cond
                  (/:context
                   (space 2)
                   (shout [asserting goal in context/:  @/:context])
                   (space 2)
                   (setq *context /:context)
                   (shout [@(context)])
                   (fput /:context x '$value y)
                   (shout [setting default context to current context]))
                  (t (shout [ no context found for this goal])))))))

;; Goals and simple assertions of information into acontext (such as
;; asserting the fact that newark is short of oil) are effectively the
;; same thing. I have set up the goal function to check the default
;; context and if it is not correct search for one. 


;;; All goals are simple statements, as in/: Goal/:Increase Oil-Supply.
;;; The rules themselves can have complex conditions containing
;;; multiple goals, as in/: If (goal/: increase oil-supply) and
;;; (goal/:keep price under $35) then ..... 

(fassert oil-supply-context
     (ako ($value (context)))
     (oil-shortage)
     (price ($value (30)))
     (limit ($value (32)))
     (oil-supply)
     (spot-market)
     (rules ($value (oil-supply-increase) (buy-oil))))

(defun activate (x)
      (foreach y (fvalues-only x 'rules)
               (run-rule y)))

;;; The above frame provides a context for reasoning about changes in 
;;; oil-supply. The slot "oil-shortage" contains the site which is short
;;; of oil. This information is asserted into the frame by whatever
;;; sentinel notices the shortage. It is necessary as rules will need to
;;; where to send the oil they find, and how much oil they must acquire.

;;; *Should the assertion of information into a context result in its
;;; rules being activated/?*


(fassert oil-supply-increase
     (ako ($value (rule)))
     (context ($value (oil-supply-context)))
     (type ($value (when-ever)))
     (condition ($value ((and ('oil-supply-context 'oil-shortage t)
                              ('oil-supply-context 'oil-supply 'increase))))
                ($describe ((shout [oil-shortage and (goal/: increase oil-supply)]))))
     (action ($value ((and (rdescribe 'oil-supply-increase)
                           (or (goal 'spot-market 'buy)
                               (goal 'oil-users 'share)))))
             ($describe ((shout [or (goal/: buy on spot market)
                              (goal/: share shortage among users)])))))


(fassert buy-oil
     (ako ($value (rule)))
     (context ($value (oil-supply-context)))
     (type ($value (when-ever)))
     (condition ($value ((and ('oil-supply-context 'oil-shortage t)
                              ('oil-supply-context 'spot-market 'buy))))
                ($describe ((shout [oil-shortage and (goal/: buy oil)]))))
     (action ($value ((and (rdescribe 'buy-oil)
                          (cond ((lessp (price 'oil) (price 'limit))
                                 (buy 'oil)
                                 (erase *context 'oil-shortage)
                                 (erase *context 'spot-market))))))
             ($describe ((shout [if price < limit buy oil])))))

(defun buy (x)
   (shout [simulating determining shortfall/,  buying oil and shipping
it]))

(defun erase (x y)
   (fremove-slot x y))

(defun price (x)
       (fvalue-only 'oil-supply-context x))

;;; To get rules to describe themselves, since the rule name is not
;;; passed on to the activated sentinel, is made an explicit function
;;; of writing a rule, just as goal erasure and multiple context slot
;;; values must be explicitly controlled through rules. Thus, each rule
;;; action has a part (rdescribe <rule>) where describe is given the
;;; rule name as argument.

(defun rdescribe (rule)
       (space 2)
       (shout 'CONDITION/:) 
       (terpri)
       (describe-condition rule)
       (terpri)
       (shout '===>)
       (shout 'ACTION/:)
       (terpri) 
       (describe-action rule))

(defun describe-condition (x)
      (cond ((eval (caar (fget x 'condition '$describe))))
            (t (fget x 'condition '$value))))

(defun describe-action (x)
      (cond ((eval (caar (fget x 'action '$describe))))
            (t (fget x 'action '$value))))
             
;;; Initially goals will never be erased. However, I must address this point,
;;; as normally I would like the success of a rule to cause the goal which
;;; invoked that rule to be erased. Right now, I envision that a rule will
;;; always be active in a context once it is activated. Hence the rule
;;; succeeding won't necessarily cause its triggers to be erased. Thus
;;; to cause this there are two options/: the goal function which asserts
;;; goals watches to see if a goal succeeds, and erases that goal; or
;;; the rule using things like the :frame-sentinel variable binds its
;;; goals and erases them upon success. Neither of these seems elegant.
;;; It would seem that I am building another level of interpretation onto
;;; sentinels and I should step back and rethink it a bit.

;;; However, goals always replace, so they do erase existing goals. Right
;;; now knowledge is incrementally added so there may be possible confusion
;;; if, say, more than one place suffers from an oil shortage.

;;; I need a context function that is used to assert information or an
;;; active goal into a context. I'd like this function to activate the
;;; context, and then deactivate it after the goal succeeds.

;;; Thus the assertion of information into a context can cause rules to
;;; be activated.

;;; I will need to determine how to allow for contexts in the rule 
;;; specification and run-rule functions, and others of this type.
;;; (e.g. make-rule, etc.0
;;; One solution is to have the system find the context. One problem is that
;;; a rule could be active in more than one context.

;;; should I have global variables /:context, /:goal, and so on so the context
;;; doesn't have to be specified and the goal is known/?

;;; Then the goal function should have a default context into which to assert
;;; goals.

;;; I need criterion for deactivating contexts.

;; issues/: 
;; a) describe function in rules, which, if verbose switch on, do something
;;    like/: (space 2)
;;          (shout Condition/: @show-condition 
;;                 ==>
;;                 Action/: @show-action
;; @show-condition and action use the :frame-sentinel variable to find
;; the condtions and actions. Or have them find the rule itself by
;; putting in a pointer to it. also, describe should print out the
;; rule type.
;; b) variables
;; c) contexts and their use.
;; d) goal erasure
;; e) metaknowledge
;; f) goal conflict
;; g) bugs in current system.


;;; Have a noticing context for invoking noticing of shortages.
;;; also complete the oil-supply context so it is at least as good as
;;; my example.
;;; Finally have a context for driving the simulation.
;;; look at the wheat sim. for use in driving the context, and have the
;;; constraint part of any simulation be explicit.

;; As a first solution, I believe it should be the responsibility of
;; the rules to know about multiple values in slots; when to fadd
;; and when to freplace. 

;; One solution to the goal erasure problem is to have explicit
;; goal removal. Thus the sentinel that reports oil shortage can
;; know when to remove this alert; its part of the sentinel. The
;; rules can also enable sentinels or triggers which explicitly
;; know when to remove relevent goals.

;; To run the example, first activate the context. Then assert the
;; location of the shortage. Finally give the goal.

