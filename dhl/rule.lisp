;;;-*-Lisp-*-
;;:vi:set lisp ai
(include declar)

(declare (macros t))
;;
;; Rule Processor and Interpretor.
;;
;;
;;  What follows are special macros used to create rules, rule-domains,
;;	and domains.  The reason these macros should be used instead
;;	of fassert, is that a function needs to be created when
;;	a rule or domain frame is created,
;;	but this can not be done as an if-added on the instance or ako
;;	slot since this will be triggered before all of the frame is
;;	created, and the entire frame needs to exist before the function
;;	can be created correctly.  So these macros call fassert to 
;;	create the frame and then create the appropriate function.
;;
;; NOTE for compiling rules and rule domains:  To compile a file with
;;	rules and rule-domains in it, you must first load the file into
;;	the environment, and then call the compiler on the file, in the
;;	same environment.  This
;;	is because the compiler needs to have to rule and domain frames
;;	existing in order to produce the correct code for the appropriate
;;	associated functions.  This note can be ignored if one is not
;;	wishing to compile their rules and/or domains.
;;
;; NOTE:  If the slots of a rule or domain change, you can check these
;;	macros to see what functions you should run to update information
;;	if any.  
;;
;; NOTE : the macros rule, domain, and rule-domain have a special syntax
;;	as noted, and will erase an existing frame with the same name
;;	before creating a new one.
;;	the macros fassert-rule, fassert-domain, and fassert-rule-domain
;;	take a syntax like fassert, and will also erase the current frame.
;;	Both sets of functions create the appropriate functions after the
;;	frame has been created.
;;
;;	(rule) => creates a rule frame.
;;
;;	(rule name ako domain 
;;	      attributes)
;;
;;		possible attributes => (action a)
;;			      (condition b)
;;			      (fail c)
;;
;;
(defun declare-rule-frame (name ako domain frame)
  `(fassert ,name
	    (ako ($value (,ako)))
	    ,@(mapcar (function
			(lambda (y)
			  `(,(car y) ($value (,@(cdr y))))))
		      frame)
	    (domain ($value . ,(cond ((atom domain)
				      (list (list domain)))
				     (domain))))))

(defmacro rule (name ako domain . frame)
  (ferase name)
  ;;
  ;;  need information to process define-apply-function correctly below.
  ;; thus things must be declared at compile time before the rule
  ;; is proceessed by the compiler.
  ;;
  (eval (declare-rule-frame name ako domain frame))
  `(progn 'compile
	  (ferase ',name)
	  ,(declare-rule-frame name ako domain frame)
	  ,@(define-apply-function name 'rule-function
	      (*fvalue-only domain 'apply-rule-function-arguments)
	      (*fvalue-only domain 'apply-rule-function-definition))))

(defun define-apply-function (name type vars defs)
  (cond (type
	 (and defs
	      `((fput ',name ',type '$value ',name)
		(defun ,name ,vars . ,(mapcar '(lambda (form)
						       (funcall form name))
					      (copy defs))))))
	(defs (mapcar '(lambda (form) (funcall form name))
		      (copy defs)))))
;
;; a new function which is more compatible with the fassert syntax
;; 
;; to use this one, ako, and domain must be specified slots in the frame.
;; syntax is the same as with fassert.
;;
;; (fassert-rule (ako ($value (rule))) 
;;		 (domain ($value (domain)))
;;		 (condition ($value (..........))))
;;
;;  
;; this function does not erase the frame it creates.
;;

(defmacro fassert-rule (name . frame)
  (ferase name)
  (eval `(fassert ,name . ,frame))
  `(progn 'compile
	  (ferase ',name)
	  (fassert ,name . ,frame)
	  ,@(let ((domain (fvalue-only name 'domain)))
	      (define-apply-function name 'rule-function
		(*fvalue-only domain 'apply-rule-function-arguments)
		(*fvalue-only domain 'apply-rule-function-definition)))))

;;
;;	rule domains and knowledge domains have similar syntax.
;;		(see later for rule domains which is slightly different.)
;;
;;	(domain name ako domain (arguments)
;;		attributes)
;;
;;	(rule-domain) => creates a rule domain frame.
;;		rule domains have special attributes used,
;;		some are specific to only rule-domains.
;;
;; 	(rule-domain name ako domain (arguments) attributes)
;;
;;
;;	attributes in a domain are usually of the form
;;		(slot function-value)
;;		
;;	Some simple system functions are provided for rule
;;		domains in the file dhl//rule2.l
;;		The user is always free to write his own.
;;
;;	necessary attributes for standard rule domains-
;;		(applyrule basic-applyrule)
;;
;;	optional attributes for standard rule domains defined by the system-
;;		(putrule pushrule)
;;		(getrule poprule)
;;		(newgoal mark-goals-function)
;;		(initialtest tell-if-marked-function)
;;		(succeed unmark-goals-function) 
;;		(fail basicfail)
;;		(entertrace print-rule-trace)
;;		(exittrace print-exit-rule-trace)
;;
;; 	default attributes for standard rule domains defined by the system-
;;	(what is used if the user leaves unspecified).
;;		(putrule putrule)  {enter entire new gather list 
;;					at front of queue}
;;		(getrule dequeue)  {take first element of front of queue}
;;		(gather gather-all-rules) {return list of all rules in domain}
;;
;;	Only the arguments and applyrule slots need
;;		to be defined for each domain.  Everything else is now
;;		optional, or has a default.
;; 	Applyrule can be replaced by using the (defapply (domainname rule) ...)
;;		function instead.
;;	See dhl//match.l and demo//tower.l demo/btower.l demo/atn.l demo//oatn.l
;;		for examples.
;;		
;;

(defmacro domain (name ako domain arguments . frame)
  `(progn 'compile
	  (ferase ',name)
	  (fassert ,name
		   (ako ($value (,ako)))
		   ,@(mapcar (function
			       (lambda (y)
				 `(,(car y) ($value (,@(cdr y))))))
			     frame)
		   ,@(cond (domain
			    `((domain ($value . ,(cond ((atom domain)
							(list (list domain)))
						       (domain)))))))
		   (arguments ($value (,arguments))))))

;;
;; fassert-domain is the same as fassert
;;
(defmacro fassert-domain (name . frame)
  `(progn 'compile
	  (ferase ',name)
	  `(fassert ,name . ,frame)))
	
(defmacro rule-domain (name ako domain arguments . frame)
  (eval `(domain ,name ,ako ,domain ,arguments . ,frame))
  `(progn 'compile
;;	  (declare (special . ,(cond ((atom arguments)
;;				      (list arguments))
;;				     (arguments))))
	  (domain ,name ,ako ,domain ,arguments . ,frame)
	  ,@(define-apply-function name 'domain-function
	      arguments
	      (fvalue-only name 'apply-domain-function-definition))))

;;
;; fassert-rule-domain : similar syntax to fassert and fassert-rule
;;
;;
(defmacro fassert-rule-domain (name . frame)
  (ferase name)
  (eval `(fassert ,name . ,frame))
  `(progn 'compile
	  (ferase ',name)
	  (fassert ,name . ,frame)
	  ,@(define-apply-function name 'domain-function
	      (*fvalue-only name 'arguments)
	      (fvalue-only name 'apply-domain-function-definition))))

;;
;; defapply -- how to define a function which will tell how
;;	to execute a rule or a rule-domain
;;
;; syntax : (defapply (domain type) vars . body)
;;	where domain is the name of the domain.
;;	body is a list of functions which when ever an entity of
;;	type 'type' is created in this domain, or as a child of
;;	this domain, will be applied to the newly created frame,
;;	and will return the body of a function which will be
;;	how one can execute the new rule or new domain.
;;
;;	vars are used to define what the variables for the function will
;;	be at run time. (see example below).
;;
;;	in the special case of type domain, vars is ignored, and is
;;	defined when the domain is created.
;;
;; several special slots are used with this function.

(defmacro defapply ((domain name) vars . body)
  `(eval-when (compile load eval)
	      (cond ((getd 'frame)
		     (fput ',domain ',(concat 'apply- name '-function-arguments)
			   '$value ',vars)
		     (fput ',domain 
			   ',(concat 'apply- name '-function-definition)
			   '$value ',body)))))

;;
;; defapplyrule -- a special early form of defapply for rules.
;;	name is ignored.
;;

(defmacro defapplyrule ((domain name) vars . body)
  (cond ((null (eq (length vars)
		   (length (*fvalue-only domain 'arguments))))
	 (princ "warning wrong number of arguments to ")
	 (princ (concat domain "-" name))
	 (terpri)))
  `(eval-when (compile load eval)
	  (cond ((getd 'frame)
		 (fput ',domain 'apply-rule-function-arguments
		       '$value ',vars)
		 (fput ',domain 'apply-rule-function-definition 
		       '$value ',body)))))

#+lispm
(defmacro compile-rule-file (file)
  `(qc-file-load ',file))

#+franz
(defmacro compile-rule-file (file)
  `(let ()
     (liszt -m ,file)
     (load ',file)))

;;
;; here we define a default standard package for interpreting rule-domains.
;;
;;
;; defapply is used to define how children of domains are defined.
;;    this function can be used by the user in a similar way to
;;	create a family of domains which are interpreted differently.
;;
  
(defapply (rule-domain domain) (ignore)
  defun-rule-domain)

(defvar ruledomain nil)
(defvar rulequeue nil)
(defvar rulelevel 0)
(declare (special *rule-level))
(declare (special rule-debug))
(defvar close-compile nil)	;; if t, compile code for rules inside
				;; code for domain-> thus all rules and
				;; have been declared.  Use function
				;; (compile-rule-domain domain) to
				;; set this variable.

;;
;; Function for close compiling standard rule domains.  This
;;	should be rewritten later to handle other kinds of
;;	rule domains when possible.  But to date, all rule
;;	domains are the standard rule domains defined by the
;;	functions below.
;;
;; put the line 
;; (compile-rule-domain domain-name)
;; at the end of a file before compiling it, to close-compile the domain.
;; all rules and frames involved must be already defined.
;;
(defmacro compile-rule-domain (domain-name)
  (let ((close-compile t)
	(rule-debug nil))
       `(progn 'compile
	       ,@(define-apply-function
		  domain-name 'domain-function
		  (*fvalue-only domain-name 'arguments)
		  (fvalue-only domain-name
			       'apply-domain-function-definition)))))

;;
;; Below are macros which help define the rule domain function. 
;;      this is done when-ever a rule-domain is created or the
;;	function defun-rule-domain is called.
;;
;;  notes:
;;	bind rulequeue so that it will be reset upon leaving.
;;	rule and goals are set only if called by other rules (nil if
;;	prove was called by the user and are used in setting up the
;;	sentinels if prove fails.)
;;	Eventually this function should be replaced with rules which
;;	can do the appropriate thing.
;;

;;
;;	Basic macros and expansion functions
;;		for rule interpretor control structure.
;;
;;
(Defun defun-rule-domain (name)
  (special rulequeue ruledomain :rulestack :goalstack
	   $rulesucceed $rulecount
	   rulelevel *rule-level)
  (let ((arguments (*fvalue-only name 'arguments)))
       (cond ((and (atom arguments)
		   arguments)
	      `((lambda (,arguments)
			,(changedomain name arguments))
		(listify ,arguments)))
	     (t (changedomain name arguments)))))

;;
;;	changedomain - run in a new domain on the given goals.
;;
;;	note about rulequeue - default setting, is a new rulequeue is
;;	created for each new level, except in the case when a ruledomain
;;	calls itself, in which case the old rulequeue is used.
;;      The default setting has the rulequeue dynamically scoped.
;;	Using the scope slot of a ruledomain can override these defaults.
;;
;;	scope -- close - new rulequeue for a domain when called by itself.
;;		 open - use previous rulequeue when called by other domains.
;;		 lexical - closed and rulequeue is scoped lexically,
;;			it is not seen by domains called.(they would see
;;			the rulequeue of the domain calling this domain.),
;;			as if this domain had no rulequeue as far as the
;;			rest of the world is concerned.
;;

(defun changedomain (newruledomain goals)
  `(let ((rulelevel (1+ rulelevel))
	 ,@(and (null (memq (fvalue-only newruledomain 'scope) 
			    '(lexical open close)))
		`((rulequeue (and (eq ',newruledomain ruledomain)
				  rulequeue))))
	 (ruledomain ',newruledomain))
	,(let ((i (initialtest-clause newruledomain goals))
	       (n (newgoal-clause newruledomain goals))
	       (r (runrules-clause newruledomain goals))
	       (f (fail-clause newruledomain goals)))
	      (cond ((or i n f)
		     `(or ,@i
			  ;; Newgoal should always returns nil.
			  ;; unless you want to use it to
			  ;; return a value and exit.
			  ;; returning non-nil = success.
			  ;; without running assert or
			  ;; succeed functions.
			  ,@n
			  ,r
			  ;; Fail should always return nil also.
			  ,@f))
		    (t r)))))

(defun initialtest-clause (domain goals)
  (let ((x (fvalue-only domain 'initialtest)))
    (cond (x `((,x . ,goals) nil)))))

(defun newgoal-clause (domain goals)
  (let ((x (fvalue-only domain 'newgoal)))
    (cond (x `((,x . ,goals))))))

(defun fail-clause (domain goals)
  (let ((f (fvalue-only domain 'fail)))
    (cond (f `((,f . ,goals))))))

(defun succeed-clause (domain result goals)
  (let ((s (fvalue-only domain 'succeed)))
    (cond (s `(apply ',s (append ,goals (list ,result))))
	  (t result))))

(defun entertrace-clause (domain rule level goals)
  (let ((entertrace (fvalue-only domain 'entertrace)))
    (cond (entertrace
	   `((,entertrace ,rule ,level . ,goals))))))

(defun exittrace-clause (domain rule level result)
  (let ((exittrace (fvalue-only domain 'exittrace)))
    (cond (exittrace
	   `(,exittrace ,rule ,level ,result)))))

;;
;; System Functions for handling the rule queue.
;;	The user can specify how the rule queue works by providing
;;	the right functions and attributes to the rule domain,
;;	some functions for simple stacks and queues are provided
;;

(defun getrule-clause (domain rq)
  `(,(fvalue-only domain 'getrule) ,rq))

(defmacro make-getrule-element (rule goal-list level)
  `(list ,rule ,goal-list ,level))

(defmacro rule-getrule (r)
  `(caar ,r))

(defmacro goals-getrule (r)
  `(cadar ,r))

(defmacro level-getrule (r)
  `(caddar ,r))


;;	:rulestack and :goalstack are two global FRL system variables
;;		which contain the current stack of goals and rules
;;		which are used by the system, any user procedure
;;		or function or rule can access these variables.
;;		the most recent goal and rule are on the top of
;;		the stack.  These varaibles are maintained by
;;		the system prove loop (see runrules below).
;;
;;	they are only set if rule-debug is not set to nil.
;;

(defvar :rulestack nil)
(defvar :goalstack nil)
(defvar $rulecount 0)
(defvar $rulesucceed 0)
(defvar rule-debug t)	;; if t -> use the above variables.
			;; if nil -> dont set above variables.
			;; (compile-rule-domain domain) sets this to nil.

;;
;; runrules-clause - this will write the code of the main
;;	loop which gathers the rulequeue then proceeds to
;;	execute through each rule onto the rulequeue.
;;	until one succeeds or the rulequeue is empty.
;;

(defun runrules-clause (domain goals)
  (let ((scope (fvalue-only domain 'scope))
	(sc (fvalue-only domain 'succeedcontinue)))
       (cond ((and close-compile
		   (eq scope 'lexical)
		   (eq 'get-all-things (fvalue-only domain 'gather))
		   (null (fvalues domain 'putrule))
		   (null (fvalues domain 'getrule)))
	      (cond ((null sc)
		     (consecutive-rules domain))
		    (t (all-consecutive-rules domain))))
	     (t (let ((record-name (cond ((fvalues domain 'getrule) 'record)
					 ((eq scope 'lexical) 'irulequeue)
					 (t 'rulequeue)))
		      (rulequeue-name (cond ((eq scope 'lexical) 'irulequeue)
					    (t 'rulequeue))))
		     `(*catch rulelevel 
			      ,(runrules-body record-name rulequeue-name 
					      scope sc domain goals)))))))

;;
;; Close -compile case for default gather, putrule, and getrule.
;;
(defun consecutive-rules (domain)
  (close-rules domain '(or) nil))

(defun all-consecutive-rules (domain)
  (close-rules domain '(append) nil))

(defun select-close-rules (domain rule-name)
  (close-rules domain `(selectq ,rule-name) t))

(defun close-rules (domain func-name individual-tag)
  (let ((rule-args (fvalue-only domain 'apply-rule-function-arguments))
	(dom-args (fvalue-only domain 'arguments)))
       (cond ((equal rule-args dom-args)
	      `(,@func-name
		,@(close-rule-list domain individual-tag)))
	     (t 
	      `(let
		,(mapcan '(lambda (x y) (and (null (eq x y))
					     `((,x ,y))))
			 rule-args dom-args)
		(,@func-name
		 ,@(close-rule-list domain individual-tag)))))))

(defun close-rule-list (domain individual-tag)
  (mapcan
   '(lambda (x)
	    (let ((y (define-apply-function
		      x nil nil
		      (fvalue-only domain
				   'apply-rule-function-definition))))
	    (cond (individual-tag `((,x . ,y)))
		  (t y))))
   (fvalues-only domain 'rule)))

;;
;; Must write code for other more common cases. 
;;

(defun runrules-body (record-name rulequeue-name scope sc domain goals)
  `(do (,@(and (eq record-name 'record) '((record)))
	  (,rulequeue-name ,(set-up-rule-queue
			     domain goals
			     (cond ((memq scope '(close lexical)) nil)
				   (t 'rulequeue)))
			   ,@(and (eq scope 'lexical)
				  `((cdr ,record-name))))
	  ,@(cond ((null (eq scope 'lexical))
		   '((nextrule) (nextgoal) (*rule-level)))
		  (t `((*rule-level rulelevel)
		       (nextgoal (list . ,goals)))))
	  (result) ,@(and sc '((resultlist))))
       ((null ,rulequeue-name)
	,@(and sc (succeed-clause domain 'resultlist goals)))
       ,@(and (eq record-name 'record)
	      `((setq record ,(getrule-clause domain rulequeue-name))))
       ,@(and (null (eq scope 'lexical))
	      `((setq nextgoal (goals-getrule ,record-name)
		      nextrule (rule-getrule ,record-name)
		      *rule-level (level-getrule ,record-name)
		      ,rulequeue-name (cdr ,record-name))))
       (and ,(cond (rule-debug
		    (applyrule-clause
		     domain (cond ((eq scope 'lexical) `(car ,record-name))
				  (t 'nextrule))
		     '*rule-level 'nextgoal))
		   (t `(setq result
			     ,(applyrule-clause domain 
						(cond ((eq scope 'lexical)
						       `(car ,record-name))
						      (t 'nextrule))
						'*rule-level 'nextgoal))))
	    ,(cond (sc '(setq resultlist
			      (append resultlist (list result))))
		   ((eq scope 'lexical) `(return ,(succeed-clause
						   domain 'result
						   `(list . ,goals))))
		   (t `(*throw *rule-level
			       ,(succeed-clause
				 domain 'result
				 'nextgoal)))))))

;;
;; applyrule-clause - writes the code that will call a specific rule
;;	that has been chosen with the correct arguments, uses
;;	the applyrule function if it exists, else it assumes the
;;	rule is a functions (that (defapply (domain rule)..) was used
;;	to define how to interpret rules for the given domain.)
;;

;;
;;  if selectq-rules-format is true (compile-rule-domain domain) will cause
;;     all rules to be compiled in line in one selectq instead of being called
;;     as a function, at this point.
;;
  
(defun applyrule-clause (domain r level goal)
  (cond	(rule-debug
	 `(let ((:rulestack (cons ,r :rulestack))
		(:goalstack (cons ,goal :goalstack)))
	       ,@(entertrace-clause domain r level goal)
	       (setq $rulecount (1+ $rulecount))
	       (and (setq result ,(simpleapplyrule-clause domain r goal))
		    (setq $rulesucceed (1+ $rulesucceed)))
	       ,@(exittrace-clause domain r level 'result)
	       result))
	(t (simpleapplyrule-clause domain r goal))))

(defun simpleapplyrule-clause (domain r goal)
  (let ((arguments (fvalue-only domain 'arguments))
	(applyfunc (fvalue-only domain 'applyrule))
	(scope (fvalue-only domain 'scope)))
       (cond ((eq scope 'lexical)
	      (cond (applyfunc `(,applyfunc ,r ,@arguments))
		    (t `(funcall ,r ,@arguments))))
	     (applyfunc
	      `(apply ',applyfunc (cons ,r ,goal)))
	     (t `(apply ,r ,goal)))))

;;
;;  Below is functions which write the code that initializes the
;;	rulequeue, based on the previously rules, certain optimizations
;;	are made.  If no value of the putrule slot is given by the user,
;;	Defaults are built into the code to just take the
;;	rules and push them onto the front of the queue.
;;
;;
;;  This should all be rewritten to allow streams and generators.
;;

(defun set-up-rule-queue (newruledomain goals oldrulequeue)
  (cond ((fvalue-only newruledomain 'putrule)
	 `(do ((rules ,(gather-rules-clause newruledomain goals)
		      (cdr rules))
	       (gl (list . ,goals))
	       (rq ,oldrulequeue
		   ,(putrule-clause newruledomain '(car rules)
				    'gl 'rulelevel 'rq)))
	      ((null rules) rq)))
	((null oldrulequeue)
	 (cond ((eq (fvalue-only newruledomain 'scope) 'lexical)
		(gather-rules-clause newruledomain goals))
	       (t (mapgatherrules newruledomain goals))))
	(t `(append ,(mapgatherrules newruledomain goals) ,oldrulequeue))))

(defun mapgatherrules (newruledomain goals)
  `(let ((gl (list . ,goals)))
	(mapcar '(lambda (x)
			 (make-getrule-element x gl rulelevel))
		,(gather-rules-clause newruledomain goals))))

(defun putrule-clause (domain rule goals level rq)
  `(,(fvalue-only domain 'putrule) ,rq 
     (make-getrule-element ,rule ,goals ,level)))

(defun gather-rules-clause (domain goals)
  `(,(fvalue-only domain 'gather) ',domain . ,goals))

;;
;;	ruledomain-throw - throws one out of the current domain with
;;		the given result.  Overrides all succeed functions, and
;;		continue-succeed flags.  If the return value is
;;		nil, then the goal of the rule-domain failed, and
;;		the appropriate fail function will be executed.
;;
(defun ruledomain-throw (result)
  (*throw *rule-level result))

;;
;;
;;	newsym - a nice version of newsym.  for creating
;;		new frame names.
;;

(defmacro newsym (word)
  `(do ((n (1+ (cond ((get ,word 'newsym-index))
		     (0)))
	   (1+ n)))
       ((or (null (getd 'frame/?))
	    (null (frame/? (concat ,word '- n))))
	(putprop ,word n 'newsym-index)
	(concat ,word '- n))))

;;
;;	newframe -- create a new frame of a given type.
;;

(defmacro newframe (type)
  `(eval (list 'fassert 
	       (newsym ,type)
	       (list 'ako (list '$value (list ,type))))))

;;
;; the following was previously the file dhl//rule2.l
;;
;;-*-Lisp-*-
;;:vi:set lisp ai
;;
;;	Auxiliary Rule file for standard rule domains.
;;		Contains sets of functions which the user
;;		can use in standard rule-domains.
;;
;;
;;	Basic queue control functions.
;;
;;	The first set make the rule domain dequeue a stack.
;;	attributes-
;;  		(getrule poprule)
;;  		(putrule pushrule)
;;  		(gather possiblerules)
;;

(defmacro poprule (stack)
  stack)

(defmacro pushrule (stack rule)
  `(cons ,rule ,stack))

(defmacro possiblerules (goal)
  `(*rvalues-only ruledomain 'rule))

;;
;;	The second set make the rule domain dequeue a queue.
;;
;;	attributes-
;;  		(getrule enqueue)
;;	  	(putrule dequeue)
;;		(gather possiblerules)
;;

(defmacro enqueue (queue rule)
  `(append ,queue (list ,rule)))

(defmacro dequeue (queue)
  queue)

(defmacro rhs (arule)
  `(*rvalue-only ,arule 'rhs))

(defmacro lhs (arule)
  `(*rvalue-only ,arule 'lhs))

(defmacro condition (arule)
  `(*rvalue-only ,arule 'condition))

(defmacro action (arule)
  `(*rvalue-only ,arule 'action))

(defmacro trace-domain (domain)
  `(trace ,domain ,@(fvalues-only domain 'rule)))

(defun erase-domain (domain)
  (mapc 'ferase (fdescendants domain 'rule))
  (ferase domain))


