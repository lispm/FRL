(include declar)


 ;; THIS SENTIN FILE CONTAINS THE VARIOUS TYPES OF SENTINELS.

;; This file requires the functions MYINSTANTIATE, MAKE-FRAME, and
;; ADD-TO-FRAME from SUTIL to work.

 ; Sentinels should be able to 
 ; serve as a basis for a problem solving language and a query language.
 ; (A) single sentinels, (B) OR sentinels, (C) AND sentinels, (D) the ability
 ; to set several sentinels in one statement, since all clauses in a sentinel
 ; statement are evaluated. The different sentinels will know about each other
 ; although they have different bodies and conditions. This is useful, 
 ; since the user need not worry about cleaning up uneeded sentinels. For 
 ; instance, in my TMS system, sentinels to monitor several aspects of a rule's
 ; environment are set in one statement. Regardless of which succeeds, all 
 ; others are removed automaticly. If set separately, would have to explicitly
 ; create the group links that are used. (E) A sentinel can either remove 
 ; itself after success or stay. This is equivalent to the difference between
 ; ALL and ANY. (F) Similarly, a sentinel's sensors have the same option. This
 ; is the difference between a sequential AND, and one in which all parts are
 ; true at the final time. This allows ANDs where a value exceeded some 
 ; criterion temporarily. e.g. ; oil demand for june dipped below x, and final
 ; supply was y. (G) Sentinels can test for their criterion on creation. (H)
 ; They should come in if-removed, and  if-changed flavours. (I) There should
 ; be an AND where all clauses are in a single frame. This is called "in". 
 ; (J) The body can choose to erase the sentinel or a sensor. 
 ; Things to check/: IN/AND combinations, and particularly whether
 ; can embed  clauses/: (Sentinel () ((AND (in () () . .)(and () () ) (body).
 ; Any problems with heritage of if-addeds. Use of variable list at beginning 
 ; of sentinel.

 ;; If have a group of rules for maintenence
 ;; linking other rules to this group will automaticly give maintenence. Thus 
 ;; this gives a practical way to have self-reflexive or meta-rules operate on
 ;; each other. i.e. using the group slot to give rules links to each other,
 ;; and augment the class of actions from erasure, to things like support,
 ;; and rewritting. 
 ;; Thus by linking a rule to a maintenence group, it gets interpreted as
 ;; TMS, by the additional action of the rules which augment the interpretation
 ;; of the original.

; <Sentinel> if-ever always removes self.
;            when-ever sensors stay; set removed, so know remains true.
;            for-ever tests existing values when added; sensors stay.
;            if-never TRIGGERED BY REMOVAL. sensors stay.
;            rule causes a rule to be run once. i.e. it checks for existence
;                 of the conditions, but does not expect them.
;            if-added causes an if-added to be created. it is identical
;                 to if-ever, except the syntactic expression is tailored
;                 for if-addeds.
;            if-removed causes an if-removed to be created. It is identical
;                 to if-never, except the syntactic expression is tailored
;                 for if-removeds.

 ;; THE VARIOUS FRAME SLOTS, BESIDES POINTING TO THE GROUP, SENSOR,
 ;; OR SENTINEL, ALSO CONTAIN/: BODY/: BODY OF SENTINEL; TYPE/: TYPE OF
 ;; SENTINEL; PLACE/: INSTANCE SENSOR IS IN; TEST/: TEST ON VALUE;
 ;; SET/: INDICATES WHETHER THE SENTINEL OR SENSOR IS WINNING OR LOSING 
 ;; CURRENTLY.


 ;; THE SENTINEL SYNTAX IS/:
 ;; (SENTINEL TYPE (VARS)
 ;;           ((FRAME SLOT TEST)    ; CONDITION OF CLAUSE
 ;;            (BODY) (BODY))       ; ACTION OF CLAUSE
 ;;           (      )              ; NEW CLAUSE
 ;;
 ;;
 ;; (SENTINEL TYPE (VARS)           ; AND/OR FORM
 ;;           ((AND
 ;;              (FRAME SLOT TEST)
 ;;              (FRAME SLOT TEST))
 ;;            (BODY) (BODY))
 ;;           (         )           ; NEW CLAUSE
 ;;
 ;;
 ;; (SENTINEL TYPE (VARS)           ; IN FORM
 ;;           ((IN
 ;;              (FRAME SLOT TEST)
 ;;              (FRAME SLOT TEST))
 ;;            (BODY) (BODY))
 ;;               ETC.

;; (IFF-ADDED (VARS) ((CONDITION) (BODY)))   ;IF-ADDED & IF-REMOVED FORM
;; This form creates the rule frame as well as run it.
;; (IF-ADDED ........) simply runs an if-added or if-removed sentinel,
;; but does not save it as an explicit rule.

;;; The rule form causes a rule or sentinel to be run only once. However,
;;; a rule may need to be active for a longer period. Thus, when a
;;; context is activated, all rules should be active as long as the
;;; context is. They are then used as "when-ever" types. It is important
;;; to keep the distinction between a "rule" type of sentinel and a sentinel
;;; used as a rule, clear. The rule type of setinel would be used for example,
;;; in doing classic planner type reasoning where the rule was found
;;; through pattern matching, and then needed to be used just once.

(declare (special /:role roles vars
	  frame slot sentinel set type test sensor group body))

(FASSERT SENTINEL
         (AKO ($VALUE (THING)))
         (BODY)
         (SENSOR)
	 (TYPE)
         (GROUP))

(FASSERT SENSOR
         (AKO ($VALUE (THING)))
         (FRAME)
         (SLOT)
         (SENTINEL)
         (SET)
	 (TYPE)
         (TEST))

(FASSERT AND-SENSOR
         (AKO ($VALUE (THING)))
         (GROUP)
         (SENTINEL)
	 (TYPE)
         (SET))

(FASSERT OR-SENSOR
         (AKO ($VALUE (THING)))
         (GROUP)
         (SENTINEL)
	 (TYPE)
         (SET))

(FASSERT GROUP-SENSOR
         (AKO ($VALUE (THING)))
         (GROUP)
         (SENTINEL)
	 (TYPE)
         (SET))

(FASSERT IN-SENSOR
         (AKO ($VALUE (THING)))
         (GROUP)
         (SENTINEL)
         (PLACE)
	 (TYPE)
         (SET))

(defun if-added macro (x) 
  `(sentinel ,@x))

(defun if-removed macro (x)
  `(sentinel ,@x))


; TO AUGMENT THE SENTINELS TWO NEW SENSOR FRAMES WERE CREATED. IN-SENSOR IS
; LIKE & OR OR, BUT HAS A PLACE SLOT TO RECORD THE INSTANCE IT IS IN.
; THE GROUP-SENSOR IS IDENTICAL TO THE AND OR OR SENSOR, BUT GROUPS IN-SENSORS.
; THESE TWO SENSORS ARE REQUIRED TO ALLOW SENTINELS TO DISTINGUISH BETWEEN
; SENSORS WHICH CAN BE ON SIMILAR SLOTS OF ARBITRARY FRAMES, AND THOSE CASES
; WHERE WE WISH TO HAVE SENSORS ON MANY SLOTS OF ONE FRAME.


; TO MAKE USE OF THESE NEW SENSORS, BUILD-SENSOR HAS AN ADDITIONAL CLAUSE
; THAT CHECKS FOR AN IN-SENSOR. IF ONE IS FOUND CONS-GROUP-SENSOR IS CALLED.
; CONS-GROUP-SENSOR SETS IF-ADDED AND IF-REMOVED SENTINELS THAT KEEP TRACK
; OF NEW INSTANCES OF THE FRAME ON WHICH THE IN-SENSOR NEEDS TO OPERATE.
; THE ARGUMENT TO CONS-GROUP-SENSOR RETURNS A LIST OF ALL IN-SENSORS.
; THESE ARE GENERATED BY COLLECTING ALL INSTANCES OF THE GENERIC FRAME AND
; APPLYING CONS-IN-SENSOR TO THEM. CONS-IN-SENSOR SETS THE PLACE VARIABLE
; IN THE DIFFERENT IN-SENSORS. CONS-IN-SENSOR IN TURN TAKES AS ARGUMENTS A 
; LIST OF THE SENSORS BUILT BY MAPPING BUILD-SENSOR OVER THE INSTANCES, AND
; THE GENERIC FRAME.  SPECIFIC COMMENTS ON THIS CODE ARE/: FOREACH SHOULD
; RETURN LIST OF ALL IN-SENSORS; (CAADR L) SHOULD RETURN GENERIC FRAME.
; (SUBST TOKEN (CAADR L) (CDR L)) SHOULD REPLACE EACH OCCURENCE OF THE GENERIC
; FRAME WITH THE INSTANCE.

; THE DESIGN OF IN-SENSORS IS TO TREAT AN INDIVIDUAL IN-SENSOR AS AN AND, 
; EXCEPT THAT IT WILL AUTOMATICLY RECORD THE TOKEN IT IS IN. THE GROUP
; SENSOR IS AN OR, WHICH AUTOMATICLY PLACES THE INSTANCE SENTINELS.

; NOTES/: 1/: BY HAVING THIS EVAL HERE, ONE MUST QUOTE THE FRAME AND SLOT
; IN THE SENTINEL CALL; HOWEVER, IT ALLOWS ONE TO SPECIFY VARIABLES
; FOR THE FRAME NAME AND HAVE THEM PASSED INTO THE SENTINEL CALL BY THIS
; TRICK/: EG/: (SENTINEL IF-EVER (ASK-FRAME) ((ASK-FRAME 'SLOT1 ......
; 2/: EVAL IS NECESSARY, AS (CAADR L) RETURNS '<FRAME>. WHEN CONS-GROUP-SENSOR
; LATER TRIES TO CREATE SENTINELS, THESE WILL TRY AND BIND '<FRAME>, AS THE
; VARIABLE DENOTING THE FRAME NAME, INTO THE SENTINEL. THIS RESULTS IN
; ''<FRAME>. EVALING HERE REMOVES ONE QUOTE.


(DEFUN BUILD-SENSOR (L)
       (COND ((EQ (CAR L) 'AND)
              (CONS-AND-SENSOR
	       (MAPCAR 'BUILD-SENSOR (CDR L))))
             ((EQ (CAR L) 'OR)
              (CONS-OR-SENSOR
	       (MAPCAR 'BUILD-SENSOR (CDR L))))
             ((EQ (CAR L) 'IN)
	      (CONS-GROUP-SENSOR (COLLECT-IN-SENSORS L) (EVAL (CAADR L)) L)) ;2
             (T (CONS-SENSOR (EVAL (CAR L))	; 1
                             (EVAL (CADR L))	; 1
                             (CDDR L)))))

 ;; 1/: (EVAL (CAADR L)) BECAUSE OTHERWISE 'FRAME RATHER THAN FRAME IS RETURNED,
 ;; WHICH SCREWS UP FVALUES-ONLY.
 ;; 2/: SUBST SUBSTITUTES <FRAME> FOR '<FRAME>, WHICH CAUSED PROBLEMS IN THE
 ;; ORIGINAL FORM, WHICH CALLED BUILD-SENSOR. THESE WERE DUE TO THE FACT THAT
 ;; CONS-SENSOR EVAL'S ITS FIRST TWO ARGS. * BINDAUX HAS THE RIGHT FORM TO
 ;; MODIFY SUBST SO IT INSERTS THE RIGHT ARG. CLEAN THIS UP.
 ;; UNTIL THEN BUILD-IN-SENSOR MIMICKS
 ;; THE RIGHT CONS-SENSOR BEHAVIOUR, AT THE COST OF A LESS ELEGANT LOOKING
 ;; FLOW OF CONTROL. 

(DEFUN COLLECT-IN-SENSORS (L)
       (FOREACH TOKEN (*FVALUES-ONLY (EVAL (CAADR L)) 'INSTANCE)   ; 1
		(CONS-IN-SENSOR 
		 (MAPCAR 'BUILD-IN-SENSOR (SUBST TOKEN (CAADR L) (CDR L))) ; 2
		 TOKEN)))

(DEFUN BUILD-IN-SENSOR (L)
       (CONS-SENSOR (CAR L) (EVAL (CADR L)) (CDDR L)))


(DEFUN CONS-AND-SENSOR (SENSORS)
       (FOR (S (POP-FRAME 'AND-SENSOR))
               (DO-FOREACH X SENSORS
                        (fput X 'SENTINEL '$VALUE S)
                        (fput S 'GROUP '$VALUE X))
            S))

(DEFUN CONS-OR-SENSOR (SENSORS)
       (FOR (S (POP-FRAME 'OR-SENSOR))
               (DO-FOREACH X SENSORS
                        (fput X 'SENTINEL '$VALUE S)
                        (fput S 'GROUP '$VALUE X))
            S))

(DEFUN CONS-IN-SENSOR (SENSORS TOKEN)
       (FOR (S (POP-FRAME 'IN-SENSOR))
	    (DO-FOREACH X SENSORS
			(FPUT X 'SENTINEL '$VALUE S)
			(FPUT S 'GROUP '$VALUE X))
	    (FPUT S 'PLACE '$VALUE TOKEN)
	    S))


(DEFUN CONS-SENSOR (FRAME SLOT TEST)
       (REUSE-FRAME 'SENSOR FRAME FRAME SLOT SLOT
                   TEST (BINDFN VARS TEST)))


; CONS-GROUP-SENSOR USES AN IF-REMOVED SENTINEL WHICH LEAVES ITS TRIGGERS IN
; TO TAKE CARE OF THE CASE OF DISAPPEARING INSTANCES. THE BODY OF THIS
; SENTINEL, SPECIFIED IN THE CALL TO IT, COMPARES THE REMOVED VALUE WITH
; THE PLACE VALUES ON THE IN-SENSORS TO FIND THE IN-SENSOR WHOSE INSTANCE NO 
; LONGER EXISTS. IT REMOVES THIS SENSOR. THE SECOND PART OF THE BODY
; REMOVES THE NAME OF THE IN-SENSOR FROM THE GROUP-SENSOR. I HAVE A NOTE
; TO MYSELF TO CREATE A VERSION OF REM-SENSOR WHICH DOESN'T REMOVE TRIGGERS
; FROM DELETED INSTANCES. FINALLY, CONS-GROUP-SENSOR SETS A SENTINEL ACTIVATED
; BY THE ADDITION OF AN INSTANCE. THIS ADDS A NEW IN-SENSOR, ETC.
; IT RETURNS S, THE NAME OF THE GROUP-SENSOR CREATED,
; 1/: THE "FOR" SETS THE VALUE OF CLAUSES (OR ANOTHER ARBITRARY NAME), RATHER
; THAN THE <L> BEING PASSED INTO ADD-GROUP-SENSOR, BECAUSE SENTINEL CALLS ITS
; ARGUMENT L. HENCE L WOULD GET RESET IN THE SENTINEL CALL AND BE SET TO THE 
; BODY OF THE NEW SENTINEL INSTEAD OF THE OLD BODY.
; 2/: SINCE VARS MUST BE PASSED TO ADD-GROUP-SENSOR, A SIMILAR PROBLEM ARISES,
; SINCE THIS FUNCTION IS WITHIN ANOTHER SENTINEL THAT RESETS VARS. HENCE
; I EXPLICITLY SET VARS1 TO THE CURRENT VALUE OF VARS.
; 3/: THIS MUST OCCUR BEFORE THE SENSORS ARE REMOVED; OTHERWISE THE SENSOR FRAME
; IS DESTROYED, AND WE CAN'T GET TO THE GROUP SENSOR.

(declare (special s frame clauses vars1 condition action vars rule type))


(DEFUN CONS-GROUP-SENSOR (SENSORS FRAME L)
       (FOR (S (POP-FRAME 'GROUP-SENSOR)
	       CLAUSES (CDR L)
	       VARS1 VARS)	; 2
	    (DO-FOREACH X SENSORS
			(FPUT X 'SENTINEL '$VALUE S)
			(FPUT S 'GROUP '$VALUE X))
	    (SENTINEL IF-NEVER (S FRAME)
		      ((FRAME 'INSTANCE T)
		       (FOR (TARGET (FILTER X (FVALUES-ONLY S 'GROUP)
					    (EQUAL
					     (FVALUE-ONLY X 'PLACE) /:VALUE)))
			    (FREMOVE S 'GROUP '$VALUE (CAR TARGET))     ; 3
			    (REM-SENSOR (CAR TARGET)))))
	    (SENTINEL WHEN-EVER (S FRAME CLAUSES VARS1)
		      ((FRAME 'INSTANCE T)
		       (ADD-GROUP-SENSOR S CLAUSES /:VALUE VARS1)))	; 1
	    S))

; ADD-GROUP-SENSOR SHOULD CREATE AN IN-SENSOR AND ADD ITS NAME TO THE
; EXISTING GROUP SENSOR, AND SET THE NEW TRIGGERS. 
; 1/: IN THE ORIGINAL SENTINEL CALL, VARS IS BOUND. WHEN CONS-SENSOR CAUSES
; A FRAME TO BE MADE, VARS IS BOUND TO THE BODY. ADD-GROUP-SENSOR MUST
; SIMILARLY PRESERVE AND PASS THIS OLD VARS TO BUILD-IN-SENSOR. (SEE CALL IN 
; CONS-GROUP-SENSOR). (AND NOT THE VARS THAT THE SENTINEL IT IS IN CREATES).
; 2/: THIS DOUBLE "FOR" IS NEEDED, AS ASSIGNMENTS IN A FOR ARE MADE IN PARALLEL.
; THUS VARS WILL NOT HAVE A VALUE WITHIN THE CONS-IN-SENSOR CALL UNLESS THIS 
; IS DONE.


(DEFUN ADD-GROUP-SENSOR (GROUP-SENSOR CLAUSES TOKEN VARS1)	
  ; 1
  (FOR (VARS VARS1)
       (FOR                                                
	; 2
	(IN-SENSOR (CONS-IN-SENSOR
		    (MAPCAR 'BUILD-IN-SENSOR 
			    (SUBST TOKEN (CAADR CLAUSES) CLAUSES))
		    TOKEN))
	(FPUT IN-SENSOR 'SENTINEL '$VALUE GROUP-SENSOR)
	(FPUT GROUP-SENSOR 'GROUP '$VALUE IN-SENSOR)
	(SET-SENSOR S))))


(DEFUN CONS-SENTINEL (BODY SENSOR GROUP)
  (fput SENSOR 'SENTINEL '$VALUE
	(REUSE-FRAME SENTINEL BODY BODY
		     SENSOR SENSOR GROUP GROUP)))

(DEFUN SET-SENSOR (S)
  (COND ((OR (AKO/? S 'AND-SENSOR)
	     (AKO/? S 'OR-SENSOR)
	     (AKO/? S 'IN-SENSOR)
	     (AKO/? S 'GROUP-SENSOR))
	 (FPUT S 'TYPE '$VALUE (GET-TYPE S))
	 (MAPC 'SET-SENSOR (fvalues-only2 S 'GROUP)))
	(T 
	 (FPUT S 'TYPE '$VALUE (GET-TYPE S))
	 (SET-TRIGGER S))))

;; 1/: the if-removed sentinel is implemented by using an if-removed, rather
;; than an if-added, if the type is recognized. it leaves its trigger in, as
;; the in- form needs this. Thus currently would get removal by putting in
;; a call to rem-sentinel in the body, and binding it with the variable I
;; provide for that purpose (see other notes).


(defun set-trigger (s)
  (for (/:type (fvalue-only s 'type)
	      :frame (fvalue-only2 s 'frame)
	      :slot (fvalue-only2 s 'slot)
	      /:trigger [trigger [quote @s]]
	      /:trigger1 [trigger1 [quote @s]])
       (cond ((null roles)		; plain meal - no roles.
	      (COND ((EQUAL /:type 'RULE)
		     (SRETRIEVE1 S))
		    ((EQUAL /:TYPE 'IF-EVER)
		     (fput :frame :slot '$if-added /:trigger))
		    ((OR (EQUAL /:TYPE 'WHEN-EVER)
			 (EQUAL /:TYPE 'IF-ADDED))
		     (FPUT :frame :slot '$if-added /:trigger1))
		    ((EQUAL /:TYPE 'FOR-EVER)
		     (FPUT :frame :slot '$if-added /:trigger1)
		     (SRETRIEVE S))
		    ((OR (EQUAL /:TYPE 'IF-NEVER)
			 (EQUAL /:TYPE 'IF-REMOVED))
		     (FPUT :frame :slot '$if-removed /:trigger1))))
	     (t (prog (oldrole result)
		      (cond ((dtpr roles)
			     (setq oldrole (car (getroles :frame)))
			     (putrole :frame (cadr roles)))
			    ((setq oldrole nil)))
		      (setq result 
			    (COND ((EQUAL /:type 'RULE)
				   (SRETRIEVE1 S))
				  ((EQUAL /:TYPE 'IF-EVER)
				   (rput :frame :slot '$if-added /:trigger))
				  ((OR (EQUAL /:TYPE 'WHEN-EVER)
				       (EQUAL /:TYPE 'IF-ADDED))
				   (rPUT :frame :slot '$if-added /:trigger1))
				  ((EQUAL /:TYPE 'FOR-EVER)
				   (rPUT :frame :slot '$if-added /:trigger1)
				   (SRETRIEVE S))
				  ((OR (EQUAL /:TYPE 'IF-NEVER)
				       (EQUAL /:TYPE 'IF-REMOVED))
				   (rPUT :frame :slot '$if-removed /:trigger1))))
		      (cond (oldrole (putrole :frame oldrole)))
		      (return result))))))
			    
;; IF THE BODY IS A SHOUT, THE SET-SENTINELS FUNCTIONS, AFTER THE 
;; SHOUT SHOUTS, WILL RETURN (), AS THE FOREACH RETURNS A LIST OF
;; ITS LAST ARG, WHICH IN THE CASE OF A SHOUT IS UNPRINTABLE.

(DEFUN SET-SENTINELS (TYPE GROUP)
  (FOREACH S GROUP
	   (FPUT S 'GROUP '$VALUE GROUP)
	   (FPUT S 'TYPE '$VALUE TYPE)
	   (SET-SENSOR (fvalue-only2 S 'SENSOR))))
			    
(DEFUN GET-TYPE (S)
  (FVALUE-ONLY (FVALUE-ONLY S 'SENTINEL) 'TYPE))
			    
(defun bindfn (vars body)
  (cons 'for (cons (bindaux vars) body)))
;; returns an sexpr which when evaluated will first bind the
;; vars to their values in the env of this bindfn call and
;; will then evaluate the body.
			    

(DEFUN BINDAUX (LIST)
  (COND (LIST
	 (CONS (CAR LIST)
	       (CONS (LIST 'QUOTE
			   (APPLY 'EVAL (LIST (CAR LIST))))
		     (BINDAUX (CDR LIST)))))))
			    
			    
;; SYNTAX OF A SENTINEL STATEMENT IS/:
;; 1. A TYPE SPECIFICATION ALLOWING THE SENTINEL TO USE THE RIGHT
;; TRIGGER-FN AND APPLY SRETRIEVE, IF DESIRED. IT IS NOT QUOTED.
;; 2. A LIST OF VARIABLES TO BE PAIRED WITH THEIR CURRENT VALUES
;; AND BOUND ON TRIGGERING. I.E. THE TEST AND THE BODY
;; OF EACH CLAUSE OF THE WATCH-FOR ARE WITHIN THE SCOPE
;; LEXICALLY OF THESE LEXICALLY BOUND VARS.
;; IF THE VARIABLES DO NOT APPEAR WITHIN COND STRUCTURE, AN
;; "UNBOUND VARIABLE" ERROR WILL OCCUR.
;; 3. A COND STRUCTURE. EACH CLAUSE HAS A CONDITION WHICH IS A SENSOR
;; STRUCTURE. THIS DETERMINES WHERE SENSORS ARE TO BE SET AND WHAT
;; CONDITIONS MUST HOLD ON ANY TRIGGERING VALUE. THE BODY WILL BE
;; EVALLED IF ENOUGH VALUES IN THE RIGHT PLACES, WHICH PASS THE TESTS
;; ARE ADDED TO THE FRAME STRUCTURE. INDIVIDUAL CLAUSES CAN BE ANDS,ORS,
;; ETC. OVERALL, THE CLAUSE FUNCTIONS LIKE AN "OR", WITH SEPARATE BODIES
;; WHILE THE INS, ORS, AND ANDS ALL SHARE A BODY. 
			    

;; NOTES/: (1) THE CADDR MUST BE LISTED TO PUT AN EXTRA SET OF PARENTHESES
;; AROUND IT. OTHERWISE PULLING OFF THE CAR FOR BUILD-SENSOR FAILS.

(DEFUN SENTINEL FEXPR (L)
  ;;
  ;; 4 arguments -- with roles.(and butter/?)
  ;;	(sentinel role type variables clauses)
  ;;	or
  ;;	(sentinel (role/: role) type variables clauses)
  (cond ((or (eq (car l) 'role)
	     (and (dtpr (car l))
		  (eq (caar l) 'role/:)))
	 (FOR (TYPE (CADR L)
	       ROLES (CAR L)
	       VARS (CADDR L))
	      (SET-SENTINELS TYPE
			     (FOREACH CLAUSE (CDDDR L)
				      (CONS-SENTINEL
				       (BINDFN VARS (CDR CLAUSE))
				       (BUILD-SENSOR (CAR CLAUSE))
				       NIL)))))
	;;
	;;	3 arguments -- with no roles.
	;;		(sentinel type variables clauses)
	;;
	;;
	(t (FOR (TYPE (CAR L)
	         ROLES NIL
		 VARS (CADR L))
		(SET-SENTINELS TYPE
			       (FOREACH CLAUSE (CDDR L)
					(CONS-SENTINEL
					 (BINDFN VARS (CDR CLAUSE))
					 (BUILD-SENSOR (CAR CLAUSE))
					 NIL)))))))
			    
;; NOTE THAT THE BODY OF A SENTINEL CAN NOW MAKE USE OF THE
;; FACT THAT /:FRAME-SENTINEL IS BOUND TO THE SENTINEL VALUE, SO THAT
;; THE BODY CAN REFER TO THE SENTINEL IT IS IN. FOR EXAMPLE, IT
;; CAN REMOVE ITSELF NOW. ALSO, THE BODY CANNOT BE <T>, AS THE
;; BODY IS PUT IN THE SLOT AS (T), WHICH WHEN EVALED, CAUSES
;; AN UNDEFINED FUNCTION ERROR MESSAGE.
;; ANNOTATION/: 1) REMOVES SENTINEL WHEN IT SUCCEEDS.
;; 2) INSURES THAT IF TRIGGER HAS SUCCEEDED, DON'T RETRY IT.
;; 3) SETS AND-SENSOR TO T, SO NEVER TRYED AGAIN.
;; 4) SETS 0R-SENSOR TO T, SO NEVER TRYED AGAIN.
;; 5) SETS SENSOR TO T, SO NEVER TRYED AGAIN.
;; AN IN-SENSOR IS ESSENTIALLY A AND-SENSOR RESTRICTED TO A SINGLE
;; FRAME, AND IS SO TREATED. THE GROUP-SENSOR IS AN OR-SENSOR OVER
;; THE VARIOUS IN-SENSORS, AND IS SO TREATED.


(DEFUN TRIGGER-FN (S)
  (COND ((AKO/? S 'SENTINEL)
	 (FOR (B (fvalue-only2 S 'BODY)
		 /:FRAME-SENTINEL S)
	      (REM-SENTINELS (fvalue-only2 S 'GROUP))	; (1)
	      (EVAL B)))
	((fvalue-only2 S 'SET))	; (2)
	((AKO/? S 'AND-SENSOR)
	 (COND ((FORALL X (fvalues-only2 S 'GROUP)
                             (fvalue-only2 X 'SET))
                     (fput S 'SET '$VALUE T)	; (3)
                     (TRIGGER-FN (fvalue-only2 S 'SENTINEL)))))
             ((AKO/? S 'OR-SENSOR)
              (fput S 'SET '$VALUE T)	; (4)
              (TRIGGER-FN (fvalue-only2 S 'SENTINEL)))
             ((AKO/? S 'IN-SENSOR)
              (COND ((FORALL X (fvalues-only2 S 'GROUP)
                             (fvalue-only2 X 'SET))
                     (fput S 'SET '$VALUE T)	; (3)
                     (TRIGGER-FN (fvalue-only2 S 'SENTINEL)))))
             ((AKO/? S 'GROUP-SENSOR)
              (fput S 'SET '$VALUE T)	; (4)
              (TRIGGER-FN (fvalue-only2 S 'SENTINEL)))
             ((shout [error in trigger-fn]))))

;;; Trigger is called whenever a trigger fires, and then calls
;;; trigger-fn. Originally this function was the last clause in the Cond
;;; in trigger-fn, but for efficiciency it is separated out.
;;; Note that if I change back, I must also update the calls in
;;; rem-sensor.

(defun trigger (s)
       (cond ((eval (fvalue-only2 s 'test))
              (fput s 'set '$value t)         ;5
              (trigger-fn (fvalue-only2 s 'sentinel)))))


 ;; THIS TRIGGER-FN1 DIFFERS FROM THE REGULAR ONE IN TWO WAYS.
 ;; ITS CALL TO REM-SENTINEL HAS BEEN REMOVED SO THAT ALL 
 ;; SENTINELS STAY IN PLACE. IN ADDITION, THE TESTS ON THE
 ;; ARGUMENT'S SET SLOT HAVE BEEN REMOVED EXCEPT IN THE CASE OF THE
 ;; AND SENTINEL. THE "SET" VALUE IS PLACED ON A SENSOR FRAME WHEN 
 ;; ITS TEST HAS BEEN SUCCESSFUL, PREVENTING IT FROM BEING APPLIED
 ;; AGAIN. THESE CONSTRAINTS ARE REMOVED. IN THE CASE OF THE AND 
 ;; SENSOR, THE CHECKING FOR THE SET VALUE ON THE "AND" SENSOR ITSELF
 ;; HAS BEEN REMOVED, BUT LEFT IN AS A CONDITION FOR THE AND-SENTINEL
 ;; TO TRIGGER THAT ALL ITS PARTS MUST HAVE SUCCEEDED. A BUG IN
 ;; THE ORIGINAL AND-SENTINEL WAS THAT SINCE EACH TRIGGER WAS REMOVED
 ;; AFTER SUCCEEDING, SOME VALUES MIGHT HAVE BEEN CHANGED BACK LATER
 ;; ON, YET THE AND WOULD SUCCEED. IN THIS VERSION THIS IS CORRECTED
 ;; BY LETTING THE SUB-SENTINELS USE SET. HOWEVER, IF THEIR TESTS
 ;; FAIL, SET IS GIVEN A VALUE OF NIL. THUS ANY CHANGES IN VALUES 
 ;; WHICH DO NOT CHANGE THE SUCCESS OF ALL TESTS WILL TRIGGER IT, AND
 ;; IT WILL NOT FIRE WHENEVER ANY VALUE IS NOT CURRENTLY CORRECT. 
     
(DEFUN TRIGGER-FN1 (S)
       (COND ((AKO/? S 'SENTINEL)
              (FOR (B (fvalue-only2 S 'BODY)
                      /:FRAME-SENTINEL S)
                   (EVAL B)))
             ((AKO/? S 'AND-SENSOR)
              (COND ((FORALL X (fvalues-only2 S 'GROUP)
                             (fvalue-only2 X 'SET))
                     (TRIGGER-FN1 (fvalue-only2 S 'SENTINEL)))))
             ((AKO/? S 'OR-SENSOR)
              (TRIGGER-FN1 (fvalue-only2 S 'SENTINEL)))
             ((AKO/? S 'IN-SENSOR)
              (COND ((FORALL X (fvalues-only2 S 'GROUP)
                             (fvalue-only2 X 'SET))
                     (TRIGGER-FN1 (fvalue-only2 S 'SENTINEL)))))
             ((AKO/? S 'GROUP-SENSOR)
              (TRIGGER-FN1 (fvalue-only2 S 'SENTINEL)))
             ((shout [wrong arg in trigger-fn1]))))

;;; See comments on trigger-sensor for this fn.

(defun trigger1 (s)
       (cond ((eval (fvalue-only2 s 'test))
              (freplace s 'set '$value t)
              (trigger-fn1 (fvalue-only2 s 'sentinel)))
             ((freplace s 'set '$value nil))))

 ;; SRETRIEVE IS THE GENERAL FUNCTION WHICH CHECKS IF THE VALUE
 ;; IS IN THE SLOT AND APPLIES THE TRIGGER-FN TO IT. IT
 ;; HAS BEEN MADE A GENERAL FUNCTION SO THAT THE INHERITENCE
 ;; OF VALUES FOR THE SLOT NEED ONLY BE CHANGED HERE, SO THAT 
 ;; THE DIFFERENT DOMAINS CAN MODIFY THIS EASILY. ONLY CHECKS FOR 
 ;; SINGLE VALUES.

;;
;; uses trigger2, as is used in the for-ever sentinel: hence trigger stays.
;; if changed to trigger, trigger will disappear if test succeeds.
;;

(DEFUN SRETRIEVE (S)
              (FOR (/:VALUE (FVALUES-ONLY2
                            (*FVALUE-ONLY S 'FRAME)
                            (*FVALUE-ONLY S 'SLOT)))
                   (COND (/:VALUE (TRIGGER1 S)))))

;;; This form of sretrieve is used by the rule sentinel. It differs from
;;; sretrieve, as the sentinel erases itself regardless of success.
;;; Consequently, if the rule fails, we do not want its body evaluated
;;; by trigger-fn before erasing the sentinel. Thus sretrieve1 calls
;; rem-sentinels explicitly, instead of relying on trigger-fn.

(Defun sretrieve1 (s)
       (For (:value (Fvalues-only2
                      (*fvalue-only s 'frame)
                      (*fvalue-only s 'slot)))
            (cond (:value (trigger s))
                  (t (rem-sentinels 
                       (*fvalue-only (*fvalue-only s 'sentinel) 'group))))))

 ;; TO GET A SENTINEL WHICH REMOVES ITSELF WHEN ALL ITS PARTS ARE CURRENTLY
 ;; SUCCESSFUL, I NEED ONLY PUT A CALL TO REM-SENTINEL IN THE BODY OF THE 
 ;; SENTINEL. ANY SENTINEL CAN, IF IT SUCCEEDS, PASS ITS NAME OUTSIDE, THROUGH
 ;; THE USE OF THE /:FRAME-SENTINEL VARIABLE, SO THAT WE CAN DECIDE TO REMOVE IT
 ;; OUTSIDE THE FRAME. IF WE WISH TO REMOVE A SPECIFIC SENTINEL BEFORE IT
 ;; SUCCEEDS, THEN WE MUST KNOW ITS NAME. I DON'T YET TO DO THIS. 


(DEFUN REM-SENSOR (S)
       (COND ((OR (AKO/? S 'AND-SENSOR)
                  (AKO/? S 'OR-SENSOR)
		  (AKO/? S 'IN-SENSOR)
		  (AKO/? S 'GROUP-SENSOR))
              (MAPC 'REM-SENSOR (fvalues-only2 S 'GROUP)))
             (T (FREMOVE (fvalue-only2 S 'FRAME)
			 (fvalue-only2 S 'SLOT)
			 '$IF-ADDED
			 `(trigger ',s))
		(FREMOVE (fvalue-only2 S 'FRAME)
			 (fvalue-only2 S 'SLOT)
			 '$IF-ADDED
			 `(trigger1 ',s))
		(FREMOVE (fvalue-only2 S 'FRAME)
			 (fvalue-only2 S 'SLOT)
			 '$IF-REMOVED
			 `(trigger1 ',s))))
       (COND ((FRAME/? S)
              (PUSH-FRAME S))))

(DEFUN REM-SENTINELS (GROUP)
       (FOREACH S GROUP
                (COND ((FRAME/? S)
                       (REM-SENSOR (fvalue-only2 S 'SENSOR))
		       (PUSH-FRAME S)))))


 ;; THIS FUNCTION IS SUBJECT TO THE SAME SCOPING PROBLEM AS CLEAN UP
 ;; ALTERNATES, HENCE MUST CHECK AT TIME OF EXECUTION THAT THE LIST
 ;; ELEMENTS ARE STILL FRAMES. THIS BECAUSE REM-SENTINELS REMOVES 
 ;; ALL SENTINELS OF A SENTINEL'S GROUP, WHICH IS DUPLICATED ON
 ;; THE ORIGINAL BINDING OF S TO SENTINEL INSTANCES. BY THE TIME 
 ;; THESE ELEMENTS ARE GOTTEN TOO, THEY HAVE ALREADY BEEN DESTROYED.

(DEFUN RESET-SENTINELS ()
       (FOREACH S (INSTANCES 'SENTINEL)
                (COND ((FRAME/? S)
                       (REM-SENTINELS (fvalue-only2 S 'GROUP))))))



;;; MAKE-RULE takes the same syntax as sentinel, but instead of actually
;;; creating and executing a rule, it creates a frame with the approriate
;;; values. This frame can then be interpreted to run a rule later on.
;;; Unlike sentinels, however, several rules cannot be created within
;;; one rule statement. If this feature of sentinels becomes important
;;; to rules, I will add this capability to form non-logical "groups".
;;; Unlike sentinel syntax, only a single body can be specified. Hence
;;; unrelated functional actions must be bound up in a single executable
;;; function through progn, or "and", etc.

(defun make-rule fexpr (l)
       (for (type (car l)
		  vars (cadr l)
		  condition (caaddr l)
		  action (cadaddr l))
            (make-frame rule
                        type type
                        variables vars
                        condition condition
                        action action)))

;; rule both makes a rule and runs it.

;;(defun rule macro (x)
;;    (list 'run-rule
;;          (cons 'make-rule (cdr x))))

;;; iff-added makes and runs a rule of the if-added type.
;;; The syntax is/: (iff-added (vars) ((frame slot test) (body)))

(defun iff-added fexpr (l)
       (run-rule 
	 (for (vars (car l)
		    condition (caadr l)
		    action (cadadr l))
              (make-frame rule
                          type 'if-added
                          variables vars
                          condition condition
                          action action))))

(defun iff-removed fexpr (l)
       (run-rule 
	 (for (vars (car l)
		    condition (caadr l)
		    action (cadadr l))
              (make-frame rule
                          type 'if-removed
                          variables vars
                          condition condition
                          action action))))

;;; run-rule takes a rule frame as argument, and starts the
;;; corresponding sentinel.

(defun run-rule (x)
     (eval (cons 'sentinel
       (cons (*fvalue-only x 'type)
         (list (*fvalue-only x 'variables)
               (list (*fvalue-only x 'condition)
                     (*fvalue-only x 'action)))))))

;;; enqueue takes two args; an item and the name of a queue. It returns
;;; the queue

;;(defun enqueue macro (x)
;;  (list 'setq (caddr x)
;;	(list 'nconc (caddr x)
;;	      (list 'list (cadr x)))))

;;; dequeue takes one arg, the name of a queue and returns the queue.
 
;;(defun dequeue macro (x)
;;  (list '(lambda (first second) first)
;;	(list 'car (cadr x))
;;	(list 'setq (cadr x) (list 'cdr (cadr x)))))

;;; The following setq's initialize the stacks for reusable rule frames.

(defvar sensor-stack nil)
(defvar or-sensor-stack nil)
(defvar and-sensor-stack nil)
(defvar in-sensor-stack nil)
(defvar group-sensor-stack nil)
(defvar sentinel-stack nil)

;;; Pop-frame pops reusable frames off the appropriate stack.
;;; It is called by the local version of finstantiate. If no frames
;;; are available, finstantiate creates a new one.

(defun pop-frame (x)
  (cond
   ((equal x 'sensor) (cond ((pop sensor-stack)) ((finstantiate x))))
   ((equal x 'sentinel) (cond ((pop sentinel-stack)) ((finstantiate x))))
   ((equal x 'or-sensor) (cond ((pop or-sensor-stack)) ((finstantiate x))))
   ((equal x 'and-sensor) (cond ((pop and-sensor-stack)) ((finstantiate x))))
   ((equal x 'in-sensor) (cond ((pop in-sensor-stack)) ((finstantiate x))))
   ((equal x 'group-sensor) (cond ((pop group-sensor-stack))((finstantiate x))))
   (t (shout [wrong type frame to pop-frame]))))

;;; Try-pop trys to pop the right frame, and if not, finstantiates it.
;;; However, I can't pass the name of a stack as an atom, as pop gives
;;; an error, and I can't pass it as a variable, as nothing gets 
;;; popped off the named stack, so am not using this now.

;;; (defun try-pop (x y)
;;;        (cond ((setq stack (pop y)))
;;;              (t (finstantiate x))))


;;; Push-frame saves old rule frames by erasing all values and then
;;; pushing them on the right stack. It is called by the local version
;;; of ferase. The method of erasing values is quick, but will not trigger
;;; side effects, as currently I do not use any "if-removed"s in the
;;; rule frames. Note that it does not erase the first slot value as this
;;; is the AKO link.


(defun push-frame (x)
   (progn
     (do-foreach slot (cdr (fslots x)) (fclear x slot '$value))
     (for (stack (fvalue-only x 'ako))
          (cond ((equal stack 'sensor) (push x sensor-stack))
                ((equal stack 'sentinel) (push x sentinel-stack))
                ((equal stack 'or-sensor) (push x or-sensor-stack))
                ((equal stack 'and-sensor) (push x and-sensor-stack))
                ((equal stack 'in-sensor) (push x in-sensor-stack))
                ((equal stack 'group-sensor) (push x group-sensor-stack))
                (t (shout [can't push @x onto a stack; it is not a recognized
rule system frame]))))))

;;; Reuse-frame is used to replace make-frame in some of the sentinel
;;; functions.

(defun reuse-frame
       fexpr (l)
       (for (f (pop-frame
                 (cond ((atom (car l)) (car l))
                       (t (cadar l)))))
	    (apply 'add-to-frame (cons f (cdr l)))
	    f))

;;; Pop-frame, Push-frame and reuse-frame are used to replace finstantiate, 
;;; ferase and make-frame in the current version
;;; of sentinels. This version reuses old sentinel frames if possible.
;;; To return to the old version, simply replace these three functions 
;;; with the original finstantiate, ferase and make-frame. Note that
;;; This modification involves only the execution of sentinels, and not
;;; the creation of rule frames representing executable sentinels. 
;;; *possibly place a switch
;;; so that either implementation can be used.


