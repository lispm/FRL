From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:10:27 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08192; Thu, 2 Jun 88 14:10:25 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04400; Thu, 2 Jun 88 13:33:59 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08213; Thu, 2 Jun 88 12:46:54+0900
Date: Thu, 2 Jun 88 12:46:54+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020346.AA08213@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: sutil.l.frl
Status: RO


(include declar)
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; THIS FILE
;;;; PROVIDES SOME BASIC MECHANISMS FOR BUILDING FRAMES, ASKING
;;;; ABOUT THEM, RETRIEVING THEM, ADDING VALUES WHILE CHECKING
;;;; REQUIREMENTS ETC.
;;;; It contains part of the original PA-UTL file. The entire file
;;;; is in SIMUL. Another part has been put in SENTIN. Some functions
;;;; are commented because they partially or entirely duplicate others
;;;; in this file, and I haven't decided which form is better.
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(declare (special base ibase *nopoint))
(setq base 10.)
(setq ibase 10.)
(SETQ *NOPOINT T)

(declare (special *fgensym*))
(declare (fixnum *fgensym*))
(defvar *fgensym* 0)

(defun FGENSYM (prefix)
       ;; makes up and interns an atom with given prefix and
       ;; current value of *FGENSYM*.
       ;; As side effect, bumps *FGENSYM*.
  (setq *fgensym* (1+ *fgensym*))
  (implode (nconc (explode prefix)(list '-)(explode *fgensym*))))

;;;;;;;;;;;;;;;;
;;;; THESE ARE THE FUNCTIONS FOR GETTING FRAMES TO DESCRIBE
;;;; THEMSELVES. A $DESCRIBE FACET ON A SLOT MAY HOLD A FUNCTION
;;;; WHICH EXPECTS AN ENVIRONMENT INCLUDING :FRAME AND /:SLOT AND
;;;; WILL DESCRIBE THAT SLOT. A $DESCRIBE FACET ON THE SELF SLOT
;;;; MAY HOLD A FUNCTION EXPECTING AN ENVIRONMENT INCLUDING /:FRAME
;;;; AND WILL DESCRIBE THE WHOLE FRAME
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;
;;commenting out since describe-slot and describe-frame are also
;; in frl/thing.lisp
;;
;;(DEFUN DESCRIBE-SLOT (/:FRAME /:SLOT)
;;    (FOR (DESCRIBE (FDATUM-ONLY /:FRAME /:SLOT '$DESCRIBE))
;;         (COND (DESCRIBE (EVAL DESCRIBE))
;;               (T (DEFAULT-SLOT-DESCRIBE /:FRAME /:SLOT)))))
;;
;;(DEFUN DESCRIBE-SLOTS (/:FRAME /:SLOTS)
;;    (FOREACH /:SLOT /:SLOTS
;;             (DESCRIBE-SLOT /:FRAME /:SLOT)))
;;
;;(DEFUN DEFAULT-SLOT-DESCRIBE (/:FRAME /:SLOT)
;;   (FOR (/:VALUES (FVALUES-ONLY /:FRAME /:SLOT))
;;        (COND ((NULL /:VALUES))
;;	      ((CDR /:VALUES)
;;	       (SHOUT (APPEND [@/:SLOT = ]
;;			      /:VALUES)))
;;	      (T
;;               (SHOUT [@/:SLOT = @(CAR /:VALUES)])))))
;;
;;(DEFUN DEFAULT-FRAME-DESCRIBE (/:FRAME)
;;       (FOREACH /:SLOT (SLOTS-OF /:FRAME)
;;                (DESCRIBE-SLOT /:FRAME /:SLOT)))
;;
;;(DEFUN DESCRIBE-FRAME ARGS
;;    ;;; THE SECOND ARG IS OPTIONAL. IF TRUE IT PRINTS OUT
;;    ;;; THE FRAME NAME IT IS DESCRIBING. IF OMITTED, THE
;;    ;;; DEFAULT IS TRUE.
;;  (FOR (/:FRAME (ARG 1)
;;	SWITCH (COND ((EQ ARGS 2) (ARG 2))
;;		     (T T)))
;;       (COND (SWITCH
;;	      (SHOUT [DESCRIBING @/:FRAME])))
;;       (FOR (DESCRIBE (FDATUM-ONLY /:FRAME 'SELF '$DESCRIBE))
;;            (COND (DESCRIBE (EVAL DESCRIBE))
;;                  (T (DEFAULT-FRAME-DESCRIBE /:FRAME))))
;;       T))
;;
;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;
;;;;;; THESE ARE THE FUNCTIONS FOR GETTING FRAMES TO ASK FOR
;;;;;; VALUES FOR THEIR SLOTS.
;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;

(DEFUN ASK-FOR-TYPE (TYPE)
    ;;; THIS FUNCTION IS CALLED FROM A SLOT AND WILL MAKE A
    ;;; NEW FRAME OF THE "TYPE" ADD IT TO THE SLOT AND THEN ASK
    ;;; ABOUT IT.
    (FOR (/:VALUE (FINSTANTIATE TYPE))
	 (FPUT /:FRAME /:SLOT '$VALUE /:VALUE)
	 (SHOUT [THE @/:SLOT OF @/:FRAME IS @/:VALUE])
	 (ASK-ABOUT-FRAME /:VALUE)
	 /:VALUE))

(DEFUN FILL-SLOT (/:FRAME /:SLOT)
    ;;; IF A SINGLE SLOT HAS NO VALUE THEN THIS WILL ASK FOR
    ;;; ONE AND INSERT IT IF THE REQS ARE OK. IF THE SLOT IS
    ;;; A SET SLOT THIS WILL ASK FOR AS MANY VALUES AS THE USER
    ;;; WILL GIVE.
    (COND ((OR (FEATURE? /:FRAME /:SLOT 'SET)
               (NOT (FVALUES-ONLY /:FRAME /:SLOT)))
           (ASK-AND-ADD /:FRAME /:SLOT))))

(DEFUN FILL-SLOTS (/:FRAME /:SLOTS)
    (DO-FOREACH /:SLOT /:SLOTS (FILL-SLOT /:FRAME /:SLOT)))

(DEFUN ASK-ABOUT-FRAME (/:FRAME)
       ;;; THIS WILL ASK ABOUT EACH SLOT AND ADD THE VALUES
       ;;; TO THE FRAME.
       (FOR (ASK (FDATUM-ONLY /:FRAME 'SELF '$ASK))
            (COND (ASK (EVAL ASK))
                  (T (DEFAULT-FRAME-ASK /:FRAME)))
            /:FRAME))

(DEFUN DEFAULT-FRAME-ASK (/:FRAME)
       (FILL-SLOTS /:FRAME (SLOTS-OF /:FRAME)))

(DEFUN ANOTHER-FN (/:FRAME /:SLOT)
       (COND ((NULL (FVALUES-ONLY /:FRAME /:SLOT)) 'A)
             (T 'ANOTHER)))

(DEFUN DEFAULT-SLOT-ASK (/:FRAME /:SLOT)
   ;;; THIS WILL ASK FOR A VALUE FOR A SLOT. IT RETURNS THE VALUE
   ;;; BUT DOESNT ADD IT.
   (COND ((FEATURE? /:FRAME /:SLOT 'SET)
          (REQUEST6 [WHAT IS @(ANOTHER-FN /:FRAME /:SLOT)
                    @/:SLOT OF @/:FRAME ?]))
         (T (REQUEST6 [WHAT IS THE @/:SLOT OF @/:FRAME ?]))))

(DEFUN ASK-ABOUT-SLOT (/:FRAME /:SLOT)
  (FOR
    (ASK (FDATUM-ONLY /:FRAME /:SLOT '$ASK))
    (PROCESS-ANSWER (COND (ASK (EVAL ASK))
                          (T (DEFAULT-SLOT-ASK /:FRAME /:SLOT))))))

(DEFUN PROCESS-ANSWER (A)
    (FOR
      (D (DEFINITION /:FRAME /:SLOT))
      (COND ((AND A (ATOM A)
                  (NOT (NUMBERP A))
                  (NOT (FRAME? A))
                  D
                  (OR (EXISTS-NAMED? D A)
                      (AFFIRMATIVE?
                         (REQUEST6 [IS THERE A
                              @D NAMED @A ]))))
             (NAMED D A))
            (T A))))

(DEFUN NAMED (TYPE NAME)
     ;;;; FINDS A FRAME OF THE SPECIFIED TYPE WITH THAT NAME
     ;;;; OR CONSTRUCTS ONE.
       (COND ((EXISTS-NAMED? TYPE NAME))
             (T (FOR (F (FINSTANTIATE TYPE))
                     (FPUT F 'NAME '$VALUE NAME)
                     (ASK-ABOUT-frame F)))))

(DEFUN EXISTS-NAMED? (TYPE NAME)
      ;;;; FINDS A FRAME OF THE SPECIFIED TYPE WITH THAT NAME IF POSS.
       (EXISTS F (FDESCENDANTS TYPE 'INSTANCE)
               (EQUAL (FVALUE-ONLY F 'NAME) NAME)))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; REQUIREMENT MECHANISM. THIS CHECKS REQUIREMENTS AND KNOWS
;;;; HOW TO COMPLAIN.
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(SETQ COMPLAIN-SW T)

(DEFUN GET-MSG (PROP-STRS PROP LABEL)
       (FOR (PROP-STR
              (EXISTS X PROP-STRS
                      (EQUAL (CAR X) PROP)))
            (AND PROP-STR
                 (CADR (FCOMMENT? PROP-STR LABEL)))))

(DEFUN COMPLAIN (/:FRAME /:SLOT /:VALUE)
  (FOR (KEY-STR (FDATA /:FRAME /:SLOT '$REQUIRE)
        REQS (FDATA-ONLY /:FRAME /:SLOT '$REQUIRE))
       (FOR (POLL (FPOLL REQS))
            (FOR (ERRORS (CADR (ASSQ '? POLL))
                  FALSEHOODS NIL)
                 (FOREACH FALSEHOOD (CADR (ASSQ NIL POLL))
                     (FOR (COMPLAINT (OR (COMPLAIN-FN
                                           FALSEHOOD)
                                         (GET-MSG KEY-STR
                                              FALSEHOOD
                                              'COMPLAIN/:)))
                          (COND (COMPLAINT (EVAL COMPLAINT))
                                (T (PUSH FALSEHOOD
                                         FALSEHOODS)))))
                 (COND ((NOT (NULL FALSEHOODS))
                        (SHOUT [THE FOLLOWING UNEXPLAINABLE
                                REQUIREMENTS ARE VIOLATED])
                        (MAPC 'PpRIN1 FALSEHOODS)))
                 (COND ((NOT (NULL ERRORS))
                        (SHOUT [THE FOLLOWING ARE NOT YET
                               COMPUTABLE])
                        (MAPC 'PpRIN1 ERRORS)))
                 (TERPRI)
                 POLL))))

(DEFUN COMPLAIN-FN (REQ)
       ;;; GETS THE MORE COMMON TYPES OF COMPLAIN FUNCTIONS
       (AND (NOT (NULL REQ))
            (COND ((EQUAL (CAR REQ) 'AKO-REQ)
                   [AKO-COMPLAIN @(CADR REQ)])
		  ((EQUAL (CAR REQ) 'BETWEEN-REQ)
		   [BETWEEN-COMPLAIN @(CADR REQ) @(CADDR REQ)]))))

(DEFUN AKO-REQ (TYPE)
       (AND (FRAME? /:VALUE)
            (AKO? /:VALUE TYPE)))

(DEFUN AKO-COMPLAIN (TYPE)
       (SHOUT [@/:VALUE IS NOT A @TYPE]))

(DEFUN BETWEEN-REQ (A B)
       (NOT (OR (LESSP /:VALUE A)(GREATERP /:VALUE B))))

(DEFUN BETWEEN-COMPLAIN (A B)
       (COND ((LESSP /:VALUE A)
	      (SHOUT [@/:VALUE IS LESS THAN @A]))
	     (T (SHOUT [@/:VALUE IS GREATER THAN @B]))))

(DEFUN CHECK-REQS-OK (/:FRAME /:SLOT /:VALUE)
       (COND ((FPOLL-SUMMARY (FDATA-ONLY /:FRAME /:SLOT '$REQUIRE)))
             (T
              (AND COMPLAIN-SW
                   (SHOUT [@/:VALUE VIOLATES THE REQUIREMENTS
                          OF THE @/:SLOT SLOT OF @/:FRAME])
                   (COMPLAIN /:FRAME /:SLOT /:VALUE))
              NIL)))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; FUNCTIONS FOR ADDING VALUES TO SLOTS OF FRAMES. SOME OF THESE
;;;; TRIGGER THE COMPLAINT MECHANISM.
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(DEFUN FADD-VALUE-IF-REQS-OK (/:FRAME /:SLOT /:NEW-VALUE)
       (COND ((CHECK-REQS-OK /:FRAME /:SLOT /:NEW-VALUE)
              (FPUT /:FRAME /:SLOT '$VALUE /:NEW-VALUE))))


(DEFUN ASK-AND-ADD (/:FRAME /:SLOT)
    ;;; ASKS FOR VALUES OF A SLOT. THE ANSWER
    ;;; NIL MEANS NO MORE VALUES ARE TO BE ADDED. OTHERWISE
    ;;; THE ANSWER IS ADDED IF ITS
    ;;; REQUIREMENTS ARE SATISFIED AND THE QUESTION
    ;;; IS REPEATED IF THEY ARE NOT. A SET TYPE SLOT WILL
    ;;; CAUSE THE QUESTION TO BE REPEATED UNTIL NO MORE
    ;;; VALUES ARE TO BE ADDED.
    (COND ((FEATURE? /:FRAME /:SLOT 'SET)
           (DO ((ANSWER (ASK-AND-ADD-N /:FRAME /:SLOT)
                        (ASK-AND-ADD-N /:FRAME /:SLOT))
                (ANSWERS NIL (CONS ANSWER ANSWERS)))
               ((NULL ANSWER) ANSWERS)))
          (T (ASK-AND-ADD-1 /:FRAME /:SLOT))))

(DEFUN ASK-AND-ADD-SLOTS (/:FRAME /:SLOTS)
    (DO-FOREACH /:SLOT /:SLOTS
        (ASK-AND-ADD /:FRAME /:SLOT)))

(DEFUN ASK-AND-ADD-1 (/:FRAME /:SLOT)       
       (FOR (ANSWER (ASK-ABOUT-SLOT /:FRAME /:SLOT))
            (AND ANSWER
                 (COND ((FADD-VALUE-IF-REQS-OK
                              /:FRAME /:SLOT ANSWER))
                       (T
                        (SHOUT [TRY AGAIN])
                        (ASK-AND-ADD-1 /:FRAME /:SLOT))))))

(DEFUN ASK-AND-ADD-N (/:FRAME /:SLOT)
    ;;; THIS CHECKS THAT ANOTHER VALUE IS TO BE INSERTED
    ;;; TO DEAL WITH SET SLOTS. IF THE ASK-FN OF THE SLOT
    ;;; IS ASK-FOR-TYPE IT WILL ADD A VALUE BEFORE QUESTIONING
    ;;; THE USER WHICH IS WHY THIS FUNCTION IS NEEDED.
    (COND ((REQUEST6 [DO YOU HAVE
                     @(ANOTHER-FN /:FRAME /:SLOT) VALUE
                     FOR THE @/:SLOT OF @/:FRAME ? ])
           (ASK-AND-ADD-1 /:FRAME /:SLOT))))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; SOME ODDS AND ENDS OF FRAME FUNCTIONS.
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(DEFUN FEATURE (/:FRAME /:SLOT /:F)
     (FPUT /:FRAME /:SLOT '$FEATURE /:F))

(DEFUN FEATURES (/:FRAME /:SLOT)
     (FDATA-ONLY /:FRAME /:SLOT '$FEATURE))
 
(DEFUN FEATURE? (/:FRAME /:SLOT /:TYPE)
       (MEMBER /:TYPE
               (FDATA-ONLY /:FRAME /:SLOT '$FEATURE)))

(DEFUN SLOTS-OF (F)
       (SETMINUS (FHERITAGE-SLOTS F)
                 DUMMY-SLOTS))

; domain is here so that when instantiating frames in a rule domain
; I am not always asked for this slot. 

(SETQ DUMMY-SLOTS
      '(NEWFN AKO INSTANCE REF REF-BY NEXT PREVIOUS
        DESCRIBE SELF DEFN MATCH ASSERT TEST
              CLASSIFICATION))

(DEFUN DEFINITION (/:FRAME /:SLOT)
       (FOR (TYPE (EXISTS X (FINHERIT /:FRAME /:SLOT '$REQUIRE)
                      (NOT (ATOM X))
                      (OR (EQUAL (CAR X) 'AKO?)
                          (EQUAL (CAR X) 'AKO-REQ))))
            (COND (TYPE (COND ((EQUAL (CAR TYPE) 'AKO?)
                               (CADR (CADDR TYPE)))
                              (T (CADR (CADR TYPE))))))))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; THESE ARE FUNCTIONS FOR MAKING NEW FRAMES.
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(DEFUN NEW-EXAMPLE FEXPR (L)
       (ASK-ABOUT-FRAME (APPLY 'MAKE-FRAME L)))

(DEFUN MYINSTANTIATE ARGS
       (COND ((FRAME? (ARG 1))
              (APPLY 'FINSTANTIATE (LISTIFY ARGS)))
             (T (SETARG 1 (REQUEST6 [ YOU ASKED ME TO
                          INSTANTIATE @(ARG 1) WHICH IS
                          NOT A FRAME
                          WHAT DO YOU REALLY MEAN ]))
                (APPLY 'MYINSTANTIATE (LISTIFY ARGS)))))

(DEFUN MAKE-FRAME FEXPR (L)
   (FOR (F (MYINSTANTIATE (COND ((ATOM (CAR L)) (CAR L))
                                (T (CADAR L)))))
        (APPLY 'ADD-TO-FRAME 
               (CONS F (CDR L)))
        F))

(DEFUN ADD-TO-FRAME FEXPR (L)
   (FOR (F (CAR L))
        (COND ((NULL (CDR L)) F)
              (T (FOR (V (EVAL (CADDR L)))
                      (COND (V (FPUT F (CADR L) '$VALUE V))))
                 (APPLY 'ADD-TO-FRAME
                        (CONS F (CDDDR L)))))))



;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; SOME SIMPLE RETRIEVAL FUNCTIONS AND FUNCTIONS FOR TESTING
;;;; FRAMES FOR PARTICULAR STRUCTURE.
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(DEFUN FIND-FRAME FEXPR (L)
    (EXISTS X (FDESCENDANTS (CAR L) 'INSTANCE)
            (APPLY 'HAS-PROPERTIES
                   (CONS X (CDR L)))))

(DEFUN FIND-FRAMES FEXPR (L)
    (FILTER X (FDESCENDANTS (CAR L) 'INSTANCE)
            (APPLY 'HAS-PROPERTIES
                   (CONS X (CDR L)))))

(DEFUN FIND-OR-MAKE FEXPR (L)
    (FOR (F (APPLY 'FIND-FRAME L))
         (COND ((NULL F)
                (APPLY 'MAKE-FRAME L))
               (T F))))

(DEFUN HAS-PROPERTIES FEXPR (L)
    (COND ((NULL (CDR L)))
          ((HAS-PROPERTY (CAR L)
                         (CADR L)
                         (EVAL (CADDR L)))
           (APPLY 'HAS-PROPERTIES
                  (CONS (CAR L) (CDDDR L))))
          (T NIL)))

(DEFUN HAS-PROPERTY (F S V)
       (MEMBER V (FVALUES-ONLY F S)))

(DEFUN ISA? (X TYPE)
    ;; SAME AS AKO-REQ
       (AND (FRAME? X)
            (AKO? X TYPE)))

;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;
;;;; PROVIDES SOME USEFUL TESTING FUNCTIONS.
;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;

(DEFUN TEST-FUNCTION FEXPR (L)
    ;;;; THE FIRST ARGUMENT IS THE NAME OF THE FUNCTION.
    ;;;; THE OTHER ARGS ARE USER NAMES FOR THE ARGUMENTS OF
    ;;;; THE FUNCTION BEING TESTED. THE FUNCTION WILL BE CALLED
    ;;;; REPEATEDLY AND EACH TIME, ARGS WILL BE ASKED FOR.
    ;;;; IF THE WORD 'RESULT IS USED TO ANSWER AN ARGUMENT
    ;;;; QUERY THE RESULT OF THE LAST EXECUTION WILL BE USED.
    ;;;; IF THE NAME OF AN ARGUMENT IS NON-ATOMIC THE CAR IS
    ;;;; TAKEN AS THE NAME AND THE CADR IS A CONTROLLER. A
    ;;;; CADR OF RESULT MEANS THAT AFTER THE FIRST TIME ROUND, RESULT
    ;;;; WILL BE USED AS VALUE. A CADR OF CONST, MEANS THAT AFTER THE
    ;;;; FIRST TIME ROUND THE OLD VALUE WILL BE USED.
    (FOR (FUN (CAR L)
	  NAMES (CDR L)
	  RESULT NIL
	  SAVED-VALS NIL)
	 (DO ((FIRST-TIME T)
	      (CONTINUE T (REQUEST6 [GO-ON ??])))
	     ((NOT (AFFIRMATIVE? CONTINUE))
	      (SHOUT [DONE]))
	     (SETQ RESULT
		  (APPLY FUN
		         (FOREACH NAME NAMES
			     (NAME-VAL NAME))))
	     (SHOUT [RESULT = @RESULT])
	     (setq first-time nil)
	     RESULT)))

(DEFUN NAME-VAL (NAME)
    ;;; AN AUXILLIARY FN FOR TEST-FUNCTION.
    (COND ((ATOM NAME)
           (FOR (V (REQUEST6 [@NAME = ??]))
	        (COND ((EQ V 'RESULT) RESULT)
		      (T V))))
	  ((EQ (CADR NAME) 'RESULT)
	   (COND (FIRST-TIME
		  (REQUEST6 [INITIAL VALUE OF @(CAR NAME) = ??]))
		 (T RESULT)))
	  ((EQ (CADR NAME) 'CONST)
	   (COND (FIRST-TIME
		  (FOR (V (REQUEST6 [INITIAL VALUE OF
				    @(CAR NAME) = ??]))
		       (SETQ SAVED-VALS
			     (CONS (CONS (CAR NAME) V)
				   SAVED-VALS))
		       V))
		 (T (CDR (ASSQ (CAR NAME) SAVED-VALS)))))
	  (T (ERROR 'BAD-NAME-FORMAT))))
;;
;;(DEFUN BUTLAST (LIST)
;;       (REVERSE (CDR (REVERSE LIST))))
;;
;;
;;(SSTATUS MACRO (CAR (EXPLODEN '/#)) 'VALMAC)
;;
;;(DEFUN BINDING-NAME (X)
;;       (COND ((ATOM X) X)
;;             (T (CAR X))))
;;
;;(DEFUN BINDING-VALUE (X)
;;       (COND ((ATOM X) (LIST 'QUOTE X))
;;             (T (CADR X))))
;;
;;(DEFUN BINDINGS (LIST)
;;       (COND ((NULL LIST) NIL)
;;             (T (CONS (BINDING-NAME (CAR LIST))
;;                      (CONS (LIST 'fvalue-only2 '/:FRAME
;;                                  (BINDING-VALUE
;;                                        (CAR LIST)))
;;                            (BINDINGS (CDR LIST)))))))
;;
;;(DEFUN CONDITION (LIST THEN-CLAUSE ELSE-CLAUSE)
;;       (LIST 'COND
;;             (CONS (CONS 'AND (SUBST NIL NIL LIST))
;;                   THEN-CLAUSE)
;;             (CONS T
;;                   ELSE-CLAUSE)))
;;
;;(DEFUN T-CLAUSE (FORM)
;;       (COND ((NULL FORM) NIL)
;;             ((EQUAL (CAR FORM) 'ELSE) NIL)
;;             (T (CONS (CAR FORM) (T-CLAUSE (CDR FORM))))))
;;
;;(DEFUN E-CLAUSE (FORM)
;;       (COND ((NULL FORM) NIL)
;;             ((EQUAL (CAR FORM) 'ELSE) (CDR FORM))
;;             (T (E-CLAUSE (CDR FORM)))))
;;
;;(DEFUN IF-PRESENT MACRO (FORM)
;;       (FOR (THEN-CLAUSE (T-CLAUSE (CDDR FORM))
;;             ELSE-CLAUSE (E-CLAUSE (CDDR FORM))
;;             ARGLIST (CADR FORM))
;;            (LIST 'FOR
;;                  (BINDINGS ARGLIST)
;;                  (CONDITION (FOREACH X ARGLIST
;;                                      (COND ((ATOM X) X)
;;                                            (T (CAR X))))
;;                             THEN-CLAUSE ELSE-CLAUSE))))
;;
;;(DEFUN IF-PRESENT-IN MACRO (FORM)
;;       ;;; USES (CAR FORM) AS THE FRAME WHOSE SLOTS ARE
;;       ;;; CHECKED AND BINDS /:FRAME TO IT. (CAR FORM)
;;       ;;; CAN BE AT-SIGNED FOR EVALUATION.
;;       (SETQ FORM (CDR FORM))
;;       [FOR [/:FRAME @(COND ((ATOM (CAR FORM))
;;                            [QUOTE @(CAR FORM)])
;;                           ((EQUAL (CAAR FORM) 'percentsign)
;;                            (CADAR FORM))
;;                           (T (ERROR "IF-PRESENT-IN ?")))]
;;            @(CONS 'IF-PRESENT (CDR FORM))])
;;
;;(DEFUN ASK-ABOUT (/:NEW-FRAME)
;;       (FOR (/:FRAME (FIRST-PARENT-WITH-NEWFN /:NEW-FRAME))
;;            (EVAL (fvalue-only2 /:NEW-FRAME 'NEWFN))
;;            /:NEW-FRAME))
;;
;;(DEFUN NEW-EXAMPLE ARGS
;;       (ASK-ABOUT (APPLY 'MYINSTANTIATE
;;                         (LISTIFY ARGS))))
;;
;;
;;(DEFUN NAMED (TYPE NAME)
;;       (COND ((EXISTS-NAMED TYPE NAME))
;;             (T (FOR (F (FINSTANTIATE TYPE))
;;                     (fput-value F 'NAME NAME)
;;                     (ASK-ABOUT F)))))
;;
;;(DEFUN EXISTS-NAMED (TYPE NAME)
;;       (EXISTS F (FDESCENDANTS TYPE 'INSTANCE)
;;               (EQUAL (fvalue-only2 F 'NAME) NAME)))
;;
;;(DEFUN ASK-FOR (/:FRAME /:SLOT)
;;  (FOR (TO-ASK (fdatum-only /:FRAME /:SLOT '$TO-ASK))
;;       (FOR (ANSWER (COND (TO-ASK (EVAL TO-ASK))
;;                          (T (REQUEST6 [WHAT IS THE @/:SLOT
;;                                       OF @/:FRAME ?])))
;;             D (DEFINITION /:FRAME /:SLOT))
;;             (COND ((AND ANSWER (ATOM ANSWER)
;;                         (NOT (NUMBERP ANSWER))
;;                         (NOT (FRAME? ANSWER))
;;                         D
;;                         (OR (EXISTS-NAMED D ANSWER)
;;                             (AFFIRMATIVE?
;;                                (REQUEST [IS THERE A
;;                                   @D NAMED @ANSWER]))))
;;                    (NAMED D ANSWER))
;;                   (T ANSWER)))))
;;
;;(DEFUN FADD-SV (/:FRAME /:SLOT /:VALUE)
;;       (FOR (X (fvalue-only2 /:FRAME /:SLOT))
;;            (COND (X (COND ((EQUAL X /:VALUE)
;;                            (SHOUT [I ALREADY KNOW])
;;                            (DESCRIBE-SLOT)
;;                            /:VALUE)
;;                           ((REQUEST [THAT DISAGREES WITH WHAT
;;                                     I KNOW. SHALL I CHANGE THE
;;                                     @/:SLOT OF @/:FRAME FROM @X
;;                                     TO @/:VALUE])
;;                            (FREMOVE-VALUE /:FRAME /:SLOT X)
;;                            (fput-value /:FRAME /:SLOT /:VALUE))
;;                           (T X)))
;;                  (T (fput-value /:FRAME /:SLOT /:VALUE)))))
;;
;;(SETQ COMPLAIN-SW T)
;;
;;(DEFUN GET-MSG (PROP-STRS PROP TOPIC)
;;       (FOR (PROP-STR
;;              (EXISTS X PROP-STRS
;;                      (EQUAL (CAR X) PROP)))
;;            (AND PROP-STR
;;                 (FOR (COMMENT (ASSOC TOPIC
;;                                      (CDR PROP-STR)))
;;                      (AND COMMENT
;;                           (CDR COMMENT)
;;                           (CADR COMMENT))))))
;;
;;
;;(DEFUN COMPLAIN (F S)
;;       (FOR (KEY-STR (fdata-heritage F S '$require)
;;             POLL (fcheck F S))
;;            (FOR (ERRORS (CADR (ASSQ '? POLL))
;;                  FALSEHOODS NIL)
;;                 (FOREACH FALSEHOOD (CADR (ASSQ NIL POLL))
;;                     (FOR (COMPLAINT (OR (COMPLAIN-FN
;;                                           FALSEHOOD)
;;                                         (GET-MSG KEY-STR
;;                                              FALSEHOOD
;;                                              'COMPLAIN/:)))
;;                          (COND (COMPLAINT (EVAL COMPLAINT))
;;                                (T (PUSH FALSEHOOD
;;                                         FALSEHOODS)))))
;;                 (COND ((NOT (NULL FALSEHOODS))
;;                        (SHOUT [THE FOLLOWING UNEXPLAINABLE
;;                                CONSTRAINTS ARE VIOLATED])
;;                        (MAPC 'PPRIN1 FALSEHOODS)))
;;                 (COND ((NOT (NULL ERRORS))
;;                        (SHOUT [THE FOLLOWING ARE NOT YET
;;                               COMPUTABLE])
;;                        (MAPC 'PPRIN1 ERRORS)))
;;                 (TERPRI)
;;                 POLL)))
;;
;;(DEFUN COMPLAIN-FN (REQ)
;;       ;;; GETS THE MORE COMMON TYPES OF COMPLAIN FUNCTIONS
;;       (AND (NOT (NULL REQ))
;;            (COND ((EQUAL (CAR REQ) 'AKO-REQ)
;;                   [AKO-COMPLAIN @(CADR REQ)])
;;                  ((EQUAL (CAR REQ) 'MUST-BE)
;;                   (SHOUT [@/:NEW-VALUE IS NOT])
;;                   (APPLY 'DESCRIBE (CDDR REQ))))))
;;
;;(DEFUN AKO-REQ (TYPE)
;;       (AND (FRAME? /:NEW-VALUE)
;;            (AKO? /:NEW-VALUE TYPE)))
;;
;;(DEFUN AKO-COMPLAIN (TYPE)
;;       (SHOUT [@/:NEW-VALUE IS NOT A @TYPE]))
;;
;;(DEFUN CHECK-REQS-OK (/:FRAME /:SLOT /:NEW-VALUE)
;;       (flistput /:FRAME (LIST /:NEW-VALUE) /:SLOT '$value)
;;       (COND ((SIMPLE-POLL (fdata-only /:FRAME /:SLOT '$require))
;;              (flistdelete /:FRAME /:SLOT '$value /:NEW-VALUE))
;;             (T
;;              (AND COMPLAIN-SW
;;                   (SHOUT [@/:NEW-VALUE VIOLATES THE CONSTRAINTS
;;                          OF THE @/:SLOT SLOT OF @/:FRAME])
;;                   (COMPLAIN /:FRAME /:SLOT))
;;              (flistdelete /:FRAME /:SLOT '$value /:NEW-VALUE)
;;              NIL)))
;;
;;(DEFUN SPEAK (LIST)
;;       ;;; PRINTS A LIST. ANY FRAME HAS ONLY ITS NAME
;;       ;;;PRINTED
;;       (TERPRI)
;;       (DO ((LIST LIST (CDR LIST)))
;;           ((NULL LIST) (TERPRI))
;;           (PRINC (COND ((FRAME? (CAR LIST))
;;                         (FNAME (CAR LIST)))
;;                        (T (CAR LIST))))
;;           (PRINC '/ )))
;;
;;(DEFUN ASK-AND-ADD (/:FRAME /:SLOT)
;;       ;;; ASKS FOR THE VALUE OF A SLOT. THE ANSWER
;;       ;;; NIL MEANS NO VALUE IS TO BE ADDED. OTHERWISE
;;       ;;; THE ANSWER IS ADDED IF ITS
;;       ;;; REQUIREMENTS ARE SATISFIED AND THE QUESTION
;;       ;;; IS REPEATED IF THEY ARE NOT
;;       (FOR (ANSWER (ASK-FOR /:FRAME /:SLOT))
;;            (AND ANSWER
;;                 (COND ((FADD-VALUE-IF-REQS-OK
;;                              /:FRAME /:SLOT ANSWER))
;;                       (T
;;                        (SPEAK [TRY AGAIN])
;;                        (ASK-AND-ADD /:FRAME /:SLOT))))))
;;
(DEFUN INSTANCES (X)
       ;;; THIS MUST NOT USE INHERITANCE!!
       (COND ((FRAME? X)
              (*fvalues-only X 'INSTANCE))
             (T NIL)))
;;
;;(DEFUN CHECK-AND-ASK (/:FRAME /:SLOT)
;;       ;;;IF THE SLOT HAS NO VALUE THEN THIS WILL ASK FOR
;;       ;;; ONE AND INSERT IT IF THE REQS ARE OK
;;       (COND ((NOT (fdata-only /:FRAME /:SLOT '$value))
;;              (ASK-AND-ADD /:FRAME /:SLOT))))
;;
;;(DEFUN DESCRIBE-SLOT NIL
;;   (FOR (/:VALUE (fvalue-only2 /:FRAME /:SLOT))
;;        (COND (/:VALUE
;;               (SHOUT [ THE @/:SLOT OF
;;                       @/:FRAME IS @/:VALUE ])))))
;;
;;(DEFUN DEFAULT-DESCRIBE (/:FRAME)
;;       (FOREACH /:SLOT (SLOTS-OF /:FRAME)
;;                (DESCRIBE-SLOT)))
;;
;;(DEFUN DESCRIBE (/:FRAME)
;;       (IF-PRESENT (DESCRIBE)
;;                   (APPLY DESCRIBE NIL)
;;        ELSE (DEFAULT-DESCRIBE /:FRAME))
;;       T)
;;
;;(DEFUN FEQUAL (A B)
;;       (EQUAL (FNAME A) (FNAME B)))
;;
;;(DEFUN NEWFN (/:FRAME SLOTS)
;;       ;;; ASKS FOR EACH THE VALUE OF EACH OF THE SLOTS
;;       ;;; THAT ARE UN-FILLED
;;       ;;; AND ADDS THEM IF THE REQS ARE OK. USED WHEN A
;;       ;;; NEW INSTANCE OF A FRAME IS MADE BY MAKING AN
;;       ;;; IF-ADDED FOR THE INSTANCE SLOT OF THE TYPE.
;;       ;;; ALTERNATIVELY IT MIGHT BE CLEANER TO ALTER
;;       ;;; FINSTANTIATE TO RUN A SLOT CALLED NEW-INSTANCE
;;       (MAPC '(LAMBDA (X) (CHECK-AND-ASK /:FRAME X)) SLOTS))
;;
;;(DEFUN NEWFN1 (/:NEW-FRAME SLOTS)
;;       ;;; SAME AS NEWFN, BUT IT FIRST LOOKS UP
;;       ;;; THE AKO CHAIN TO FIND THE FIRST PARENT WITH
;;       ;;; A VALUE IN THE NEWFN SLOT. IT RUNS THAT VALUE
;;       ;;; AND THEN DOES WHAT NEWFN WOULD HAVE DONE.
;;       ;;; NEW-EXAMPLE WILL HAVE BOUND /:FRAME TO BE
;;       ;;; THE ONE CONTAINING THIS PARTICULAR NEWFN1
;;       ;;; AS ITS NEWFN.
;;       (FOR (/:FRAME (FIRST-PARENT-WITH-NEWFN /:FRAME))
;;            (COND (/:FRAME
;;                   (EVAL (fvalue-only2 /:FRAME 'NEWFN)) )))
;;       (NEWFN /:NEW-FRAME SLOTS))
;;
;;(DEFUN FIRST-PARENT-WITH-NEWFN (/:FRAME)
;;       (SETQ /:FRAME (*fvalue-only /:FRAME 'AKO))
;;       (COND ((NULL /:FRAME) NIL)
;;             ((*fvalue-only /:FRAME 'NEWFN) /:FRAME)
;;             (T (FIRST-PARENT-WITH-NEWFN /:FRAME))))
;;
;;(DEFUN DEFAULT-NEWFN ()
;;       (NEWFN /:NEW-FRAME (SLOTS-OF /:NEW-FRAME)))
;;
;;(DEFUN MAP-INTO ARGS
;;       (FOR (/:FRAME (OR (fvalue-only2 /:FRAME (ARG 1))
;;                        (DO-DEFINITION /:FRAME (ARG 1))))
;;            (COND ((EQ ARGS 2)
;;                   (fput-value /:FRAME (ARG 2) /:VALUE))
;;                  (T (APPLY 'MAP-INTO (LISTIFY (1- ARGS)))))))
;;
;;(DEFUN DO-DEFINITION (/:FRAME /:SLOT)
;;       (FOR (F (FINSTANTIATE (DEFINITION /:FRAME /:SLOT)))
;;            (fput-value F 'PART-OF (LIST /:FRAME /:SLOT))
;;            (fput-value /:FRAME /:SLOT F)))
;;
;;
;;(DEFUN FIND-FRAME FEXPR (L)
;;    (EXISTS X (INSTANCES (CAR L))
;;            (APPLY 'HAS-PROPERTIES
;;                   (CONS X (CDR L)))))
;;
;;(DEFUN FIND-OR-MAKE FEXPR (L)
;;    (FOR (F (APPLY 'FIND-FRAME L))
;;         (COND ((NULL F)
;;                (APPLY 'MAKE-FRAME L))
;;               (T F))))
;;
;;(DEFUN HAS-PROPERTIES FEXPR (L)
;;    (COND ((NULL (CDR L)))
;;          ((HAS-PROPERTY (CAR L)
;;                         (CADR L)
;;                         (EVAL (CADDR L)))
;;           (APPLY 'HAS-PROPERTIES
;;                  (CONS (CAR L) (CDDDR L))))
;;          (T NIL)))
;;
;;(DEFUN HAS-PROPERTY (F S V)
;;       (EQUAL V (fvalue-only2 F S)))
;;
;;(DEFUN FINSERT-VALUE (/:FRAME /:VALUE)
;;       (FOR (SLOTS (FILTER S (SLOTS-OF /:FRAME)
;;                           (SATISFIES /:VALUE
;;                                      (DEFINITION /:FRAME S))))
;;            (COND ((NULL SLOTS)
;;                   (SHOUT [@/:VALUE DOESNT FIT ANY SLOT
;;                          OF @/:FRAME])
;;                   (FUSER-INSERT-VALUE /:FRAME /:VALUE))
;;                  ((CDR SLOTS)
;;                   (SHOUT [@/:VALUE FITS SEVERAL SLOTS OF
;;                          @/:FRAME. THEY ARE])
;;                   (SHOUT SLOTS)
;;                   (FUSER-INSERT-VALUE /:FRAME /:VALUE))
;;                  (T (fput-value /:FRAME (CAR SLOTS) /:VALUE)))))
;;
;;(DEFUN SATISFIES (X DEFN)
;;       (AND (FRAME? X)
;;            (AKO? X DEFN)))
;;
;;(DEFUN FUSER-INSERT-VALUE (/:FRAME /:VALUE)
;;       (SHOUT [THE SLOTS OF /:FRAME ARE
;;              @(SLOTS-OF /:FRAME)])
;;       (DESCRIBE /:FRAME)
;;       (COND ((FRAME? /:VALUE) (DESCRIBE /:VALUE)))
;;       (FOR (SLOT (REQUEST [WHERE SHALL I PUT @/:VALUE
;;                           IN @/:FRAME]))
;;            (COND ((NULL SLOT))
;;                  (T (fput-value /:FRAME SLOT /:VALUE)))))
;;
;;
;;(DEFUN CLEAN-UP-LINKS NIL
;;        (DO-FOREACH X (*FVALUES-ONLY 'LINK 'INSTANCE)
;;                    (SET-NIL X)
;;                    (FDESTROY X)
;;                    (FDELETE 'LINK 'INSTANCE '$VALUE X)))
;;
;;(DEFUN CLEAN-UP-FRAMES NIL
;;       (DO-FOREACH X (FDESCENDANTS 'THING 'INSTANCE)
;;                   (FDELETE X 'LINKS-FROM '$VALUE)
;;                   (FDELETE X 'LINKS-TO '$VALUE)
;;                   (FDELETE X 'EVIDENCE '$VALUE)))
;;
;;
;;
;;
;;(DEFUN INITIALIZE NIL
;;       (RESET-SENTINELS-ALL)
;;        (CLEAN-UP-LINKS)
;;        (CLEAN-UP-FRAMES)
;;        (SHOUT [READY TO GO AGAIN]))


