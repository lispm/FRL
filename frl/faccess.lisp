From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 13:08:11 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA07882; Thu, 2 Jun 88 13:08:10 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA03733; Thu, 2 Jun 88 12:44:24 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA07952; Thu, 2 Jun 88 12:44:13+0900
Date: Thu, 2 Jun 88 12:44:13+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083@tansei.cc.u-tokyo.junet>
Message-Id: <8806020344.AA07952@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: faccess.l.frl
Status: RO


(include declar)
;;;-*-Lisp-*-
;; The Frame Data Base is implemented using Flist's (see FRAME;FLIST).
;; A FRAME is a five-level flist referenced using the following terminology/:
;; 
;; ( <name>
;; 
;; 	( <slot1> ( <facet1> ( <datum1> ( <topic1> <msg1> <msg2> ... )
;;					( <topic2> ...) ... )
;;                           ( <datum2> ... ) )
;;                ( <facet2> ... ) ... )
;; 
;; 	( <slot2> ... )
;; 
;;         ... )
;;                   
;; 
;; WHERE
;; 
;; <name>	   = frame name
;; 
;; ( <name> ... )  = frame structure (or just "frame")
;; 
;; <slot1>	   = slot name  (Note/: non-atomic slot names have special significance
;;			               with respect to "indirection".)
;; 
;; ( <slot1> ... ) = slot structure (or just "slot")
;; 
;; <facet1>	   = facet name or facet
;;		     ( Convention/: $ prefix; eg, "$VALUE" is the value facet. )
;;
;; ( <facet1> ... ) = facet structure
;; Facets used to be called "keys", and some old references may still exist.
;;
;; <datum1> = datum     (Note/: Certain non-atomic data have special significance/:
;;			       (@ ...) denotes "indirection"
;;                             (% ...) denotes "evaluation". )
;; 
;; ( <datum1> ... ) = datum structure
;; Data used to be called "properties", and some old references may still exist.
;; 
;; <topic1> = topic
;;            ( Convention/: "/:" suffix; eg, "SOURCE/:" is a topic. )
;; 
;; <msg1> = message   (Note/: need not be a flist; that is, atomic messages are acceptable.)
;; 
;; ( <topic1> <msg1> ) = <datum1>'s "Comment".
;; 
;; 
;; 
;; Note/:  Order of items in each bucket (eg, slots in frame, data in a facet) does not matter
;;        for the semantics of the frame.  None of the access functions depend on or
;;	  maintain order!

;;;		VALUES and associated FACETs of slots.
;;
;;
;; The values of a given slot in a frame are the data under the
;; 
;; 	$VALUE facet.
;; 
;; 
;; A related facet is the
;; 
;;	$DEFAULT facet
;;
;; which is used by inheritance mechanism of the FGET function if $VALUE is empty.
;; 
;; 
;; $REQUIRE  =  required constraints which values should satisfy; FCHECK runs these.
;;
;; $PREFER  =  preferred  ...
;;
;;
;; Procedural Attachments/:
;; 
;; 	Data under these facets may be atoms or lists.
;; 
;; 	If an atom is found it is taken as a function name.
;;	The function is assumed to accept no arguments
;; 	and the free variables /:FRAME, /:SLOT, /:FACET, and /:VALUE are set when it is evaluated.
;; 
;;      If a list is found, it is similarly evaluated as a functional form.
;; 
;; 
;; $IF-NEEDED  =  methods which (when run) generate possible values for slot
;; 
;; $IF-ADDED = methods run whenever new value is added to slot (by FPUT-VALUE)
;; 
;; $IF-REMOVED = methods run whenever a value is removed from slot (by FREMOVE-VALUE)

(macro FGET (call)
  ;;  (FGET frame slot} facet} keywords})
  (cond ((< (length call) 3) (error '|Too few arguments -- FGET| call))
	((= (length call) 3)			       ; (FGET frame slot)
	 (rplaca call 'FLISTGET))
	((= (length call) 4)			       ; (FGET frame slot facet)
	 (rplaca call 'FDATA1))
	((= (length call) 5)			       ;  with keywords
	 (rplaca call 'FGET-USING-KEYWORDS))
	(t (error '|Too many arguments -- FGET| call))))

(declare (*lexpr fbuild-frame))

(macro FPUT (call)
  ;;  (FPUT frame slot} facet} datum} label} message})
  (cond ((< (length call) 3) (error '|Too few arguments -- FPUT| call))
	((> (length call) 7) (error '|Too many arguments -- FPUT| call))
	((< (length call) 5)			       ; (FPUT frame slot)
	 (rplaca call 'FBUILD-FRAME))		       ; (FPUT frame slot facet)
	(t			                    
	 ; (FPUT frame slot facet datum ... )
	 (rplaca call 'FPUT-DATUM))))


(macro FREMOVE (call)
       ;;  (FREMOVE frame slot} facet} datum} label} message})
       (cond ((< (length call) 3) (error '|Too few arguments -- FREMOVE| call))
	     ((> (length call) 7) (error '|Too many arguments -- FREMOVE| call))
	     ((= (length call) 3)		   ; (FREMOVE frame slot)
	      (rplaca call 'fremove-slot))
	     ((= (length call) 4)		   ; (FREMOVE frame slot facet)
	      (rplaca call 'fremove-facet))
             ((= (length call) 5)		   
	      ; (FREMOVE frame slot facet datum)
	      (rplaca call 'fremove-datum))
	     ((> (length call) 5)		   
	      ; (FREMOVE frame slot facet datum label ...)
	      (rplaca call 'flistdelete))))


(macro FREPLACE (call)
       ;;  (FREPLACE frame slot} facet} datum} label} message})
       (cond ((< (length call) 5) (error '|Too few arguments -- FREPLACE| call))
	     ((> (length call) 7) (error '|Too many arguments -- FREPLACE| call))
;	     ((= (length call) 3)	          ; (FREPLACE frame slot)
;	      (rplaca call 'freplace-slot))
;	     ((= (length call) 4)		  ; (FREPLACE frame slot facet)
;	      (rplaca call 'freplace-facet))
             ((= (length call) 5)		  ; (FREPLACE frame slot facet datum)
	      (rplaca call 'freplace-datum))
	     ((> (length call) 5)		  ; (FREPLACE frame slot facet datum label ...)
	      (rplaca call 'flistreplace))))


(macro FPUT-STRUCTURE (call)
       (cond ((< (length call) 3) (error '|Too few arguments -- FPUT-STRUCTURE| call))
	     ((> (length call) 7) (error '|Too many arguments -- FPUT-STRUCTURE| call))
	     (t ;; Construct a form suitable of input to fadd-slot-structure.
	        (do ((form (car (last call)) `(list ,(car args) ,form))
		     (args (cdr (reverse (cddr call))) (cdr args)))
		    ((null args) `(fadd-slot-str ,(2nd call) ,form))))))


(defun FREMOVE-SLOT (f s)
       (do-foreach value (*fvalues-only f s) (fremove-value f s value))
       (flistdelete f s))

(defun FREMOVE-FACET (f s k)
       (if (eq k '$value) (do-foreach value (*fvalues-only f s) (fremove-value f s value)))
       (flistclear f s k))

(defun FREPLACE-SLOT (F S)
       (fremove-slot f s))

(DEFUN FREPLACE-FACET (f s k)
       (fremove-facet f s k))

(DEFUN FBUILD-FRAME narg
       ;;(FBUILD-FRAME f s} k)
       (setarg 1 (frame+ (arg 1)))
       (apply 'fbuild (listify narg)))

(macro FDELETE (form)
       ;; gracelessly remove a piece of a frame.
       ;; (FDELETE frame slot ... )
       ;; No side-effects, but see FERASE.
       (rplaca form 'FLISTDELETE))

(macro FCLEAR (form)
       ;; like FDELETE but leaves the last cell attached to save a CONS
       ;; when the next chunk is added.
       (rplaca form 'FLISTCLEAR))





(defun INSPECT-DATUM (datum fn)
       ;; Use this to conveniently handle the case of getting several pieces of
       ;; data and expecting only one.  The value of *DATUM-ERROR-RECOVERY*
       ;; controls the action taken if Datum is in fact datA.
       ;; 0 -- ERROR
       ;; 1 -- User chooses one.
       ;; 2 -- BREAK loop.
       ;; 3 -- Take the LAST one, printing announcement iff *VERBOSE* is non-nil.
       (cond ((cdr datum)				; There is >1 datum!
	      (caseq *datum-error-recovery*
		 (0.	; ERROR
		  (error (Stringify '|More than one datum -- | fn) datum))
		 (1.	; Choose and continue.
		  (shout0 `(|/;More than one datum --| ,fn))
		  (request2 '|Which should be returned/?| datum))
		 (2.	; BREAK
		  (shout0 `(|/;More than one datum --| ,fn))
                  (patom datum)
		  (terpri)
		  (break INSPECT-DATUM))
		 (3.	; Choose LAST and continue.
		  (for (return (car (last datum)))
		    (if *verbose* (shout0 `(|/;More than one datum --| ,fn))
				  (shout0 `(|/;Choosing| ,return)))
		    return))))
	     (t					   ; just a single datum
	      (car datum))))

(defmvar *DATUM-ERROR-RECOVERY* 2.)			; Default is BREAK.

(declare (special *fget-inherit-default* *fget-evaluate-default* *fget-indirect-default*	
		  *fget-comment-default* *fget-number-default*))


(defun FGET-USING-KEYWORDS (:frame :slot :facet keywords)
       ;;      Keyword       Default
       ;; (1) Inheritance	1
       ;; (2) Evaluate		yes
       ;; (3) Indirect		yes
       ;; (4) Comments		no
       ;; (5) Number		all
       (prog (inherit evaluate/? indirect/? comment/? number items)
	  (setq inherit   (match-keyword keywords '(H -H /0 /1 /2) *fget-inherit-default*))
	  (setq evaluate/? (match-keyword keywords '(/% -/%)        *fget-evaluate-default*))
          (setq indirect/? (match-keyword keywords '(/@ -/@)        *fget-indirect-default*))
	  (setq comment/?  (match-keyword keywords '(C -C)          *fget-comment-default*))
	  (setq number    (match-keyword keywords '(A O)           *fget-number-default*))

         ;; Now compose the proper "FGET" function.
	  ;; First, inherit the data-structures.
	  (setq items (caseq inherit
			(H  (fbucket (fheritage :frame :slot :facet)))
			(-H (fbucket (flistget  :frame :slot :facet)))
			(/0 (finherit  :frame :slot :facet))
			(/1 (finherit1 :frame :slot :facet))
			(/2 (finherit2 :frame :slot :facet))))

	  ;; Second, do necessary evaluation and indirection.
	  (setq evaluate/? (eq '/% evaluate/?)
		indirect/? (eq '/@ indirect/?))		       ; convert to True/False notation
	  (setq items
		(mapappend '(lambda (item) (fprocess-indic-by-key item evaluate/? indirect/?))
			   items))

	  ;; Third, strip off the comments, if required.
	  (if (eq '-C comment/?) (setq items (findicators1 items)))

	  ;; Fourth, check the number of returnees.
	  (if (eq 'O number) (setq items (inspect-datum items 'fget-using-keywords)))

	  (return items)))

(setq *fget-inherit-default*  '/1
      *fget-evaluate-default* '/%
      *fget-indirect-default* '/@
      *fget-comment-default*  '-C
      *fget-number-default*   'A )

(defun MATCH-KEYWORD (keywords input default)
       ;; Keywords and Input are lists (sets) of keywords.  Returns the first member of
       ;; Input in Keywords, else Default, if none occur.
       (for (hits (intersectq input keywords))
	(cond (hits (car hits))
	      (t    default))))

;;;*****************************************************************
;;;        DATUM retrieval along an arbitrary PATH
;;;*****************************************************************


(defun FDATA-ALONG (PATH :frame :slot :facet)
       ;; returns list of datum structures under given frame, slot, and facet.
       ;; Allows inheritance, evaluation and indirection.
       (fprocess-indics1 (finherit-along path :frame :slot :facet)))


(defmacro FDATA-ONLY-ALONG (PATH :frame :slot :facet)
          ;; returns list of data (without attached comments) under
          ;; given frame slot and facet.
          ;; Allows inheritance, evaluation and indirection.
          `(findicators1 (fdata-along ,PATH ,:frame ,:slot ,:facet)))


(defmacro FDATUM-ALONG/? (path :frame :slot :facet)
	  ;; non-nil iff there is at least one datum (possibly inherited).
	  `(finherit-along ,path ,:frame ,:slot ,:facet))


(defmacro FDATUM-ONLY-ALONG (path :frame :slot :facet)
          ;; same as fdata-only except expects only one and returns it directly.
          `(inspect-datum (fdata-only-along ,path ,:frame ,:slot ,:facet) 'fdatum-only-along))


(defmacro FDATUM-ALONG (path :frame :slot :facet)
          ;; same as fdata except expects only one and returns it directly.
          `(inspect-datum (fdata-along ,path ,:frame ,:slot ,:facet) 'fdatum-along))

;;;*****************************************************************
;;;		     DATUM RETRIEVAL using FINHERIT
;;;*****************************************************************

;; *** Do not make the DEFUNs into DEFMACROs; the bindings are needed ***

;;; These functions attempt to find a datum using FINHERIT, if no datum
;;; is found in the named frame.  Otherwise they return NIL.  Any ambiguity
;;; between a nonexistent datum and an Evaluated datum which returns NIL
;;; can be resolved by inspecting the facet with FDATUM/?.

(defun FDATA (:frame :slot :facet)
       ;; returns list of datum structures under given frame, slot, and facet.
       ;; Allows inheritance, evaluation and indirection.
       (fprocess-indics1 (finherit :frame :slot :facet)))


(defmacro FDATA-ONLY (:frame :slot :facet)
          ;; returns list of data (without attached comments) under
          ;; given frame, slot and facet.
          ;; Allows inheritance along AKO, evaluation and indirection.
          `(findicators1 (fdata ,:frame ,:slot ,:facet)))


(defmacro FDATUM/? (:frame :slot :facet)
	  ;; non-nil iff there is at least one datum (possibly inherited).
	  `(finherit ,:frame ,:slot ,:facet))


(defmacro FDATUM-ONLY (:frame :slot :facet)
          ;; same as FDATA-ONLY except expects only one and returns it directly.
	  ;; Course of action if >1 piece of data found is adjustable via the
	  ;; variable *DATUM-ERROR-RECOVERY* 
	  `(inspect-datum (fdata-only ,:frame ,:slot ,:facet) 'fdatum-only))


(defmacro FDATUM (:frame :slot :facet)
          ;; same as FDATA except expects only one and returns it directly.
          `(inspect-datum (fdata ,:frame ,:slot ,:facet) 'fdatum))


;;;**********************************************************************
;;;		     DATUM retrievial with NO Inheritance
;;;**********************************************************************

;;; These retrieval functions look only in the named frame (1st argument)
;;; for a datum.  They return NIL if none is found.


(defun *FDATA (:frame :slot :facet)
       ;; returns list of local data under given frame, slot, and facet.
       ;; process evaluation and indirection, but not inheritance
       (fbucket (fprocess-indics (flistget :frame :slot :facet))))


(defmacro *FDATA-ONLY (:frame :slot :facet)
          ;; returns list of local data (without attached comments) under
          ;; given frame slot and facet.
          ;; processes evaluation and indirection, but not inheritance
          `(findicators1 (*fdata ,:frame ,:slot ,:facet)))


(defmacro *FDATUM/? (:frame :slot :facet)
          `(fbucket (flistget ,:frame ,:slot ,:facet)))


(defmacro *FDATUM-ONLY (:frame :slot :facet)
          ;; same as *fdata-only, except expects only one
          `(inspect-datum (*fdata-only ,:frame ,:slot ,:facet) '*fdatum-only))


(defmacro *FDATUM (:frame :slot :facet)
          ;; same as *fdata, except expects only one
          `(inspect-datum (*fdata ,:frame ,:slot ,:facet) '*fdatum))

;;;**********************************************************************
;;;		   Utilities for accessing VALUES
;;;**********************************************************************

;; *** Do not make the DEFUNs into DEFMACROs.  The bindings are needed! ***

;;; In addition to not requiring a facet as an argument ($VALUE is assumed),
;;; these functions attempt to find a value using FINHERIT1
;;; if the named frame and slot has none.  Otherwise, NIL is returned.

(defun FDATA1 ( :frame :slot :facet )
       (fprocess-indics1 (finherit1 :frame :slot :facet)))


(defun FVALUES1 (:frame :slot)
       ;; returns a list of the $value datum structures of given frame and slot
       ;; following indirection, evaluation, and inheritance
       (fprocess-indics1 (finherit1 :frame :slot '$value)))


(defmacro FVALUES-ONLY1 (:frame :slot)
          ;; returns a list of the $value data of given frame and slot
          ;; following indirection, evaluation, and inheritance.
          `(findicators1 (Fvalues1 ,:frame ,:slot)))


(defmacro FVALUE1/? (:frame :slot)
          `(finherit1 ,:frame ,:slot '$value))


(defmacro FVALUE-ONLY1 (:frame :slot)
          ;; same as FVALUES-ONLY1 except expects only one value
          ;; and returns it directly (or NIL if none).
          `(inspect-datum (Fvalues-only1 ,:frame ,:slot) 'fvalue-only1))


(defmacro FVALUE1 (:frame :slot)
          ;; same as FVALUES1 except expects only one value structure.
          `(inspect-datum (Fvalues1 ,:frame ,:slot) 'fvalue1))



;;; Using FINHERIT2 for inheritance.

(defun FVALUES2 (:frame :slot)
       ;; returns a list of the $value datum structures of given frame and slot
       ;; following indirection, evaluation, and inheritance
       (fprocess-indics1 (finherit2 :frame :slot '$value)))


(defmacro FVALUES-ONLY2 (:frame :slot)
          ;; returns a list of the $value data of given frame and slot
          ;; following indirection, evaluation, and inheritance.
          `(findicators1 (fvalues2 ,:frame ,:slot)))


(defmacro FVALUE2/? (:frame :slot)
          `(finherit2 ,:frame ,:slot '$value))


(defmacro FVALUE-ONLY2 (:frame :slot)
          ;; same as FVALUES-ONLY2 except expects only one value
          ;; and returns it directly (or NIL if none).
          `(inspect-datum (fvalues-only2 ,:frame ,:slot) 'fvalue-only2))


(defmacro FVALUE2 (:frame :slot)
          ;; same as FVALUES2 except expects only one value structure.
          `(inspect-datum (fvalues2 ,:frame ,:slot) 'fvalue2))

;;; Essentially, renamings of corresponding *FDATUM-ONLY... functions
;;; since do NO INHERITANCE of any type, but look only in the named frame.

(defmacro *FVALUES (:frame :slot)
          ;; returns list of local values structures, but processing
          ;; indirection and evaluation.
          `(*fdata ,:frame ,:slot '$value))

(defmacro *FVALUES-ONLY (:frame :slot)
          ;; returns list of local values only, but processing
          ;; indirection and evaluation.
          `(*fdata-only ,:frame ,:slot '$value))

(defmacro *FVALUE/? (:frame :slot)
          ;; returns non-nil iff a value is present in the named frame and slot.
          `(*fdatum/? ,:frame ,:slot '$value))

(defmacro *FVALUE-ONLY (:frame :slot)
          ;; same as *FVALUES-ONLY, except expects only one value
          `(inspect-datum (*fvalues-only ,:frame ,:slot) '*fvalue-only))

(defmacro *FVALUE (:frame :slot)
          ;; same as *FVALUES, except expects only one value.
          `(inspect-datum (*fvalues ,:frame ,:slot) '*fvalue))



;;; DATUM retrieval using FINHERIT.

(defmacro FVALUES (:frame :slot)
          ;; returns list of values structures (local or inherited), processing
          ;; indirection and evaluation.
          `(fdata ,:frame ,:slot '$value))

(defmacro FVALUES-ONLY (:frame :slot)
          ;; returns list of local values only, but processing
          ;; indirection and evaluation.
          `(fdata-only ,:frame ,:slot '$value))

(defmacro FVALUE/? (:frame :slot)
          ;; returns non-nil iff a value is present in the named frame and slot.
          `(fdatum/? ,:frame ,:slot '$value))


(defmacro FVALUE-ONLY (:frame :slot)
          ;; same as FVALUES-ONLY, except expects only one value
          `(inspect-datum (fvalues-only ,:frame ,:slot) 'fvalue-only))

(defmacro FVALUE (:frame :slot)
          ;; same as fVALUES, except expects only one value.
          `(inspect-datum (fvalues ,:frame ,:slot) 'fvalue))


;;;*****************************************************************
;;;			ADDING DATA
;;;*****************************************************************



(defun FPUT-DATUM args
       ;;
       ;; (FPUT-DATUM :frame :slot :facet /:datum {topic} {msg})
       ;;
       ;; adds datum to indicated frame slot and facet
       ;; if slot name is non-atomic and facet is new, updates reference
       ;; comment, ie <topic> and <msg>, is optional
       ;; returns datum
       ;; but can be used to create new frames (because of possible forward
       ;; references in frame definitions)

       (for (:frame  (arg 1) :slot (arg 2) :facet (arg 3) /:datum (arg 4)
             slot-str (fbuild (frame (arg 1))(arg 2))
	     facet-str nil prop-str nil
	     *replacing/?* nil)
            ;; Get the facet-structure involved.  The datum will go in it.
            (setq facet-str (fbuild slot-str :facet))

            (cond ((and (eq :facet '$value )(not (setq prop-str (flistget facet-str /:datum))))
                   ;; $VALUE is recognized in order to run $IF-ADDED procedures,
                   ;; but only if the value is new (to avoid loops).
                   (setq prop-str (fbuild facet-str /:datum))	; Add the value.
                   (if (> args 4)			; Add optional comment.
		       (fput-msg (fbuild prop-str (arg 5)) (arg 6)))
                   ;; Run $IF-ADDED procedures.
                   (run-if-added-methods /:datum :frame :slot))
                  
                  (t
                   ;; All other data are processed here
                   ;; by merging and adding optional comment.
                   (setq prop-str (fbuild facet-str /:datum))
                   (cond ((> args 4)
                          (fput-msg (fbuild prop-str (arg 5))
                                    (arg 6))))))
                   
            ;; Process possible indirect slot name.
            (or (atom :slot)
                ;; Indirect slot and facet.
                (fput-datum (frame+ (car :slot))
			    (cadr :slot)
			    :facet
			    `(atsign (,(fname :frame) ,:slot ,:facet))))

            ;; Return the datum.
            /:datum))

;;; *** For VALUES, Comments are not added unless the value is actually added. ***
;;;>>>>>Reconsider this.

;;;>>>>>Instead of returning the datum, maybe it should return NIL
;;;     if the value was not added.

(macro FPUT-COMMENT (s)
       ;; An auxiliary name for fput-datum which focuses on adding the 
       ;; Comment instead of the Datum.
       ;; >>> It should NOT be just like fput-datum with respect to what it does
       ;; when the datum is already there.  Fput-datum will not add the
       ;; comment, but Fput-comment SHOULD.  Also, the comment should be in the
       ;; form of a complete comment, not a string.  A plural form too.<<<
       (rplaca s 'FPUT-DATUM))


;;;*****************************************************************
;;;			  REMOVING DATA
;;;*****************************************************************



(defun FREMOVE-DATUM (:frame :slot :facet /:datum)
       ;; Removes datum under indicated facet in slot of frame.
       ;; It returns the datum.
       ;; The If-Removed methods are collected BEFORE the value is deleted,
       ;; but run AFTER it is deleted.
       (prog (prop-str if-removed-methods *replacing/?*)
	(ifnot (frame/? :frame) (return nil))
	(setq prop-str (flistget :frame :slot :facet))
	(if (and (eq :facet '$value)
		 (flistget prop-str /:datum))
	    ;; $VALUE is recognized to run $IF-REMOVED procedures, but
	    ;; only if the value is there to be removed;
	    ;; otherwise, no action is taken.
	    (setq if-removed-methods (get-if-removed-methods :frame :slot)))
	(flistdelete prop-str /:datum)
	(if if-removed-methods
	    (run-if-removed-methods1 /:datum :frame :slot if-removed-methods))
	(return /:datum)))

;;;>>>>See comment at FPUT-DATUM about returning a value signaling whether
;;;    removal was "successful".

;;;*****************************************************************
;;;			REPLACING DATA
;;;*****************************************************************

(defun FREPLACE-DATUM args
       ;;
       ;; (FREPLACE-DATUM frame slot facet datum {topic} {msg})
       ;;
       ;; approximately equivalent to mapping FREMOVE-DATUM down all
       ;; existing data and then doing a FPUT-DATUM.
       ;; In the case of $VALUE, this can be important since any $IF-REMOVED 
       ;; procedures are run independently of actually deleting the values.
       (for (:frame (arg 1) :slot (arg 2) :facet (arg 3) /:datum (arg 4)
             facet-str (fbuild (frame (arg 1)) (arg 2) (arg 3))
             p-str nil
	     *replacing/?* T)

            (setq p-str (flistget facet-str /:datum))	;Non-NIL => p already appears.

            (cond((eq :facet '$value)
                  ;;; Special treatment for the $VALUE facet starts here.

                  ;; First, run $IF-REMOVED procedures on the old values.
                  ;; NOTE/: Unequivalent to mapping FREMOVE down values
                  ;; if procedures detect what data remains.
                   (mapc '(lambda (:value) (run-if-removed-methods :value :frame :slot))
                         ;; NB This would be more efficient if didn't have to collect
                         ;;    methods afresh for each value deleted.
                         (delete /:datum (findicators facet-str)))

                   ;; Now actually remove all the old values.
                   (flistclear facet-str)

                   ;; Restore or add the new value.
                  (cond((null p-str)
                         ;; It's a brand new value.
                         (setq p-str (fbuild facet-str /:datum))
                         ;; p-str now contains the new p-structure.
                         ;; Add optional comment.
                         (if (> args 4)
			     (fput-msg (fbuild p-str (arg 5)) (arg 6)))
                         ;; Run $IF-ADDED procedures on the new value; again
                         ;; only since it really is a new value.
                         (run-if-added-methods /:datum :frame :slot)
                         ;; Process any Indirect slot name [only if new value!]
                         (or (atom :slot)
                             (fput-datum (frame+ (car :slot))
					 (cadr :slot)
					 :facet
					 `(atsign (,(fname :frame) ,:slot ,:facet)))))

                        ;; The value was already there, so just reattach it.
                        ;; Note -- no special processing is done in this case.
                        ((fadd facet-str p-str))))	;/?/?/? Is this dangerous /?/?/?
                 
                 (t
                  ;;; The facet is NOT a $VALUE facet.
                  (flistclear facet-str)				; Delete the old.
                  (setq p-str (fbuild facet-str /:datum))	; Add the new.
                  (if (> args 4)				; Add optional comments.
		      ;; Note/: if label exists, then msg MUST be there.
		      (fput-msg (fbuild p-str (arg 5)) (arg 6)))
                  ;; Process any indirect slot specification.
                  (or (atom :slot)
                      (fput-datum (frame+ (car :slot))
				  (cadr :slot)
				  :facet
				  `(atsign (,(fname :frame) ,:slot ,:facet))))))

            ;; Return the datum.
             /:datum))


;;;		Special purpose adding and removing for VALUES

(macro FPUT-VALUE (s)
       ;;
       ;; (FPUT-VALUE :frame :slot :value {topic} {msg})
       ;;
       ;; handy packaging to add value under $value in indicated frame and slot
       ;; returns value
       ;; comment, ie <topic> and <msg>, is optional
       ;; May create new frames (because of possible forward references in frame definitions)
       `(fput-datum ,(2nd s) ,(3rd s) '$value ,.(cdddr s)))


(defmacro FREMOVE-VALUE (:frame :slot :value)
	  ;; remove value from $value facet in indicated frame and slot
	  ;; returns value
          `(fremove-datum ,:frame ,:slot '$value ,:value))


(macro FREPLACE-VALUE (f)
       `(freplace-datum ,(2nd f) ,(3rd f) '$value ,.(cdddr f)))



;;; The following three functions assume /:FRAME and /:SLOT are free variables
;;; and use them implicitly.

(macro PUT-VALUE (form)
       ;; eg, (PUT-VALUE 'foo)
       ;; This assumes /:FRAME and /:SLOT are bound (as they would be
       ;; if this were executing as a $If-needed procedure eg).
       `(fput-value :frame :slot ,(cadr form)))


(macro REMOVE-VALUE (form)
       ;; eg, (REMOVE-VALUE foo)
       ;; Assumes :frame and :slot are bound.
       `(fremove-value :frame :slot ,(cadr form)))


(macro REPLACE-VALUE (form)
       ;; eg, (REPLACE-VALUE foo)
       ;; Assumes /:FRAME and /:SLOT are bound.
       `(freplace-value :frame :slot ,(cadr form)))


;;;*****************************************************************
;;;		Internal Utilities for COMMENTS
;;;*****************************************************************

;; Note/: for adding comments to existing data at user level, use
;; optional arguments to FPUT-DATUM, FPUT-VALUE.

;; These functions manipulate directly the comments of datum-structures.

(defun FCOMMENT/? args
       ;; (FCOMMENT/? datum-str topic {msg})
       ;; A predicate to test whether a datum structure has a particular
       ;; comment; ie, a comment with the proper topic, or, if a 3rd argument
       ;; is given, that Msg is among the messages.  Returns NIL or the comment.
       (for (comment (flistget (arg 1) (arg 2)))
            (cond((or (= args 2)
                      (and (> args 2)
                           (member (arg 3) (fbucket comment))))
                   comment))))

(defun FPUT-DATUM-COMMENT args
       ;; (FPUT-DATUM-COMMENT datum-str topic {msg})
       ;; merges comment into datum-structure.
       (for (comment (fbuild (arg 1) (arg 2)))
         (mapc '(lambda (msg) (fput-msg comment msg)) (listify (- 2 args)))
	 (arg 1)))

(defun FREMOVE-DATUM-COMMENT args
       ;;(FREMOVE-DATUM-COMMENT datum-str topic {msg})
       (cond ((> args 2)				; Remove the Msgs
	      (for (comment (flistget (arg 1) (arg 2)))
	        (mapc '(lambda (msg) (fremove-msg comment msg)) (listify (- 2 args)))))
	     (t						; Flush whole comment with Topic
	      (fdelete (arg 1) (arg 2))))
       (arg 1))

(defun FREPLACE-DATUM-COMMENT args
       ;; (FREPLACE-DATUM-COMMENT datum-str topic {msg})
       (fclear (flistget (arg 1) (arg 2)))
       (apply 'fput-datum-comment (listify args)))

;; ** FADD-COMMENT is included only because it is mentioned in The FRL Manual. **

(defun Fadd-comment (datum comment)
       ;; merges the comment (ie, label and messages) into the datum.
       ;; returns the modified datum.
       (apply 'fput-datum-comment
	      `(,datum ,(findicator comment) ,@(fbucket comment))))

;; **Two internal functions**
(defun FPUT-MSG (comment msg)
       ;; merge msg into comment, but do not make msg an flist.
       ;; This is special because messages are at the maximum depth
       ;; in a frame, and will not themselves be further accessed.
       ;; And besides, they look better without the extra ().
       (ifnot (memq msg (fbucket comment))
              (nconc comment (ncons msg)))
       comment)

(defun FREMOVE-MSG (comment msg)
       ;; excise msg from comment.
       (rplacd comment (delq msg (fbucket comment))))

;;;*******************************************************************
;;;			INDIRECTION AND EVALUATION
;;;*******************************************************************
;;
;; Indirection and Evaluation of data is denoted by certain
;; special non-atomic indicators/:
;; 
;; 
;; (PERCENTSIGN <sexp> )   =  evaluate the s-exp to get the actual datum.
;;                            N.B. % macro
;; 
;; (ATSIGN ( <frame> <slot> <facet> ) ) =  substitute the data in the indicated
;; 		   	                   frame, slot and facet.
;;                                         N.B. @ macro
;;
;; In the case of indirection, the remote slot usually has a non-atomic
;; indicator name of the form/:
;;
;; (<remote-frame>
;;	( (<frame> <slot>) ($FACET1 ...)
;;			   ($FACET2 ...) ))
;;
;; Whenever a non-atomic slot name is created by FASSERT (FADD-SLOT-STR) or FPUT-VALUE
;; or FREPLACE-VALUE or FPUT-DATUM, indirection pointers are put to it in each corresponding
;; facet of the frame and slot in the non-atomic indicator, e.g.
;;
;; (IRA ((MEETING TIME) ($PREFER 12)))
;;
;; (RBR ((MEETING TIME) ($PREFER 1)))
;;
;; (MEETING (TIME ($PREFER (@(IRA (MEETING TIME) $PREFER))
;;			   (@(RBR (MEETING TIME) $PREFER)))))
;;
;; 

;;; Does having an indirection pointer fill a slot constitute having a value even
;;; if the target is empty/?  YES.
;;    The reason for this is in the current design, processing data with respect to
;;    indirection and evaluation is done AFTER they are accessed by FINHERIT.
;;    For the purpose of inheritance, the existence of the evaluate or indirect
;;    form is considered to be a value.

(defun FACCESS args
       ;;
       ;; (FACCESS <flist> <ind1> <ind2> ...)
       ;;
       ;; <flist> = flist structure or atomic frame name
       ;; <ind1> to <indn> = optional indicator path
       ;;
       ;; Like FLISTGET except process non-atomic indicators for
       ;; evaluation and indirection on the returned flist.
       ;; That is, it returns only local values; it does NOT inherit.
       ;;
       (do ((i 2 (1+ i))
            (flist (frame (arg 1))
                   (fassoc (arg i) flist)))
           ((or (null flist)(> i args))
            (fprocess-indics flist))
           (declare (fixnum i))))



(defun FPROCESS-INDIC (item)
       ;; A basic utility function to process special indicators on an
       ;; item (a datum structure).  Allowable indicators are/:
       ;; INDIRECTION (@) and EVALUATION (%).
       ;; Returns the --LIST-- of items, since more than one
       ;; -can- result from processing indirection.
       (for (indic (findicator item))
            (cond ((atom indic) (ncons item))

                  ;; EVALUATION
                  ((eq (car indic) 'percentsign)(ncons (cons (eval (cadr indic))
							     (fbucket item))))
                  ;; INDIRECTION		>>>> TO DO/: allow % inside @ <<<<
                  ((eq (car indic) 'atsign)
                   ;; Add a comment (IN/: <frame>).
                   (funcall *mark-data-fn* (append (fbucket (apply 'faccess (2nd indic))) nil)
                         ;; Copy top level before attach comment, so doesn't appear
                         ;; in the source frame.
			                 (caadr indic)))

                  ;; otherwise, no special meaning
                  ((ncons item)))))

(defun FPROCESS-INDIC1 (indic)
       ;; like FPROCESS-INDIC except works on lone indicators instead of entire items.
       ;; It returns a --LIST-- of indicators, since such can result from processing
       ;; indirection pointers.
       (cond((atom indic) (ncons indic))
            ;;Evaluation
            ((eq (car indic) 'percentsign)
             (ncons (eval (cadr indic))))
            ;;Indirection		TO DO/: allow % inside @
            ((eq (car indic) 'atsign)
             (findicators (apply 'faccess (2nd indic))))
            ;; undistinquished, non-atomic indicator
            ((ncons indic))))


(defun FPROCESS-INDICS (flist)
       ;; same as FPROCESS-INDICS1 except takes whole flist
       (prog (bucket1 bucket2)
             (setq bucket1 (fbucket flist)
                   bucket2 (fprocess-indics1 bucket1))
             (return (cond ((eq bucket1 bucket2) flist)
                           ((cons (findicator flist) bucket2))))))

(defun FPROCESS-INDICS1 (bucket)
       ;; auxiliary function which process indirection and evaluation on
       ;; indicators of items in given bucket
       ;; returns a bucket (copied if any changes)
       ;; NOTE/: efficiency hack, first checks if any special indicators
       ;; present, so don't needlessly do copying.
       (do ((items bucket (cdr items)))
           ((null items) bucket)
           (and (not (atom (findicator (car items))))
                (memq (caaar items) '(atsign percentsign))
                (return
                 ;; need to do complete copy, since there are indicators to be processed.
                 (do ((items bucket (cdr items))
                      (nbucket nil (nconc nbucket
                                          (fprocess-indic (car items)))))
                     ((null items) nbucket))))))



;; See FGET-USING-KEYWORDS for the use of this version of Fprocess-Indic.

(defun FPROCESS-INDIC-BY-KEY (item evaluate/? indirect/?)
       ;; Just like FPROCESS-INDIC except doesn't automatically process marked indicators.
       (for (indic (findicator item))
            (cond ((atom indic) (ncons item))

                  ;; EVALUATION
                  ((and evaluate/? (eq (car indic) 'percentsign))
		   (ncons (cons (eval (cadr indic)) (fbucket item))))

                  ;; INDIRECTION		TO DO/: allow % inside @
                  ((and indirect/? (eq (car indic) 'atsign))
                   ;; Add a comment (IN/: <frame>).
                   (funcall *mark-data-fn* (append (fbucket (apply 'faccess (2nd indic))) nil)
                         ;; Copy top level before attach comment, so doesn't appear
                         ;; in the source frame.
			                 (caadr indic)))

                  ;; otherwise, no special meaning
                  ((ncons item)))))

;;;*****************************************************************
;;;                   Using IF-NEEDED procedures
;;;*****************************************************************


(defun FNEED args
       ;; (FNEED frame slot {types})
       ;; runs $IF-NEEDED methods having a "TYPE/:" comment among TYPES
       ;; until one is found which returns a value.  This value is returned
       ;; by FNEED.  This function is mentioned in the FRL Manual.
       (prog ( value run-if-needed methods types :frame :slot :facet )
         (setq run-if-needed (cond(*trace-if-needed* 'frun-and-trace-if-needed)('frun-safely)))
	 (setq :frame (arg 1) :slot (arg 2) :facet '$if-needed)	; Frame Environment for method.
	 (setq methods  (fdata :frame :slot '$if-needed))
	 (setq types (if (> args 2) (arg 3)))
         (exists method (or (restrict-data methods 'type/: types)
			    (restrict-data methods 'to/:   types))
            (setq value (funcall run-if-needed (findicator method))))
         (return value)))


(defun RESTRICT-DATA (data LABEL messages)
       ;; DATA is list of datum elements presumed to have comments with
       ;; a label LABEL.   MESSAGES is a list of possible messages.
       ;; Returns those DATA having any message among those prescribed.
       ;; *** Could beef this up to take AND-OR messages a la AKO/?.
       (cond((null messages)
             ;; no restriction => return everything.
             data)
            (t (filter datum data
                 (intersect/? (fbucket (fcomment/? datum label))
			     messages)))))

(defun FVALUE-AS-NEEDED (f s)
       (or (fdatum-only f s '$value)
           (fdatum-only f s '$default)
           (fneed f s '(immediate))))

(defun FVALUES-AS-NEEDED (f s)
       (or (fdata-only f s '$value)
           (fdata-only f s '$default)
           (for (v (fneed f s '(immediate)))
	     (if v (list v)))))

;; >>> What if a slot has several values and you wish to have them all -- how manipulate the
;;     If-Needed methods in this case <<<


