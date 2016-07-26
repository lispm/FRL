From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:10:21 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08180; Thu, 2 Jun 88 14:10:20 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04388; Thu, 2 Jun 88 13:33:03 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08215; Thu, 2 Jun 88 12:46:59+0900
Date: Thu, 2 Jun 88 12:46:59+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020346.AA08215@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: thing.l.frl
Status: RO


(include declar)
;;;-*-lisp-*-
;;; (version)				; record version number (ie, 2nd file name)


;;  These four procedures maintain the AKO<=>INSTANCE linkage by
;; insuring that corresponding values in the inverse slots appear and
;; disappear together.

;;  This essential IF-ADDED and IF-REMOVED knowledge resides in the
;; THING frame.  By convention, these procedures are FRUN in the
;; context defined by /:FRAME and /:SLOT, with /:VALUE bound to the item
;; whose addition or deletion triggered the procedure.  They are only
;; run if the value was actually added or deleted; not if an EQUAL
;; value was already there (in the case of IF-ADDED) or not there to
;; be deleted (in the case of IF-REMOVED).


(defun add-instance ()
       (fput-value (frame+ :value) 'instance (fname :frame) ))

(defun add-ako ()
       (fput-value (frame+ :value) 'ako (fname :frame) ))

(defun remove-instance ()
       (if (frame/? :value)
	   (fremove-value :value 'instance (fname :frame) )))

(defun remove-ako ()
       (if (frame/? :value)
           (fremove-value :value 'ako (fname :frame) )))

;;   USER NOTE/: Under some circumstances, it may be desirable to
;; distinguish the first time a value is added to a slot.  No special
;; mechanism is necessary for this since an IF-ADDED procedure can be
;; written to sense this itself.

;; The execution of attached IF-ADDED and IF-REMOVED procedures is assured by
;; using a special function for adding and deleting slot values/:
;; FPUT-VALUE, FREMOVE-VALUE and FREPLACE-VALUE (also FPUT, FREMOVE AND FREPLACE).

;;   	       >>>>> Possible New Design <<<<<
;; Make maintainance of ako and instance links a system default, not
;; something inherited from THING.  I.e., establish system-wide
;; semantics for the essential pairing of these slot names -- using
;; perhaps a frame for the slot names themselves.
;;
;; The general issue of which this is one instance is the amount of
;; the system's self knowledge.  Just as EXPR means something
;; (implicitly) to the LISP system, AKO and INSTANCE and others have a
;; uniform semantics within FRL.  How is this to be made explicit,
;; Will there be an AKO frame/?  What will it contain/?  How to access,
;; how to inherit, how to match, default attached procedures...

(defun INSTANTIATE-A-FRAME args
       ;; This method for creating new INSTANCEs of a frame can
       ;; receive advice (slots to be merged into the newly created frame).
       ;; It is the the FRL's default, since it is in the $If-needed facet
       ;; of the Instance slot of the Thing frame.
 (for (:frame						; Create the new frame.
         (fadd-frame-str
             (cons (finstantiate
                      (fname :frame)
		      ;; The name of the new frame can be passed as
		      ;; the value of a NAME slot structure among the
		      ;; arguments to this function.  Otherwise, the
		      ;; user is asked for one.
		      (or (inspect-datum
			   (findicators (faccess (assq 'NAME (listify args)) '$value))
			   'instantiate-a-frame)
			  ;; >>> Could inherit method to produce a name <<<
			  ;; Request unique name from the user.
			  (do ((name (request `(|What shall I call this new instance of|
						,(fname :frame) /?)
					      (fgename (fname :frame)))	; default
				     (request `(|That name has already been used. |
						|Tell me another/:|))))
			      ((not (frame/? name)) name))))
		   ;; The "advice", in the form of slots to be added to the new instance.
		   (listify args)))
      instantiating-frame (fname :frame))

   (fassign :frame)
   (terpri)
   (shout0 `(,(fname :frame) /( |an instance of| ,instantiating-frame /) |has been created.|))
   (fname :frame)))

;;; >>>>> New design/: FILL-FRAME gets the name and does FADD-FRAME-STR itself, then calls
;;;       FNEED (to/: instantiate) on an instance of the ako frame.
;;;       Where is it to look for the partially instantiated frame/?  This is a general
;;;       problem, since one can expect several passes at filling a frame.
;;; OR Make fill-frame the instantiate-a-frame.  The problem comes when want to
;;; supply some information about the new frame.  In this case -- one could
;;; say DO IT YOURSELF and be done with it.  The general problem is one of "sharing
;;; responsibility".  By this scheme, SCHEDULE would become the If-Needed method for activity!!!
;;; OR Have several methods keyed to different access functions using the "BY/:" label.

;;  (INSTANTIATE-A-FRAME) exists as a form in the Instance slot of THING, to be
;; picked up, appended to, and eval'ed by anyone wanting to pass information.
;; The function FILL-FRAME knows how to do this.

;;  Some scheme for interpreting inputs must be used.  A typical desire
;; is to have a default name for the new frame, but allow an
;; instantiating process to supply another if it wishes.  The current
;; solution is to make any argument to INSTANTIATE-A-FRAME about which
;; another process may want to supply advice exist as a slot in the
;; frame itself; incoming forms are interpreted as slots to add to the
;; new frame and can set it thus.
;;  Another Solution/: a frame for the If-Needed method containing
;; information about it; eg, default args, use of each arg, type of process.
;;  Yet another organization/: divide up the work into two places.  Let
;; FILL-FRAME be $FILL knowledge on the self slot.  Default If-Needed
;; instance generates a name and establishes the ako and calls fill. 

;;; FASSIGN tries to fill slots in frames by running its slots $if-needed methods.
;;; It uses a convention for detecting which to try based on the 'TYPE/:' comment
;;; on the method.  (TYPE/: REQUEST) says "Try me; I entail asking the user questions."
;;; FASSIGN makes two passes over the slots (not all the slots, those returned by
;;; the function FILLABLE-SLOTS) first running methods only for those slots which
;;; have no local value whatsoever, then a second time, running only the methods
;;; with a (TYPE/: REREQUEST) comment.

(defun FASSIGN (frame)
       ;; FASSIGN catches THROWs with the tags SLOT or FRAME which are produced by 
       ;;  <altmode-N> and <altmode-Q> respectively during responses to REQUEST,
       ;;  the general keyboard request function.
       ;;  It could accept a wider variety of "escape" commands/: LATER, DO, BACK,
       ;;  REMAINING, HOLD, DOWN, UP.
  (for (slots (fillable-slots frame)
        processed-slots nil
        other-slots nil)
   (if *verbose* (shout0 `(|Assigning values for the slots of| ,(fname frame) /.)))
   (catch
     (progn
       (do-foreach :slot slots
                (push :slot processed-slots)
                (cond ((null (*fvalue/? frame :slot))
                       ;; If no LOCAL values in the frame, then run REQUEST $if-needed methods.
                       (if *verbose* (shout0 `(|Trying REQUEST methods for| ,:slot |...|)))
                       ;; Catch THROWs out of method with tag SLOT, which are
                       ;; generated, for example, by the <altmode>-N command.
                       (catch (fneed frame :slot '(REQUEST)) SLOT))))
       ;; >>>>> (To/: SPECIFY) would be better! <<<<<
       (do-foreach :slot slots
                (if *verbose* (shout0 `(|Trying REREQUEST methods for| ,:slot |...|)))
                (catch (fneed frame :slot '(rerequest)) slot)))
     FRAME)
     ;; Catches any throws out of a method with the tag FRAME (<altmode>-Q).

;;;    ;; Ask about unprocessed slots, and revise those if desired using FRED.
;;;    (cond((and (setq other-slots (setminus slots processed-slots))
;;;               (affirmative/?
;;;                (progn
;;;                  (shout0 `(|The heritage for| ,(fname frame) '|includes these slots|
;;;                            |for which| |no REQUEST-type If-Needed methods were run/:|))
;;;		  (terpri)
;;;		  (printblock other-slots)
;;;                  (request '|Do you want to use FRED to further modify this frame/?|))))
;;;          (apply 'fred (list (fname frame)))))		; FRED is a fexpr.
     
   (fname frame)))


(defun FREASSIGN (frame)
       ;; like FASSIGN except asks about slots which already have local values as well.
       (catch (mapc '(lambda (slot) (catch (fneed frame slot '(REQUEST))
					   Slot))
                    (fillable-slots frame))
              Frame)
       (fname frame))

;;  Improvement/: Write new function which accepts a list of slots to
;; fill and presents them in menu format; with indication of the
;; current values, whether they have been processed yet in this
;; session; whether a value is necessary.  Actually, we may want two
;; modes (selected in menu) one of which does what we do now in
;; scanning through the slots to be filled, letting the user postpone
;; it if need be.  FASSIGN keeps, therefore, its own queue of slots to
;; be done.


(defun FILL-FRAME args
       ;; Arg 1 = name of frame to be instantiated
       ;; Args 2...N = slots to be passed to the Instantiator as args
       ;;  (see eg the behavior of INSTANTIATE-A-FRAME.)
       (cond((not (frame/? (arg 1)))
             (shout0 `(,(fname (arg 1)) |is not a frame.|))
             nil)
            ((individual/? (arg 1))
	     ;; Fill an individual by FASSIGNing each of its slots.
             (do-foreach slot (listify (- 1 args))
                 (fadd-slot-str (frame (arg 1)) slot))
             (fassign (arg 1)))

            (T
	     ;; Fill a generic frame by instantiating it (running the If-Needed methods)
	     ;; in its Instance slot.
	     (for (how-to-add (fdata-only (arg 1) 'instance '$if-needed)
                   :facet '$If-Needed)
               (cond((null how-to-add)
                     (if *verbose*
			 (shout0 `(|I don't know how to instantiate| ,(fname (arg 1)) /.)))
                     nil)
                    (T
		     (prog (value run-to-add :frame :slot)
                       (setq run-to-add (cond(*trace-if-needed* 'frun-and-trace-if-needed)
                                             (t 'frun-safely))
			     :frame (arg 1)
			     :slot 'instance)
                       (exists method how-to-add
                          ;; No order in trying If-Needed methods.
                          (setq value (funcall run-to-add
                                               (append method
						       ; The method will evaluate its args.
                                                       (foreach slot (listify (- 1 args))
							   (kwote slot))
						       ; It will be an individual.
                                                       '('(classification
							   ($value (individual))))) )))
                       (return value))))))))

(defun FILLABLE-SLOTS (frame)
       ;; Used by FASSIGN to select slots to fill.
       (setify (apply 'append				; Some slot names are lists!
		      (fextract-data (filter p-str (fdata frame 'self '$slots)
				      ;; >>>>>> Need ability to preselect and avoid unnecessary
				      ;;        evaluation of useless data.
				            (fcomment/? p-str 'type/: 'fillable))))))

(defun UNFILLED-SLOTS (frame)
       ;; returns a two-list, a partitioned set of "fillable-slots"
       ;; that do not yet have values (inherited or otherwise).
       ;; The partition is on the basis of whether or not a default is present.
       ;;   Ie,  ( <possess defaults>  <not possess defaults> )
       (do ((slots (filter :slot (fillable-slots frame)
                           (not (fdatum/? frame :slot '$value)))
                   (cdr slots))
            (slots-w-def nil)
            (slots-wo-def nil))
           ((null slots) (list slots-w-def slots-wo-def))
           (cond((fdatum/? frame (car slots) '$default)
                 (setq slots-w-def (cons (car slots) slots-w-def)))
                ((setq slots-wo-def (cons (car slots) slots-wo-def))))))


;;; >>>>> Improvement/: A separate $ORDER function to be applied to a list of any slots
;;;       which returns them in some preferred order.  This is a companion to the $SLOTS
;;;	  facet of SELF.

(defun ORDER-SLOTS (slots)
       ;; "apply" functions in $ORDER key of SELF slot to slots.
       (warning '|ORDER-SLOTS -- not yet implemented.|)
       slots)


;;; CLASSIFICATION/: Generic, Individual


(defun INDIVIDUALIZE narg
       ;; declares Frame an Individual.
       (do ((n 1 (1+ n)))
	   ((> n narg))
	   (declare (fixnum n))
	   (freplace-value (arg n) 'classification 'individual)))

(defun INDIVIDUALITY (f)
       ;; returns T   if frame F is marked as individual;
       ;;         NIL if it is marked as generic;
       ;;	  /?   otherwise.
       (for (i (fdata-only f 'classification '$value))
         (cond ((memq 'individual i) T)
	       ((memq 'generic i) NIL)
	       (t '/?))))

(defun INDIVIDUAL/? (frame)
       (true/? (individuality frame)))

(defun NON-INDIVIDUAL/? (frame)
       (false/? (individuality frame)))

(defun UNKNOWN-INDIVIDUAL/? (frame)
       (unknown/? (individuality frame)))

(defun GENERICITY (f)
       ;; returns T   if frame F is marked as generic;
       ;;         NIL if it is marked as individual;
       ;;	  /?   otherwise.
       (for (i (fdata-only f 'classification '$value))
         (cond ((memq 'generic i) T)
	       ((memq 'individual i) NIL)
	       (t '/?))))

(defun GENERIC/? (f)
       (true/? (genericity f)))

(defun NON-GENERIC/? (f)
       (false/? (genericity f)))

(defun UNKNOWN-GENERIC/? (f)
       (unknown/? (genericity f)))

;;;	Discussing FRAMES


(defun FDISCUSS narg
       ;; (FDISCUSS frame) discusses the frame.
       ;; (FDISCUSS frame slot) discusses the slot.
       ;; Each runs the procedure in the $DISCUSS facet.
       ;; Returns T iff something printed out (ie, the cursor changed position).
       (prog (:frame :slot loc methods discussion SOURCES)
        (setq :frame (arg 1)
	      :slot (cond((> narg 1) (arg 2))(t 'self))
	      loc (cursorpos))			  ; Hack to detect if anything has been said.
	(setq methods (fdata :frame :slot '$discuss))
	(setq discussion (setify (fextract-data methods)))
	(cond ((null discussion)		  ; No default discussion supplied.
	       (if (= narg 1) (shout0 `(|I have nothing to say about| ,(fname :frame) /.)))
	       (return nil))
	      ((null (cdr discussion))		  ; If only one method, use it.
	       (frun-safely (1st discussion)))
	      ((> narg 1)			  ; There is >1 method for discussing a slot;
						  ; use them all.
	       (do-foreach d discussion (frun-safely d)))
	      (t				  ; Discussing a frame with >1 method.
	       (setq sources (COLLECT-SOURCES METHODS))
	       (if (cdr sources)		  ; Not all the same source.
		   (SETQ METHODS (SELECT-MOST-SPECIFIC METHODS)
			 SOURCES (COLLECT-SOURCES METHODS))
		   (IF (CDR SOURCES)
		       (setq sources (requests `(|Discuss| ,(fname :frame)
					         |from which point of view/?|) sources))))
	       (do-foreach source sources
	        (do-foreach method (setify (fextract-data
					    (filter method methods
						    (fcomment/? method 'in/: source))))
		 (frun-safely method)))))
	;; Returns T if something printed; NIL otherwise.
	(return (not (equal loc (cursorpos))))))

;;; DESCRIBE-FRAME is the default procedure for explaining frames.  It is in
;;; the $DISCUSS facet of the SELF slot of the THING frame in the original
;;; environment.


(defun DESCRIBE-FRAME (f)
       ;; Describes the frame F by asking each of the slots to discuss itself.
       (setq f (fname f))
       (terpri)
       (shout0 `(|Discussing| ,f |...|))
       (do-foreach slot (discussable-slots f)
        (describe-slot f slot)))

(defun DISCUSSABLE-SLOTS (frame)
       ;; Used by DESCRIBE-FRAME to dtermine which slots to include in a discussion of Frame.
       (setify (apply 'append (fdata-only frame 'self '$discuss-slots))))

(defun DESCRIBE-SLOT (f s)
       ;; Describes the slot S of frame F by running FDISCUSS or
       ;; printing the slot and value (if any) whenever FDISCUSS returns nil;
       ;; i.e., whenever the slot had nothing to say for itself.
       (ifnot  (fdiscuss f s)
	       (for (values (*fvalues-only f s))
		    (if values
			(terpri)(princ s)(princ '|/:	|)
			(printblock values)))))


