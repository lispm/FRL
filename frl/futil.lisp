From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:10:16 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08167; Thu, 2 Jun 88 14:10:14 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04375; Thu, 2 Jun 88 13:32:06 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08102; Thu, 2 Jun 88 12:45:34+0900
Date: Thu, 2 Jun 88 12:45:34+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020345.AA08102@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: futil.l.frl
Status: RO


(include declar)
;;;*****************************************************************
;;;
;;;		MISCELLANEOUS UTILITIES FOR FRAME DATA BASE
;;;
;;;*****************************************************************

(defvar frames nil)
(defvar *frames* nil)

(defun FRAMES ()
       ;; (FRAMES) returns a list of the names of all frames in existence.
       ;; The system supplied function keeps them on the atom *FRAMES*,
       ;; which is kept up to date by the *FRAME-CREATE-HOOK* and
       ;; *FRAME-DESTROY-HOOK* supplied with the system.  Alternate means
       ;; of keeping (or not keeping) track of frames should include a function
       ;; to return the list of all frames in the system.  Make it the value of
       ;; the atom FRAMES.
       (cond ((null frames) *frames*)
	     (t (funcall frames))))

(defun FNAME (F) 
       ;; F is frame-name or frame-structure.
       ;; Returns the name.
       ;; Relies on the assumption that frame names are atoms.
       (COND ((ATOM F) F) ((1st F))))

(defun FNAME/? (f)
       ;; F is a frame-name or a frame-structure.
       ;; If F is a valid frame name, returns the name (an atom);
       ;; otherwise, returns NIL.
       ;; NOTE/: This function is suspicious and if given a non-atomic
       ;; argument actually looks to see whether the frame property of its
       ;; CAR is EQ to the argument.
       (cond ((atom f)(and (get f 'frame) f))
             (((lambda (frame) (and (eq frame f) (car f)))
               (get (car f) 'frame)))))


;; This form pretty prints out frames in FRANZ. Changing frame itself
;; means changing several other functions that make use of the fact
;; FRAME returns a frame structure.

;; This function causes frames to display nicely in Franz.

(declare (special pretty-print-function))

#+lispm
(defvar pretty-print-function (function grind-top-level))

#+franz
(defvar pretty-print-function (function $prpr))

(defun show x
  (mapc (function (lambda (y)
		    (funcall pretty-print-function (frame y))
		    (terpri)))
	(listify x))
  nil)

(defun print-one-frame (x)
  (show x))

(defun FRAME (F) 
  ;; F is frame-name or frame-structure.
  ;; Returns the frame-structure itself.
  ;; Relies on the assumption that frame names are atoms.
  (cond ((null f) nil)
	((atom f)(or (get f 'frame)
		     (frame (error '|is not a frame name -- FRAME/:| f
				   'wrong-type-arg))))
	(f)))

(defun FRAME/? (f)
       ;; checks if F is a frame or not.  If so, returns the 
       ;; frame structure, otherwise NIL (not error like FRAME).
       ;; NOTE/: This function is suspicious, just like FNAME?.
       (cond ((atom f)(get f 'frame))
             ((eq (get (car f) 'frame) f) f)))

(defun FRAME+ (f)
       ;; like FRAME, except that if F is a name for a non-existent frame
       ;; FRAME+ creates it (it will have no slots, of course).
       (or (frame/? f) (frame (fcreate f))))

(defun FSLOTS (f)
       ;; returns a list of slot-names of the frame F.
       (findicators (frame f)))

(defun FRESET nil
       ;; debugging aid, clears out all frame definitions
       (if (ok/? '|Do you REALLY want to erase all the frames in the world/?|)
	   (mapc 'fdestroy (frames))))

(defun FRESET1 nil
       (if (ok/? '|Destroy all frames except THING/?|)
	   (mapc 'fdestroy (frames))
	   (load '((dsk frame) /#thing))))

(defun FCOPY (str)
       ;; makes a copy of a piece of the frame.
       (subst nil nil str))

(defun FEQUAL (f1 f2)
       ;; Returns T if frame F1 is frame F2.
       (eq (fname/? f1) (fname/? f2)))
       

;;; Many functions are evaluated in an environment where /:FRAME
;;; and /:SLOT are assumed to be bound.  FEVAL and FRUN handle this trivially.

(defun FEVAL args 
       ;; After binding /:FRAME to the value of the 2nd argument, if any,
       ;; and /:SLOT to the value of the 3rd arg, if any, and /:FACET to the value
       ;; of the 4th arg, if any;
       ;; the form which is the value of the 1st arg is evaluated.
       ;; Ie, (FEVAL <form> frame slot facet)
       (COND((null (arg 1)) nil)
	    ((= args 4)
             (for (:frame (arg 2) :slot (arg 3) :facet (arg 4))
                  (eval (arg 1))))
            ((= ARGS 3)
             (FOR (/:FRAME (ARG 2) /:SLOT (ARG 3))
                  (EVAL (ARG 1))))
            ((= ARGS 2)
             (FOR (/:FRAME (ARG 2))
                  (EVAL (ARG 1))))
            ((= ARGS 1)
             (EVAL (ARG 1)))))

(defun FRUN args
       ;; (see FEVAL for comments about binding /:FRAME and /:SLOT and /:FACET)
       ;; It differs in that if 1st arg is atomic and non-nil, it is considered
       ;; to be the name of a function and APPLYs it to NIL.  This is efficient and often used
       ;; to evaluate If-Added procedures of no argments.
       (COND((null (arg 1)) nil)
	    ((= args 4)
             (for (:frame (arg 2) :slot (arg 3) :facet (arg 4))
		  (cond((atom (arg 1)) (apply (arg 1) nil))
                       ((eval (arg 1))))))
            ((= ARGS 3)
             (FOR (/:FRAME (ARG 2) /:SLOT (ARG 3))
                  (cond((atom (arg 1)) (apply (arg 1) nil))
                       ((EVAL (ARG 1))))))
            ((= ARGS 2)
             (FOR (/:FRAME (frame (ARG 2)))
                  (cond((atom (arg 1)) (apply (arg 1) nil))
                       ((EVAL (ARG 1))))))
            ((= ARGS 1)
             (cond((atom (arg 1)) (apply (arg 1) nil))
                  ((EVAL (ARG 1)))))))

;; >>>>>> These might be turned into a macro that returns proper lambda-binding
;;        around (arg 1).  However, this would prohibit mapping and applying it,
;;        both of which are frequently done.

(defun FRUN-SAFELY args
       ;; An ERROR just returns NIL unless *DEBUG* is T.
       (for (result (errset (apply 'frun (listify args)) nil))
            (cond( result (car result))	; no errors
                 ( t (break frun-safely *debug*)))))

(defun FEVAL-SAFELY args
       ;; An ERROR just returns NIL unless *DEBUG* is T.
       (for (result (errset (apply 'feval (listify args)) nil))
            (cond( result (car result))	; no errors
                 ( t (break feval-safely *debug*)))))

;;; FRAME LINKAGES

(defun FTREE (:frame :slot)
       ;; returns the tree formed by starting at :frame
       ;;    and following the :slot link eg, AKO, INSTANCE, SUB.
       ;; A tree is represented as a list/: (root (subtree1)(subtree2)...(subtreeN)).
       ;; NOTE/: links are not inherited, to prevent loops on AKO, etc.
       (cons (fname :frame)
             (mapcar (function (lambda (f) (ftree f :slot)))
                     (*fvalues-only :frame :slot))))


(DEFUN FRINGE (F S)
       ;; returns a list of the terminal nodes of the tree defined by the S link
       ;; with root F.
       ;; *** Improve this later; I'm in a hurry. ***
       (fringe1 (ftree f s)))


(defun FRINGE1 (tree)
       ;; returns a list of the terminal nodes of tree.
       ;; where a tree is/: (root (subtree1)(subtree2)...(subtreeN)).
       (cond((null tree) nil)
            ((null (cdr tree)) (list (car tree)))
            ((mapcan (function (lambda (subtree) (fringe1 subtree))) (cdr tree)))))


(defun FCHILDREN (frame slot)
       ;;returns a list of the immediate inferiors (children, that is) of the 
       ;;frame F along given slot link
       (*fvalues-only frame slot))


(defun FDESCENDANTS (frame slot)
       ;; returns a list of ALL descendents of frame along given link(slot);
       ;; similar to FTREE except returns list, and doesn't include root.
       ((lambda (children)
            (setifyq
                (append children
                        (mapcan (function (lambda (f) (fdescendants f slot)))
                                children))))
        (fchildren frame slot)))

(defun FFAMILY (frame slot)
       (cons (fname frame) (fdescendants frame slot)))

(defun COLLECT-FAMILY (f s depth)
       ;; like FFAMILY except only goes DEPTH generations along the link S.
       (cond ((null f) nil)
	     ((zerop depth) (ncons (fname f)))
	     (t (cons (fname f)
		      (mapcan (function (lambda (frame) (COLLECT-FAMILY frame s (1- depth))))
			      (fchildren f s))))))

(defun FSIBLINGS (frame slot inverseslot)
       ;; returns a list of all the other "children" of your "parents",
       ;; where Slot = parent and InverseSlot = child.
       (delq (fname frame)
	     (setifyq (mapappend (function (lambda (parent)
					     (fchildren parent inverseslot)))
				 (fchildren frame slot)))))

(defun COLLECT-SPECIALIZATIONS (f)
       ;; returns list of F and its instances who are not individuals.
       (cond ((null f) nil)
	     ((generic/? f) (ncons (fname f)))
	     (t (mapcan 'collect-specializations (fchildren f 'instance)))))

(defun COLLECT-INDIVIDUALS (f)
       ;; returns list of all individuals which are ako F.
       (cond ((null f) nil)
	     ((individual/? f) (ncons (fname f)))       ; returns no instances of individuals
	     (t (mapcan 'collect-individuals (fchildren f 'instance)))))

(defun COLLECT-SOURCES (data)
       ;; returns a list of the sources of Data.
       (setifyq (mapappend (function (lambda (datum) (fextract-messages datum 'in/:)))
			   data)))

(defun SELECT-MOST-SPECIFIC (data)
       ;; Return list of data lowest down in the AKO hierarchy.
       ;; BUG/: There may be >1 specific data, in different branches of the hierarchy.
       ;;      This currently only returns the first encountered.
       (LIST (CDAR (SORTCAR (FOREACH D DATA (CONS (FEXTRACT-MESSAGE D 'IN/:) D)) 'AKO/?))))

(defun ADD-INVERSE-SLOT (slot)
       ;; Create Slot in the /:Value frame with value /:Frame.
       (fput-value (frame+ :value) slot (fname :frame)))

(defun REMOVE-INVERSE-SLOT (slot)
       ;; Remove /:Frame as a value for Slot in the /:Value frame.
       (if (frame/? :value)
	   (fremove-value :value Slot (fname :frame))))

(defun FLINK/? (link f1 f2)
       ;; Does Link connect f1 to f2/?  Link is a slot name whose value (or values)
       ;; is another frame name; F1 and F2 are frames.
       ;; (FLINK/? 'ako x 'meeting) is true iff a path (possibly null) exists
       ;; from x to 'meeting following only the AKO "link"; ie, if X = MEETING,
       ;; or one of the local values of X is MEETING, or FLINK/? is true when
       ;; applied to any of these local values.
       (or (eq (fname f1) (fname f2))
           (memq (fname f2) (fdescendants f1 link))))

(defun FCONNECT/? (link f1 f2)
       ;; Like FLINK except disallows the null path.
       ;; Ie, (FCONNECT/? link 'A 'A) is false.
       (cond((eq f1 f2) nil)				; an easy special case.
	    ((memq (fname f2) (fdescendants f1 link)))))


; Extracting AKO restrictions.
; 
; (AKO/? frame)              	        ; Is this an allowable variation/?  NO.
; (AKO/? :value frame) = (AKO/? :value (frame)) = (AKO/? :value ((frame)))
; (AKO/? :value (frame1 frame2 ...))	; OR
; (AKO/? :value ((frame1 frame2 ...)))	; AND
; 
; 
; (FORALL (AKO/? frame))
; (EXISTS (AKO/? frame))
; 
; Convention/: Disjunctive Normal Form 
;   ie (or (and f1 f2 ..)(and f11 f12 ..) ...)
;  represented as ( (f1 f2 ...)(f11 f12 ...) ...)

(defun AKO/? (f x) 
       ;; x is an atom, or is disjunctive normal form.
       ;; True iff F1 is a kind of F2 as established by the AKO slot (link).
       ;; EG, (AKO/? 'PA-MEETING 'ACTIVITY) is true, because PA-MEETING has a
       ;; an AKO value MEETING which has an AKO value COMMUNICATION which has
       ;; an AKO value ACTIVITY.  (AKO/? X X) is always true.
       (prog (x1 x2 z)
	     (setq x1 (cond ((atom x) (ncons x))
			    (t x)))
	  loop1
	     (cond ((null x1) (return nil)))
	     (setq x2 (cond ((atom (car x1))
			     (ncons x1))
			    (t x1)))
	  loop2
	     (cond ((null x2)
		    (setq x1 (cdr x1))
		    (go loop1))
		   ((setq z (flink/? 'ako f (car x2)))
		    (return z))
		   (t (setq x2 (cdr x2))
		      (go loop2)))))
	   

;;; FRAME NAME GENERATION

(defun FGENAME (prefix)
       ;; generates a guaranteed unique frame name by
       ;; adding a numerical suffix as needed.
       (do ((name prefix (fgensym prefix)))
           ((not (frame/? name)) name)))

;;;				Miscellany

(defun NUMBER (L)
       ;; returns the number of elements in the list L.
       (length l))

(declare (special *break-on-warning*))
(defun WARNING (message)
       ;; obeys the flag/: *BREAK-ON-WARNING*
       (if *verbose* (shout0 `(|Warning/:| ,message)))
       (break warning *break-on-warning*))

(or (boundp '*break-on-warning*) (setq *break-on-warning* nil))

;; The variables /:FRAME, /:SLOT and /:VALUE in the following functions
;; provide the proper environment for evaluating the attached methods.

;; The CATCH tags *if-added-exit* and *if-removed-exit* are provided to
;; catch throws out of a method, thus aborting the process.  It is a bug 
;; that they do not trace properly when this is done.

(defun RUN-IF-ADDED-METHODS (/:VALUE /:FRAME /:SLOT)
       (catch
	(run-if-added-methods1 :value :frame :slot (get-if-added-methods :frame :slot))
	*if-added-exit*))

(defun RUN-IF-ADDED-METHODS1 (:value :frame :slot methods)
  (for (:facet '$if-added)
       (mapc (cond (*trace-if-added* 'frun-and-trace-if-added)
		   ('frun))
	     methods)))

(defun RUN-IF-REMOVED-METHODS (:value :frame :slot)
  (catch (run-if-removed-methods1 :value 
				  :frame 
				  :slot 
				  (get-if-removed-methods :frame :slot))
	 *if-removed-exit*))

(defun RUN-IF-REMOVED-METHODS1 (/:Value /:Frame /:Slot METHODS)
       (for (:facet '$if-removed)
        (mapc (cond(*trace-if-removed* 'frun-and-trace-if-removed)('frun)) METHODS)))


;;
;; modified the if-added, if-removed sentinels to work with roles now.
;;	using the rdata functions instead of fdata. (see raccess.l)
;;		-- dhl (6/30/81)
;;

(defun GET-IF-ADDED-METHODS (f s)
       (rdata-only f s '$if-added))
       ;;(fdata-only f s '$if-added))

(defun GET-IF-REMOVED-METHODS (f s)
       (rdata-only f s '$if-removed))
       ;;(fdata-only f s '$if-removed))

(defun PREFERENCES (f s)
       (rdata-heritage f s '$prefer))

(defun REQUIREMENTS (f s)
       (rdata-heritage f s '$require))


(defun fapropos (frame)
  ; find all frames whose pname's contain a given string.
  (prog (test results)
	(setq test (exploden frame)
	      results nil)
	(mapc
	  (function
	    (lambda (candidate)
	      (do ((pname (exploden candidate))) nil	;i.e. let
		(do ((pname pname (cdr pname)))			;search for first char
		    ((null pname))
		  (cond ((= (car pname) (car test))		;found, see if test follows
			 (cond ((do ((test (cdr test) (cdr test))
				     (pname (cdr pname) (cdr pname)))
				    ((or (null pname) (null test))
				     (null test))		;t if all of test matched
				  (or (= (car test) (car pname))
				      (return nil)))
				(return (setq results (cons candidate results))))))))
		(reclaim pname t))))
	  (frames))
	(return results)))

(defun fapropos-sorted (frame)
       (sort (fapropos frame) (function alphalessp)))



