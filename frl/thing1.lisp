From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:03:25 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08070; Thu, 2 Jun 88 14:03:24 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04280; Thu, 2 Jun 88 13:30:19 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08217; Thu, 2 Jun 88 12:47:02+0900
Date: Thu, 2 Jun 88 12:47:02+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020347.AA08217@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: thing1.l.frl
Status: RO


(include declar)
;;; (version)
;;; requiredef commented until fixed.
;;; (requiredf '((dsk frl) thing fasl))

;; NB/: Uses Standard Readtable.

;;; *******************************************************************
;;;			Everything is a THING
;;; *******************************************************************

(fassert THING

         (Instance	($if-needed ( (instantiate-a-frame) (type/: request)
							    (to/: instantiate)))
           ;; Evaluating the function (INSTANTIATE-A-FRAME) in a particular context
	   ;; (ie with /:FRAME bound) creates an instance of that frame
	   ;; and asks for values for each slot.
                   	($if-added   (add-ako))
                   	($if-removed (remove-ako)))

	 (Ako		($if-added   (add-instance))
              		($if-removed (remove-instance)))
           ;; THING, of course, is not AKO anything!

	 (Classification ($value (generic)))

	   ;; This frame represents a generic concept (as opposed to an individual, eg).
           ;; It is a basic kind of self knowledge.  However, it is
	   ;; something of a hack to allow generic frames to inherit this
	   ;; information from THING!

     ;; The SELF slot is the repository for knowledge about the frame AS A FRAME;
     ;; as distinguished from knowledge about the thing the frame represents.
     ;; THING, in addition to being the most general concept in FRL, is the
     ;; canonical FRAME.
     ;; Note that right at the beginning, we see a frame as having several aspects.

         (Self		($discuss ((describe-frame :frame)))
		           ;; Prints a description of the frame by asking each slot
			   ;; in the $discuss-slots facet of type DISCUSSABLE.
			($say (fname))
                           ;; Returns a procedure to be applied to the frame to produce a name.
			   ;; NB/: It has some of the functions of a name slot.

			($value ( (percentsign :frame) ))


			;; $SLOTS is used by INSTANTIATE-A-FRAME as the relevant
			;; slots to try to fill.  Another possible location is on the INSTANCE
			;; slot "close" to the function.  Probably more general though;
			;; as discourse wants it too as slots about which I can be told
			;; a value.  Perhaps have other types of slot functions/: optional,
			;; essential, etc.
			($slots ((percentsign (setminus (fheritage-slots :frame)
							(fslots 'thing)))
				 (type/: fillable)))
			($discuss-slots
				((percentsign (setminusq (fheritage-slots :frame) '(self)))
				 (type/: discussable)))
			($match)))		  ; To be announced

;;; >>>>> Rather than restrict self-knowledge to one self slot; allow (1) a self frame,
;;;       or (2) an indentification of some subsets of slots as "self" slots.
;;	  CLASSIFICATION is a good example.
;;;       It is nice to allow simple inheritance to occur among the same AKO link as the
;;;       other slots despite being semantically confusing!  We can now trigger on "self"
;;;       knowledge as well without any new mechanism.


