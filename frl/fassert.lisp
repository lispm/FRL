(include declar)
;;;-*-LISP-*-
;;;*****************************************************************
;;;
;;;		          CREATING FRAMES
;;;
;;;*****************************************************************

;; Note/:  FASSERT is disabled by FASSERT = NIL.
;;        DEFRAME is disabled by DEFRAME = NIL.

(defvar fassert t)
(defvar deframe t)

(defun FASSERT fexpr (f)
  ;; typically used to read in frame in from files, i.e.
  ;;
  ;; (FASSERT <name> ( <slot1> ... )( <slot2> ...))
  ;;
  ;; NOTE/: FASSERT is non-destructive, i.e. it adds information to
  ;; frame if it already exists, running If-Added Methods, etc.
  ;; See DEFRAME for alternative quick clobber.
  ;;
  ;; NOTE/: However, since FASSERT is typically used to assert new frames
  ;; a warning is given if a frame already exists with given name (if *VERBOSE* is T).
  ;;
  ;; Returns the name of the frame.
  (cond ((not (null fassert))
	 (cond ((and *verbose* (frame/? (car f)))
		(print (car f))
		(princ '|  Warning, frame being redefined -- FASSERT|)))
	 (fadd-frame-str f)
	 (car f))))






(defun DEFRAME fexpr (l)
  ;;
  ;; (DEFRAME <name> ( <slot1> ... )( <slot2> ...))
  ;;
  ;; used to read complete frame definitions in.  No checking or extra
  ;; processing whatsoever.  Just clobbers the list structure created
  ;; by the reader onto the FRAME property of the name and enters into
  ;; *FRAMES*.
  ;; Returns the name of the frame.
  (cond ((not (null deframe))
	 (cond ((and *verbose* (frame/? (car l)))
		(print (car l))
		(princ '|  Warning, frame being redefined -- DEFRAME|)))
	 (putprop (car l) l 'frame)
	 (if *FRAME-CREATE-HOOK* (funcall *frame-create-hook* (car l)))
	 (car l))))

(defvar *frame-create-hook* 'default-frame-create-fnc)

(defvar *frame-destroy-hook* 'default-frame-destroy-fnc)

(defun DEFAULT-FRAME-CREATE-FNC (frame)
       (ifnot (memq frame *frames*) (push frame *frames*))
;       (ifnot (memq frame *new-frames*) (push frame *new-frames*))
;	This could possibly have been used in PAL --
;	check and rewrite to alter *FRAME-CREATE-HOOK* instead to perform this function.
       )

(defun DEFAULT-FRAME-DESTROY-FNC (frame)
       (if (memq frame *frames*) (setq *frames* (delq frame *frames*)))
;       (if (memq frame *new-frames*) (setq *new-frames* (delq frame *new-frames*)))
;	This could possibly have been used in PAL --
;	check and rewrite to alter *FRAME-CREATE-HOOK* instead to perform this function.
       )

;; To properly disable the maintenance of *FRAMES* as a listing of frames in the system,
;; do/:  (SETQ *FRAME-CREATE-HOOK* NIL
;;	      *FRAME-DESTROY-HOOK* NIL
;;	      FRAMES (FUNCTION
;;		      (LAMBDA () (PROG (FRAMES)
;;		                  (MAPATOMS '(LAMBDA (F) (IF (FRAME/? F) (PUSH F FRAMES))))
;;				  (RETURN FRAMES)))))



(defun FADD-FRAME-STR (input-frame-str)
       ;; merges input frame structure with any existing one.
       ;; Also used to define new frames, especially as called via FASSERT.
       ;; Returns frame name.
       (for (:frame (frame+ (findicator input-frame-str))
	     slots (fbucket input-frame-str)
             ako (flistget input-frame-str 'ako))

            ;; The AKO slot must be processed first for inheritance to work on the remainer.
            (mapc '(lambda (input-slot-str)
			   (fadd-slot-str :frame input-slot-str))
                  (cond ((null ako) slots)
			((eq ako (car slots)) slots)
			(t (cons ako (delq ako (fbucket input-frame-str))))))
            (fname :frame)))





(defun FADD-SLOT-STR (:frame input-slot-str) 
       ;; merges given slot structure into /:FRAME.
       ;; Returns the name of /:FRAME.
       (prog (:slot :facet target-slot-str target-key-str target-data-str target-comment) 
	     (setq :slot (findicator input-slot-str))
	     (setq target-slot-str (fbuild :frame :slot))

             ;; Run through keys skipping data under $VALUE facet.
             (mapc
	      '(lambda (input-key-str) 
   	          (setq :facet (findicator input-key-str))
		  ;; build key structure.
 		  (setq target-key-str (fbuild target-slot-str :facet))

 		  ;; skip values -- do those last.
		  (cond(
		   (not (eq :facet '$value))
		   ;; put in property structure and comments
		   (mapc 
		    '(lambda (input-data-str) 
		      (for (/:data (findicator input-data-str))
		       (setq target-data-str (fbuild target-key-str /:data))
		       (mapc '(lambda (input-comment) 
			       (for (/:topic (findicator input-comment))
				(setq target-comment (fbuild target-data-str /:topic))
				(mapc '(lambda (msg) (fput-msg target-comment msg))
				      (fbucket input-comment))))
			     (fbucket input-data-str))))
		    (fbucket input-key-str))

 		   (or (atom :slot)
                       ;; indirect slot and key
   	               (fput-datum (frame+ (car :slot))
  				      (cadr :slot)
				      :facet
				      `(atsign (,(fname :frame) ,:slot ,:facet)))) )))
	     (fbucket input-slot-str))
;;; continued on next page

;;; continuing...

             ;; $VALUE key, if present, must be processed last (i.e., after $IF-ADDED)
             (for (input-key-str (flistget input-slot-str '$value)
		   *replacing/?* nil)
              (setq :facet '$value)
              (cond
	       (input-key-str
		 ;; put in values and comments
                 (setq target-key-str (fbuild target-slot-str '$value))
		 (mapc 
		  '(lambda (input-data-str) 
		    (for (:value (findicator input-data-str))
		     ;; Run if-added methods only for new values.
		     (cond
		      ((not (setq target-data-str (flistget target-key-str :value)))
		       (setq target-data-str (fbuild target-key-str :value))
		       ;; Comments should be present.
		       (mapc '(lambda (input-comment) 
			       (for (/:topic (findicator input-comment))
				(setq target-comment (fbuild target-data-str /:topic))
				(mapc '(lambda (msg) (fput-msg target-comment msg))
				      (fbucket input-comment))))
			     (fbucket input-data-str))
		       ;; Run if-added methods.
                       (run-if-added-methods :value :frame :slot))
		      ;; Old value, just add comments.
		      (t
		       (mapc '(lambda (input-comment) 
			       (for (/:topic (findicator input-comment))
				(setq target-comment (fbuild target-data-str /:topic))
				(mapc '(lambda (msg) (fput-msg target-comment msg))
				      (fbucket input-comment))))
			     (fbucket input-data-str))))

		     (or (atom :slot)
                     ;; indirect slot and key
   	             (fput-datum (frame+ (car :slot)) (cadr :slot) :facet
				 `(atsign (,(fname :frame) ,:slot ,:facet)))) ))
		  (fbucket input-key-str)))))
           (return (fname :frame))))

;;; THE END.



(defun FERASE (:frame)
       ;; Tries hard to remove all traces of Frame from the data base.
       ;; Ie, 1. Flush indirect references,
       ;;     2. Run If-Removed procedures on each value.
     (prog (:slot ako *replacing/?*)
       (ifnot (frame/? :frame) (return nil))
       (setq ako (flistget :frame 'ako))
       (mapc  '(lambda (slot-str)
          	        (setq :slot (findicator slot-str))

                        ;; Run IF-REMOVED procedures on each value.
                        (mapc '(lambda (:value) (run-if-removed-methods :value :frame :slot))
                               (findicators (flistget slot-str '$value)))

                        ;; Check for INDIRECTION.
                        (cond((not (atom :slot))
                              ;; remove the indirection pointer from each key.
                              (mapc
                               '(lambda (:facet)
				   (fremove-value (car :slot)
						  (cadr :slot)
						  `(atsign (,:frame ,:slot ,:facet))))
                                (findicators (flistget (car :slot) (cadr :slot)))))))

	      ;; Do AKO slot last -- otherwise inheritance fails to get the IF-REMOVED methods
	      ;; on the others slots.
	      ;; *** Alternatively, get ALL methods first and then run them. *** 
	      (nconc (delq ako (fbucket (frame :frame))) (list ako)))

       (fdestroy :frame)))

;;     CREATE, DESTROY and INSTANTIATE (CREATE as AKO another frame)

(defun FCREATE args
       ;;
       ;; (FCREATE <name>)
       ;;
       ;; Makes an empty frame and returns its name.
       ;;
       ;; <name> = optional atomic name to be given to frame created
       ;;          if omitted, system makes up unique name
       ;;	   check is made that supplied name is unique
       ;;
       (prog (:frame)
             (cond ((> args 0)
                    (ifnot (atom (arg 1))(error '|non-atomic frame name -- FCREATE|
                                                (arg 1)
                                                'wrng-type-arg))
                    (if (frame/? (setq :frame (arg 1)))
                        (setq :frame (fgename :frame))))
                   ((setq :frame (fgename 'frame))))
             (cond(*trace-create* (ftrace-create :frame))
                  ((fcreate1 :frame)))
             (return :frame)))

(defun FCREATE1 (name)
  (putprop name (ncons name) 'frame)
  (if *frame-create-hook* (funcall *frame-create-hook* name)))


(defun FINSTANTIATE args
       ;;
       ;; (FINSTANTIATE <type> <name>)
       ;;
       ;; <type> = obligatory, frame type of which new frame to be an instance
       ;;          It can be an atom or a list of types, all of which
       ;;          the new frame will be made a-kind-of.
       ;; <name> = optional, name to be used; if omitted, system will make
       ;;	   one up; if supplied, it is checked for uniqueness (uses FCREATE).
       ;;
       ;; creates a new frame, which is instance of type and returns name.
       ;; The types must already exist.

       (prog (name types)
	  (setq types (cond((atom (arg 1))(list (arg 1)))((arg 1))))
	  (if (or (memq nil types) (select1 '(lambda (f) (not (frame/? f))) types))
	      (error '|Input not a frame -- FINSTANTIATE|))
	  (setq name (fcreate (cond((> args 1)(arg 2))
				   ((atom (arg 1))(arg 1))
				   (t (car (arg 1))))))
          (cond(*trace-instantiate* (ftrace-instantiate name types))
               ((finstantiate1 name types)))
          (return name)))

(defun finstantiate1 (name types)
       (mapc '(lambda (ako) (fput-value name 'ako ako)) types))


(defun FDESTROY (:frame)
       ;; gracelessly removes :frame from the system.
       ;; No $IF-REMOVED procedures are run, for example.
       ;; (cp. FERASE)
       (for (:frame (fname :frame))
          (cond(*trace-destroy* (ftrace-destroy :frame))
               ((fdestroy1 :frame)))
           :frame))

(defun FDESTROY1 (name)
       (remprop name 'frame)
       (if *frame-destroy-hook* (funcall *frame-destroy-hook* name)))



