(include declar)
;;;-*-Lisp-*-

;;;  Fdump and fsave and ffile now work in franz,
;;;	in fact ffile has been rewritten to work in franz,
;;;	this version will probably not work in maclisp.
;;;	As for saving an frl system, just do (dumplisp file)
;;;	and that will save everything, and then do
;;;	file 
;;;	in the shell to retrieve the system.
;;;		--dhl (6/27/81)

;;; The demo function is commented, and the only change left to do
;;; is to comment part of startup function that checks versions.

;;; (version)
;;;*****************************************************************
;;;                     Saving FRAMES in a file
;;;*****************************************************************

;;; (Recall/:  *FRAMES*  is the list of all frames in the database.)

;;; FDUMPed frames will be DEFRAMEd when read.
;;; FSAVEd frames will be FASSERTed.
;;;      (Nmemonic/: Dump = Deframe, Save = faSSert)

#+Franz
(declare (special /$outport/$))

(defun FDUMP args
       ;;  (FDUMP {list of frames, default is *frames*} {file name})
       (ffile (and (plusp args) (arg 1)) (and (> args 1) (arg 2)) 'sprinter 'deframe))

(defun FDUMP1 (frames filename)
       (ffile frames filename 'PRINT 'deframe))

(defun FSAVE args
       ;;  (FSAVE {list of frames, default is *frames*} {file name})
       (ffile (and (plusp args) (arg 1)) (and (> args 1) (arg 2)) 'sprinter 'fassert))

(defun FSAVE1 (frames filename)
       (ffile frames filename 'PRINT 'fassert))

;;; The previous functions merely packages the arguments to FFILE differently.
;;; FFILE does the work.
;;;  This is rewritten for Franz by dhl.(6/27/81)

#+franz
(defun FFILE (frames filename print defun)
  ;; prints on filename (asking for one if NIL) the frames
  ;; (*frames* if NIL) using the print function supplied
  ;; (e.g., SPRINTER to grind, PRINT to not grind) as a "defun" form
  ;; (i.e., FASSERT or DEFRAME).
  (prog (framelist file terpri /$outport/$
		   prinlength prinlevel df)
	(setq framelist (cond((null frames) *frames*)
			     ((atom frames) (list frames))
			     (t frames))
	      file (cond (filename)
			 (t (request '|What file name/?|))))
	;;
	;; $outport$ is a variable used in /usr/lib/lisp/auxfns0.l
	;;	which tells where (pp)'s output is to go.
	;;	If someone changes auxfns0.l or the pp,
	;;	this will have to be changed also.
	;;	$prpr is a function used by pp to print out
	;;	structures psuedo-pretty. -dhl (6/27/81)
	;;
	(setq /$outport/$ (outfile file))

	(do-foreach f framelist
		    (/$prpr (cons defun (frame f)))
		    (terpri /$outport/$))
	(close /$outport/$)
	(return file)))

#+dec20
(defun FFILE (frames filename print defun)
       ;; prints on filename (asking for one if NIL) the frames
       ;; (*frames* if NIL) using the print function supplied
       ;; (e.g., SPRINTER to grind, PRINT to not grind) as a "defun" form
       ;; (i.e., FASSERT or DEFRAME).
       (prog (framelist file terpri
	      prinlength prinlevel df)
	     (setq framelist (cond((null frames) *frames*)
				  ((atom frames) (list frames))
				  (t frames))
		   file (mergef (cond((null filename) (request '|What file name/?|))
				     (t filename))
				(defaultf nil))
		   terpri t)
	     (cond((and (memq (cadar file) *protected-directories*)
			(not (memq (status userid) *privileged-users*)))
		   (shout0 `(|FFILE -- Sorry, but| ,(status userid) |cannot change the|
			     ,(cadar file) |directory.|))
		   (error nil)))
	     (setq file (open file 'out))
	     (setq df (dribblefile/?))		  ;get dribblefile
	     (return (prog2 (for (outfiles (cons file (sremove df outfiles))
					   ; don't dribble while filing.
				  ^R T ^W T)
				 (do-foreach f framelist
					     (funcall print (cons defun (frame f)))))
			    (truename file)
			    (close file)))))


#+lispm
(defun ffile (frames filename print defun)
  (prog (f framelist file)
	(setq framelist (cond((null frames) *frames*)
			     ((atom frames) (list frames))
			     (t frames))
	      file (cond (filename)
			 (t (request '|What file name/?|))))
	;;
	(unwind-protect
	  (progn (setq f (open file ':write))
		 (mapc (function
			 (lambda (x)
			   (grind-top-level (cons defun (frame x)) nil f)))
		       framelist))
	  (close f))))

;; The rest of this file should not work in franz or have any
;;	real meaning in franz since it is based on "ITS" environment.
;;

;;
;;	Use unix protection to decide where user can write.
;;
#-Franz
(or (boundp '*protected-directories*)
    (setq *protected-directories* '(pa frame nudge pal)))
#-Franz
(or (boundp '*privileged-users*)
    (setq *privileged-users* '(rbr)))

#-Franz
(declare (special dribblefile))

#-Franz
(defun DRIBBLEFILE/? ()
       ;;; If LISP is dribbling, returns the dribble file object; else nil.
       (and (boundp 'dribblefile) dribblefile (status filemode dribblefile)
	    dribblefile))

;;;*****************************************************************
;;;                     Dumping FRL in LISP
;;;*****************************************************************


#+maclisp
(defun SYSDUMP (fn)
       ;; PDUMPs the job in the file FN.  *VERSION* is set to fn1;
       ;; therefore, (VERSION *VERSION*) will get fn2.  When restarted, the
       ;; functions STARTUP will be run; it will in turn run any function hanging
       ;; on the *VERSION* property of STARTUP.
       (if (boundp 'remload) (remload))		  ; Flush residual loader functions.
       (mapc 'makunbound '(remload *editx*
			   /:user /:user-frame
			   :frame :slot :facet :value :values /:v /:vs))

       (setq readtable /#readtable)

       ((lambda (f)
                 (setq *version* (2nd f))
                 (putprop '*version*
                          `(,(1st f) ,(2nd f) ,(bump-version (3rd f)))
                          (2nd f))
                 (purify 0 0 'bporg)			; Purify the job.
		 (sstatus flush t)			; Flush pure LISP pages.
		 (gctwa)				; GC truly worthless atoms.
		 (setq noret nil)			; Take back unused BPS.
		 (shout0 '|Suspending...  (Update LISP;LOCK >)|)
                 (suspend				; Does a Garbage Collection
		   '|//:DDT|			; and PDUMPs the job.
                   `(,(1st f) ,(2nd f) >)))
         ((lambda (f) (or (probef `(,(1st f) ,(2nd f) >))
			  ;; If file doesn't exist, start at 0.  It will be bumped.
			  `(,(1st f) ,(2nd f) /0)))
          (mergef (namelist fn) defaultf)))

       (startup))

#-Franz
(defun DUMP-PA nil
       (sysdump '((dsk frame) pa >)))

#-Franz
(defun DUMP-FRL nil
       (sysdump '((dsk frame) frl >)))

(defun INITIALIZE-SWITCHES ()
  ;; Not necessary in initial system, since these are set when defined.  But
  ;; why not keep them around here in one place.
  (shout0 '|Resetting some switches...|)
  (setq fassert T				; Interpret FASSERT forms/?
	deframe T				; Interpret DEFRAME forms/?
	*verbose* nil				; Used to enable some printing.
	*debug* nil				; Used to enable some printing.
	*break-on-warning* nil			; Controls function WARNING.
	*confirmation-required* nil		; Controls function CONFIRM.
	*demo* nil				; Don't treat this as a demo.
	/#answer nil				; Don't number prompts(REQUEST)
	*say* T					; Do "say" constraints.
	*discuss* T				; Do "discuss" suggestions.
	*fgensym* 0		      ; Counter used to generate unique frame names.
	*trace-instantiate* nil			; Controls tracing (FTRACE).
	*trace-create* nil *trace-destroy* nil
	*trace-if-added* nil *trace-if-removed* nil *trace-if-needed* nil)
  t)

;;;  Initialization routines. - see files doit.l, & ftoplevel.l for unix
;;;	franz version of frl - initialization routines. -dhl (6/28/81)
;;;

#-Franz
(defun STARTUP nil
       ;; The first thing run on loading a dumped FRL.
       ;; Print out version number.  The system name is the value of *VERSION*.
       (cursorpos 'c)					       ; Clear screen
       ((lambda (f) (if f (princ (2nd f)) (tyo 32.) (princ (3rd f))))
        (get '*version* *version*))
       (princ '|  (LISP |)
       (princ (status lispversion))
       (princ '/))
       (terpri)

       ;; check version numbers of all files
       ((lambda (hits)
	  (if hits
	      (terpri)
	      (princ '|/; These files have more recent versions than in the current system/:|)
	      (mapc (function (lambda (hit)(printc hit))) hits)
	      (terpri)))
        (check-versions))

       (apply 'crunit (status udir))
       (setq /:user (status userid))	; will eventually want to identify user in a frame.
       (sstatus gcwho 1)		; display GCs in wholine
       (wholine-init)
       (sstatus gctime 0)		; a Lisp variable, therefore not reset automatically

       ;; Load .<SYS>. (INIT) from user's working directory (merging JCL with name first).
       (errset (apply 'readfile
                      (mergef (namelist (maknam (status jcl)))
                              `((dsk ,(status udir))
				,(maknam (nconc (ncons '/.)(explodec *version*)(ncons '/.)))
				|(INIT)|) ))
               nil)

       ;; Run any "startup" function on <sys> property of the atom STARTUP.
       ((lambda (startupfn) (cond (startupfn (apply startupfn nil))
				  (t '*)))
        (get 'startup *version*)))

#-Franz
(defprop startup pa-startup-fn pa)

#-Franz
(defun pa-startup-fn nil
       (talk)	; Load FRAMISH, TIMISH.  Makes { ... } recognizable in current readtable.
       (initialize-user-frame)		 ; Setq /:USER-FRAME
       '*)

#-Franz
(defun FUPDATE nil
       ;; After asking for confirmation, *FLOADs any files with versions
       ;; greater than dumped versions in current PA.  (uses Check-Versions).
       (mapc (function (lambda (filename) (apply '*fload filename)))
              (mapcar (function (lambda (filename) ; Convert newio to oldio format
			(append `(,(2nd filename) FASL) (1st filename))))
                      (filter filename (check-versions)
                          (ok/? `(|Reload| ,(2nd filename) /?)))))
       '*)


#-Franz
(or (boundp '*demo*) (setq *demo* nil))

;;; This function commented as conflicts with one of mine.
;;(defun DEMO fexpr (x)
;;       ;; (DEMO T)	refuses comlinks, gags all messages, starts numbering requests
;;       ;;		sets *RSET=NIL, *DEMO*=T, *SAY*=T, LINEL=80.
;;       ;; (DEMO foo)	in addition, loads file PA;DEMO foo. By convention, DEMOFN
;;       ;;		is evaled after demo file is loaded.
;;       ;; (DEMO)	= (DEMO >)
;;       ;; (DEMO nil)	undoes (DEMO T).
;;     (declare (special demofn *rset-save-during-demo linel-save-during-demo *debug-pidgin*))
;;     (prog (fn2 demofn)
;;       (setq fn2 (cond ((null x) '>)
;;		       (t (car x))))
;;       (cond ((null fn2)				  ; Just leave DEMO mode.
;;	      (setq *demo* nil *say* nil /#answer nil *rset *rset-save-during-demo)
;;	   ;   (linel t linel-save-during-demo)
;;	      (valret '|//:tctyp accept /:nomsg 1 /:continue / |)
;;	      (return nil)))
;;       (cond ((null *demo*)		  ; Not currently in DEMO mode.
;;	      (setq *demo* t *say* t /#answer t
;;		    *rset-save-during-demo *rset *rset nil
;;		 ;   linel-save-during-demo (linel t 80.)
;;		    )
;;	      (valret '|//:tctyp refuse /:nomsg 0 /:continue / |)))
;;       (cond ((eq fn2 t)(return nil)))			       ; just enter demo mode
;;       (load (list '(dsk pa) 'demo fn2))
;;       (return (eval demofn))))

;;
;;	vi - interactive call to vi to edit interpretive functions
;;		and reload them back in.  No copy of the edited
;;		functions is saved with this command.
;;		if function is not a function but a frame, it
;;		will edit that instead. (with deframe).
;;
#+franz
(declare (special /$outport/$))
;;
;;	/$outport/$ - this is the variable which tells $prpr (the
;;		internal function to pp where to print its output.)
;;	

#+franz
(defun vi fexpr (functions)
  (fed 'vi functions))

#+franz
(defun ex fexpr (functions)
  (fed 'ex functions))
;;
;;
#+franz
(declare (special /$a))

#+franz
(defun fed (editor functions)
  (prog (tmpname /$a /$outport/$)
	(setq tmpname (concat '//tmp//frl (syscall 20.)))
	(setq /$outport/$ (outfile tmpname))
	(setq /$a /$outport/$)
	(mapc (function
		(lambda (x)
		  (cond ((atom (getd x))
			 (cond ((fframe x)
				(cons 'deframe (fframe x)))))
			((eval `(pp (|P| /$a) ,x))))
		       (terpri /$a)))
	      functions)
	(close /$a)
	(eval (list 'exec editor tmpname))
	(load tmpname)
	(eval `(exec rm ,tmpname))))


