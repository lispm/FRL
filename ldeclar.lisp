;;;*****************************************************************
;;;          Compiler Initilization for FRL (all files)
;;;*****************************************************************

#-lispm
(declare (macros t))

#+franz
(declare (*fexpr readfile))


(declare 
  (special *trace-create* *trace-destroy* *trace-instantiate*
	   *key-name-directory*
	   rulelevel $rulecount $rulesucceed ruledomainy
	   rulequeue
	   *new-frames* *break-on-error* :frame-sentinel
	   readtable /:rulestack /:goalstack ruledomain
	   /:comment-field	/:not-comment-field
	   slot-field		inherit-slot-field
	   $facet-field		$not-facet-field
	   *trace-if-added* *trace-if-removed* *trace-if-needed*)

  (*fexpr add-to-frame
	  debug deframe
	  iff-added iff-removed make-rule reuse-frame sentinel
	  fassert find-frame find-frames find-or-make ftrace
	  function funtrace has-properties make-frame
	  new-example process test-function)

  (SPECIAL *frames* *frame-create-hook* *frame-destroy-hook*
	   *fgensym* *request-prompter* *version* /#answer
	   *discourse-cxt* *old-discourse-cxts*
	   *day-list* *month-list* *daytime*		; FTIME
	   *-infinity* *+infinity* *once* *twice* *thrice* *forever*
	   /:user  /:user-frame  :frame  :slot  :facet  :value  :values
	   *privileged-users* *protected-directories*
	   fassert deframe *mark-data-fn* *replacing/?*
	   readtable *readtable /#readtable /[readtable ^readtable
	   defaultf prinlevel prinlength pagepause ^Q displace
	   *confirmation-required* *verbose* *debug* *debug-pidgin*
	   *help* *discuss* *check-conflicts*
	   *break-on-warning* *RSET *demo* *say* *datum-error-recovery*
	   *trace-if-needed* *trace-if-added* *trace-if-removed*	; FTRACE
	   *trace-create* *trace-destroy* *trace-instantiate*
	   *substitute-slot-for-value*			; SAY
	   *ffind-default-max*  *ffind-default-generator*  *matchlist*)   ; FFIND
  
  (FIXNUM *ffind-default-max*	; FFIND
	  *datum-error-recovery*	; FACCESS
	  *fgensym*)		; FUTIL
  
  (*LEXPR faccess fdiscuss fneed freplace-datum fput-datum
	  feval frun feval-safely frun-safely fdump fsave
	  fcreate finstantiate fheritage individualize fill-frame archetype
	  fdebug fverbose fhelp dribble ok/?
	  fsay fsay1 fsay-procedure ask ask* lask lask*
	  flistclear flistdelete flistget flistput flistreplace fbuild         ; FLIST
	  fput-datum-comment fremove-datum-comment freplace-datum-comment fcomment/?   ; FACCESS
	  ffind ffind-frame fcheck fcheckpreferences
	  instantiate-a-frame instantiate-a-super-plan instantiate-a-sub-plan
	  remember nudge list-calendar-restricted
	  sort-by-time clist cdisplay
	  concatenate stringify
	  intersect intersectq union unionq pprint pprin1 topprin1
	  /#print /#prin1 /#princ prinl prinl1 prinlc /#explodel ; LIBDOC;/#PRINT (the pretty printer)
	  schedule shout shout-indented shout-centered
	  request request1 request2 request4 requests requests1 readline request-pidgin
	  time-request interval-request
	  push-cxt save-cxt restore-cxt local-cxt
	  eventlist equiv)
  
  (*FEXPR fred fassert deframe defpattern
	  demo whois altmode
	  cload *cload fload *fload
	  edit *edit 
	  ftrace funtrace)
  ;; from FTIME
  
  ;(FIXNUM (year notype)(week notype)(day notype)(hour notype)(minute notype)
	   ;        (noon)(midnight)
	   ;        (round notype) (now)
	   ;        (day-number notype)(no-of-day notype)(month-number notype)(no-of-month notype)
	   ;	(day-accumulated notype)(day-in-month notype)
	   ;	(convert-ctime-to-rtime notype)(convert-ftime-to-rtime notype))
  
  (*LEXPR earliest latest hence the am pm extend-ctime)
  (*LEXPR compound-difference compound-sum compound-intersection compound-complement) ; FTIMES
  (SPECIAL *noon* *midnight*
	   *am* *pm*
	   *night* *morning* *afternoon* *evening*)
  
  ;; from CGOL
  
  (SPECIAL /:pidginSYN	; the readtable for {} reader
	   /:LISPSYN	; the standard lisp readtable
	   /:pidgin-character	; must make these into single-char-obj
	   /:EOF		; end-of-file marker returned by READ
	   /:TOKEN	; the "current" token
	   /:LEFT		; the "previous" parsed structure
	   /:DRBP		; declared in DEFFIX; used in ISF, RIGHT and RIGHTLIST
	   /:FUN		; declared in DEFFIX; used in ISF
	   /:ISFUN 	; declared in DEFFIX; used in ISF
	   /:GTEST	; used by GT to hold test phrase
	   )
  (*fexpr den denfun buildfun pidgin-to-lisp gt)
  (*lexpr variants)
  )

#+Franz
(eval-when (compile)
  (load 'frllib//fauxfns))

#-lispm
(eval-when (compile)
  (setq interpret-mode t)
  (initial-syntax))

#-lispm
(eval-when (compile)
  (frl-define-switches)
  (frl-syntax)
  (frl-utility-macro-load)
  ;; These readtable functions require the print file, hence are placed here.
  ;;;            (DECLARE (REQUIREDEF '((PS FRL) PRINT FASL)) (/#1SET-UP-MACROS))
  ;;;            (/#MAKE-INVERT-QUOTE-FN EXCLAMATION /!)
  ;;;            (/#MAKE-INVERT-QUOTE-FN PERCENTSIGN /%)
  ;;;            (/#MAKE-INVERT-QUOTE-FN AMPERSAND   /&)
  ;;;            (/#MAKE-INVERT-QUOTE-FN ATSIGN      /@)
  )

#+lispm
(let ()
  (load "hulk:ps:<frl.frllib>fauxfns" 'frl)
  (setq interpret-mode t)
  (frl-define-switches)
  (set-frl-syntax))

  


