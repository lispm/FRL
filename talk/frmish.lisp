From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:10:24 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08186; Thu, 2 Jun 88 14:10:23 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04394; Thu, 2 Jun 88 13:33:53 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08084; Thu, 2 Jun 88 12:45:21+0900
Date: Thu, 2 Jun 88 12:45:21+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020345.AA08084@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: frmish.l.frl
Status: RO


(include declar)
;;; (requiredf '((dsk pa) pidgin fasl)		; the interpreter.
;;; 	   '((dsk pa) ftalk0 fasl))		; contains Basic Framish.

;;;*****************************************************************
;;; 			       FRAMISH
;;;*****************************************************************

;; Extracting parts of a Path.  These are needed in the remainder of this file.

(defmacro pathframe (path)
	  `(2nd ,path))

(defmacro pathslot (path)
	  `(3rd ,path))

(defmacro pathfacet (path)
	  `(4th ,path))

;;; Nomenclature
;;   { x , y ... } denotes a mandatory choice.
;;   ( x )         denotes optional.
;;   <foo>         denotes a phrase which will serve the role of foo; often a token.
;;   PATH          defined using "frame", "slot", "facet", "for", "of", "'" or <facet name>.

;; Note/: A, The, An are always ignored!

;; "{<slot>, PATH} of <frame>"
(infixr OF
	19.
	(denfunc
	 '(for (slot /:left frame right)
	    (ifnot (path/? slot) (setq slot (buildpath nil slot nil)))
	    (cond ((or (not (path/? frame))
		       (and (null (pathslot frame))
			    (null (pathfacet frame))))
		   ;; a frame name, an expression or "the X frame"
		   (newframe frame slot))
		  (t					; a Path to a frame
		   (newframe (*pathify frame) slot))))))



;; "<frame>'s {<slot>, PATH}"
;; Equivalent to OF.
(infixr /'
	20. (denfunc '(progn
		       (check 'S)
		       (for (path right)
		        (cond ((path/? path) (newframe /:left path))
			      (t	    (buildpath /:left path nil)))))))

(define-pidgin-character '/')

;; "MY {<slot>, PATH}"
;; Equivalent to "USER's <slot>".
(prefix MY
	20. (denfunc '(progn
		       (for (path right)
		        (cond ((path/? path) (newframe /:USER-FRAME path))
			      (t	    (buildpath /:USER-FRAME path nil)))))))



;; "{ITS, HIS, HER, THEIR} {<slot>, PATH}"
;; Equivalent to "<the current frame>'S <slot>".
(prefix ITS
	20. (denfunc '(for (path right)
		        (cond ((path/? path)
			       (newframe (MERGE-CURRENT-FRAME NIL) path))
			      (t
			       (buildpath (MERGE-CURRENT-FRAME NIL) path nil))))))

(variants 'ITS   'HIS 'HER 'THEIR)

;; "<facet> for {<slot>, PATH}"
;; FOR connects facets to slots like OF connects slots to frames.
(infix FOR 19.
       (denfunc						; FOR merges the facet into the
	'(for (path right)				; path on the right.
	   (ifnot (path/? path) (setq path (buildpath nil path nil)))
	   (newfacet /:left path))))



;; "<frame> frame"; "<slot> slot"; "<facet> facet"
(suffix FRAME
	21. (denfunc '(buildpath /:left nil nil)))

(suffix SLOT
	21. (denfunc '(buildpath nil /:left nil)))

(suffix FACET
	21. (denfunc '(buildpath nil nil /:left)))

;;; PATHS

;; A PATH is a form/: (PATH frame slot facet).
;; The items in a path are indicators which locate a datum in a frame.
;; Where atoms occur, they are taken as quoted.  A special mechanism (UNQUOTE)
;; is used to cause the value of an atom to be taken as an indicator.


(defun PATH/? (x)
       ;; Test whether X is a path datatype.
       (and (listp x) (eq (car x) 'path)))

(defun BUILDPATH (frame slot facet)
       ;; Any of the arguments might be a path;
       ;; if so, extract the corresponding indicator
       ;; for inclusion in the path at that point.
       (list 'path (check-unquote (cond ((path/? frame) (2nd frame)) (t frame)))
	           (check-unquote (cond ((path/? slot)  (3rd slot))  (t slot)))
		   (check-unquote (cond ((path/? facet) (4th facet)) (t (or (recognize-facet facet) facet))))))


(defun PATHIFY (x)
       ;; Note/: Always returns a Path.
       (prog (facet)
        (cond ((path/? x)				; already is a path.
	       (return x))
	      ((setq facet (recognize-facet x))		; a recognizable facet name.
	       (return (buildpath nil nil facet)))
	      (t					; otherwise, a slot.
	       (return (buildpath nil (check-unquote x) nil))))))

(defun *PATHIFY (path)
       ;; Convert Path to *Path; ie, an embedded Path specification.
       ;; Embedded paths must always return a single value.  The default facet
       ;; of an embedded path will always be $VALUE, not /:Pathfacet.
       (rplac1 path '*PATH))

(defun NEWFACET (facet path)
       (if (path/? facet) (setq facet (4th facet)))
       (rplac4 path (check-unquote (or (recognize-facet facet) facet))))

(defun NEWSLOT (slot path)
       (if (path/? slot) (setq slot (3rd slot)))
       (rplac3 path (check-unquote slot)))

(defun NEWFRAME (frame path)
       (if (path/? frame) (setq frame (2nd frame)))
       (rplac2 path (check-unquote frame)))

(declare (special *facet-name-conversions*))

(defun recognize-facet (facet)
       (cdr (assq facet *facet-name-conversions*)))

(setq *facet-name-conversions*
      (mapcar 'cons
	      '(value  values default  defaults preference preferences
		requirement requirements if-needed  if-removed  if-added  say  discuss)
	      '($value $value $default $default $prefer    $prefer
		$require    $require     $if-needed $if-removed $if-added $say $discuss)))

(macro *PATH (call)
	;; (*PATH frame slot facet) occurring alone behaves like FRAMISH-USE *ONE*.
        `(framish-use *one* ,(construct-indicator-path* call)))

(macro PATH (call)
       ;; (PATH frame slot facet)
       ;; If a path alone should be executed, if behaves like FRAMISH-GET
      `(framish-get ,(construct-indicator-path call)))

;;; CONTEXT

;; The context is defined by /:pathframe, /:pathslot and /:pathfacet, all global variables.

(declare (special /:pathframe /:pathslot /:pathfacet))
;; NB --  CREATE can also alter the context by changing the /:pathframe.

;; "Consider {<frame>, PATH}"
(prefix CONSIDER
	2 (denfun 'set-framish-context
		  (for (path right)
		   (ifnot (path/? path)		; Otherwise, assume it is a frame name.
			  (setq path (buildpath path nil nil)))
		   (kwote (cdr path)))))


;; "Reconsider"
(nilfix RECONSIDER
	(isf 'reset-framish-context))


;; "it"; "them"; "they"
(nilfix IT
	;; returns a path corresponding to the current context as set by CONSIDER.
	'(buildpath nil nil nil))

(Variants 'it   'THEM 'THEY)



;;; "me";  "myself"
(nilfix ME '/:user-frame)

(variants 'me  'MYSELF)



;; "the current {frame, slot, facet, value, values}"
(prefix CURRENT
	15. (denfunc
	     '(for (word (caseq /:token
				 (frame ':frame) (slot ':slot) (facet ':facet)
				 (value ':value) (values ':values)))
		(cond (word (eattoken) `(unquote ,word))
		      (t right)))))

;;; CONTEXT functions.

;; The frame context is just the set of free variables/: /:pathframe, /:pathslot and /:pathfacet.
;; Whenever arguments are needed for a frame function, the path specified
;; (often incomplete) is merged with the frame context to obtain a complete set
;; of indicators.  "CONSIDER" sets the frame context.  "RECONSIDER" resets it.
;; The initial state is :frame, :slot, $value.

(defun SET-FRAMISH-CONTEXT (path)
       ;; (SET-FRAMISH-CONTEXT '(a b c)) => (setq /:pathframe 'a /:pathslot 'b /:pathfacet 'c)
       ;; for non-nil path indicators.  The others are left unchanged.
       (do ((p path         	      (cdr p))
	    (i '(/:pathframe /:pathslot /:pathfacet) (cdr i))
	    (v))
	   ((null p))
	   (setq v (car p))
	   (if v (set (car i) v)))
       (current-framish-context))

(defun CURRENT-FRAMISH-CONTEXT ()
       (merge-framish-context nil))

(defun MERGE-FRAMISH-CONTEXT (path)
       ;; returns a list of indicators "(frame slot facet)" substituting the current
       ;; values of /:Pathframe, /:Pathslot and /:Pathfacet respectively for NIL.
       (list (merge-current-frame (2nd path))
	     (merge-current-slot  (3rd path))
	     (merge-current-facet (4th path))))

(defun MERGE-CURRENT-FRAME (frame)
       (or frame /:pathframe))

(defun MERGE-CURRENT-SLOT (slot)
       (or slot /:pathslot))

(defun MERGE-CURRENT-FACET (facet)
       ;; NB/: facet gets $VALUE
       (or facet /:pathfacet))

(defun DISPLAY-FRAMISH-CONTEXT ()
       (for (indent (- (linel t) 12.))
	(do-foreach i '(/:PATHFRAME /:PATHSLOT /:PATHFACET /:FRAME /:SLOT /:FACET)
	 (shout0 i) (indent-to indent) (shout-indented (cond ((boundp i) (symeval i))
							     (t          '|<unbound>|))
						       indent))))

(defun RESET-FRAMISH-CONTEXT ()
       (setq /:pathframe '(unquote :frame)
	     /:pathslot  '(unquote :slot)
	     /:pathfacet '$value)
       nil)

(reset-framish-context)

;;; COMMANDS to access frames.

;; "Get (each, all) PATH"
(prefix GET
	2 (denfun 'framish-get < all > < each >
		  (construct-indicator-path right)))

;; "Say (each, all) PATH"
(prefix SAY
	2 (denfun 'framish-say < all > < each >
		  (construct-indicator-path right)))

;; "What {is, are} PATH"
(prefix WHAT						; Identical to "SAY"
	2 (denfun 'framish-say < is > < are >
		  (construct-indicator-path right)))

;;  "Use (each, all, {one}) PATH"
(prefix USE
	2 (denfun 'framish-use (cond ((memq /:token '(each all))
				      (EATTOKEN) '*all*)
				     (t (check 'one) '*one*))
		  (construct-indicator-path right)))

;;  "Remove {each, all, <data>} (from) PATH";  "Remove X {from} PATH"
(prefix REMOVE
	2 (denfun 'framish-remove (cond((memq /:token '(each all)) (eattoken) '*all*)
				       (t (kwote-if-atom right)))
		  < from >
		  (construct-indicator-path right)))

;; "Put X (in, as) PATH"
(prefix PUT
	2 (denfun 'framish-put (kwote-if-atom right) < in > < as >
		  (construct-indicator-path right)))

;; "Replace (all) PATH (by, with) X"
(prefix REPLACE
	2 (denfun 'framish-replace < all > (construct-indicator-path right)
		  < by > < with > (kwote-if-atom right)))

;; "Show X"
(prefix SHOW
	2 (denfun 'framish-display < me > (cons (newframe right '(path nil nil nil))
		  ''frame) '/$prpr))

;; "Create a (new) X (frame) (called <name>)"
;; NB updates path context like "Consider <name>" if a name is specified.
;; NB "Create a new example of X" calls new-example to get data values for
;;    the slots of the new frame, which is ako X. In this case, the name
;;    is currently ignored even if present.
(prefix CREATE
	2 (denfunc
	   '(list 'set-framish-context
		  (list 'list (nconc
			(buildfun < new > (cond ((istoken 'example) (check 'of)
			'new-example) ('finstantiate)) (kwote-if-atom right) < frame > )
			       (if (istoken 'called) (list (kwote right))))
			nil nil))))

;; "Request (a response to) <message>"
(prefix REQUEST
	2 (denfun 'request < response > < to > right))

;; "Display <frame,path,datum> { as a [frame,slot,facet] } { using print-fn }
;; Invokes the print function print-fn (or print if none named) on the object
;; specified. If the optional specification "as a frame" ("as a slot",
;; "as a facet") is specified, display is done at the appropriate detail level.
(prefix DISPLAY
	2 (denfunc
	   '(buildfun 'framish-display right < using right // 'print > )))

;;; Local environments
;; "With <environment> { do } <statements>"
;; <environment> is a list of "<var> AS <expr>" pairs. The expressions are
;; lambda-bound to the variables and the statements are evaluated.
(prefix WITH
	2 (denfunc
	   '(for (localenv (mapc '(lambda (x) (putprop (car x) (cons t
				  (get (car x) 'unquotable)) 'unquotable)) (checklist right))
		  action (car (buildfun < do > (/:parselist 0 'when))))
	      (cond ((and (dtpr (car action)) (eq (caar action) 'sentinel))
		    (rplac3 (car action) (mapcar 'car localenv))))
	      (mapc '(lambda (x) (putprop x (cdr (get x 'unquotable)) 'unquotable))
		     (mapcar 'car localenv))
	      (cons
		 (nconc `(lambda ,(mapcar 'car localenv)) action)
		 (mapcar 'cdr localenv)))))

;;; IS, ARE
(declare (special *is-comparative* /:if-flag))
(or (boundp '*is-comparative*) (setq *is-comparative* nil))


;; "PATH is X"

(infix IS
       5 (denfunc
	  '(cond ((memq /:token *is-comparative*)
		  (comparative-left-argument /:left))
		 ((memq /:token '(added removed found)) /:left)
		 ((and (listp /:left)			; NOT frame assignment, but setq!
		       (eq (car /:left) 'UNQUOTE))
		  (buildfun (cond (/:if-flag 'equal) (t 'setq))
			    (fix-quote-sign /:left)
			    (convert-path (kwote-if-atom right))))
		 ((buildfun
		   (cond (/:if-flag 'framish-is-/?)	; Frame query
			 (t	   'framish-is))	; Frame assignment
		   (construct-indicator-path /:left)
		   (convert-path (kwote-if-atom right)))))))



;; "PATH are X,Y,Z..."

(infix ARE
       5 (denfunc
	  '(cond ((memq /:token *is-comparative*)
		  (comparative-left-argument /:left))
		 ((buildfun
		   (cond (/:if-flag 'framish-are-/?)
			 (t        'framish-are))
		   (construct-indicator-path /:left)
		   ;; Expect a list to the right, so don't have to quote it.
		   (convert-path-all right))))))

;;; AS
;;; for assignment in local context -- "X AS expr" in WITH statement lambda-
;;; binds expr to X.

(infix AS
       18. (denfunc '(cons /:left (kwote-if-atom right))))

;;; ANY, ALL
;;; used to prefix WHERE and HAS operators for appropriate quantification.

(prefix ANY
	3 (denfunc '(rplac2 right nil)))

(prefix ALL
	3 (denfunc '(rplac2 right t)))

;;; selection operator/: "PATH where p {,q}"
;;; creates an argument list for a call to select or select1

(infix WHERE
       4 (denfunc
	  `(for (indics /:left
		preds (for (/:pathframe '(unquote /:pathframe) /:if-flag t) right))
	  (buildfun
	    'framish-select
	    'nil
	    (construct-indicator-path indics)
	    (checklist preds)))))

;;; synonyms are "with","which"

(variants 'WHERE  'with 'which)

;;; quantification operator/: "PATH has p {,q}"
;;; creates an argument list for a call to forall or exists

(infix HAS
       4 (denfunc
	  `(for (indics /:left
		preds (for (/:pathframe '(unquote /:pathframe) /:if-flag t) right))
	  (buildfun
	    'framish-quantify
	    'nil
	    (construct-indicator-path indics)
	    (checklist preds)))))

;;; synonym is "have"

(variants 'HAS 'have)

;;; Sentinel constructs
;;;
;;; Simple sentinels
;;;
;; "[when, whenever] PATH having PREDICATE < is > [added, removed, found]
;; < do > stmts"
;; creates a sentinel of the appropriate type using path and predicate given
;; as the triggering condition for the stmt. "The current value" is usable in
;; the predicate to denote the entry that caused the sentinel to trigger.
;;
;; Type of sentinel		Expression to create it
;;
;;	if-ever			when ... added
;;	when-ever		whenever ... added
;;	for-ever		when (whenever) ... found
;;	if-never		when (whenever) ... removed

;;; More complex sentinels
;;;
;; and- and or-type sentinels are realized by adding additional "PATH having
;; PREDICATE" clauses between the when (whenever) and the added, removed, or
;; found. Precedence constraints require that each such clause be parenthesized
;; in order for triggering to proceed correctly.
;;
;; in-type sentinels are realized by adding the expression "in frame-name"
;; immediately after the when or whenever. The frame name given is bound to
;; the current path frame for the duration of the predicate clauses, thus
;; need not be repeated.

(prefix WHEN
	0 (denfunc
	      '(for (condition right)
		    `(sentinel
			 ,(cdr (assq (eattoken) '((added . if-ever)
				(removed . if-never) (found . for-ever))))
			 nil
			 ,(cons (cond ((path/? condition)
				       (rplac3 (construct-indicator-path
					       condition) t))
				(condition))
				(/:parselist-unless 1 '/; 'when))))))

(prefix WHENEVER
	0 (denfunc
	      '(for (condition right)
		    `(sentinel
			 ,(cdr (assq (eattoken) '((added . when-ever)
				(removed . if-never) (found . for-ever))))
			 nil
			 ,(cons (cond ((path/? condition)
				       (rplac3 (construct-indicator-path
					       condition) t))
				(condition))
				(/:parselist-unless 1 '/; 'when))))))

(prefix IN
	18. (denfunc
	      '(progv '(frame-name /:pathframe condition) `(,right)
		     (istoken '/,)
		     (setq /:pathframe (cond ((path/? frame-name) (2nd frame-name))
					   (frame-name)))
		     (setq condition (/:parse 2))
		     (cond ((eq (car condition) 'and) (rplaca condition 'in))
		     (condition)))))

(infix HAVING
       4 (denfunc
	     '(for (indicators (construct-indicator-path /:left) /:if-flag t)
		   `(,(1st indicators) ,(2nd indicators) ,right))))

;; NB A comparative has to decide for itself how many values are appropriate;
;; then merge paths with the frame context and apply the proper access function.

(defun COMPARATIVE-LEFT-ARGUMENT (x)
       ;; Embodies the principle that use of a single token as the first argument
       ;; to a comparative refers to the value of that slot in the current pathframe,
       ;; or the facet if recognizable as one.
       (cond ((atom x) (pathify x))
	     (t x)))

(defun COMPARATIVE-WITH-IS (word)
       ;; Utility for declaring that a word is a comparative
       ;; and so could be introduced by "is".
       (setq *is-comparative* (saddq word *is-comparative*)))


;; These are defined in FTALK0.
(mapc 'comparative-with-is '( GREATER LESS EQUAL ))

;;; Access functions tailored for Framish.


(macro framish-is (call)
       ;; (framish-is indicators datum)
       (for (indicators (2nd call))
	  (cond ((equal (2nd indicators) ''slot)
		`(fput ,(1st indicators) ,(3rd call)))
	 (t `(fput ,(1st indicators) ,(2nd indicators) ,(3rd indicators) ,(3rd call))))))

(macro framish-are (call)
       ;; (framish-are indicators data)
       (for (indicators (2nd call))
	 `(mapc '(lambda (datum)
			 ,(cond ((equal (2nd indicators) ''slots)
				`(fput ,(1st indicators) datum))
			 (t `(fput ,(1st indicators) ,(2nd indicators) ,(3rd indicators) datum))))
		,(3rd call))))

(macro framish-is-/? (call)
       ; (framish-is-/? indicators datum)
	(for (indicators (2nd call))
          `(member ,(3rd call)
		    (fextract-data
		     (fdata ,(1st indicators) ,(2nd indicators) ,(3rd indicators))))))

(macro framish-are-/? (call)
	; (framish-are-/? indicators data)
	(for (indicators (2nd call))
	   `(forall datum ,(3rd call)
	      (member datum 
		      (fextract-data
			(fdata ,(1st indicators) ,(2nd indicators) ,(3rd indicators)))))))

(macro framish-select (call)
	; (framish-select all/? indicators predicates)
	(for (quant (cond ((2nd call) 'nselect) (t 'select1))
	     indicators (3rd call))
	   `(,quant '(lambda (/:pathframe) ,(cons 'and (4th call)))
	      (fextract-data (fdata ,(1st indicators) ,(2nd indicators)
	      ,(3rd indicators))))))

(macro framish-quantify (call)
	; (framish-quantify all/? indicators predicates)
	(for (quant (cond ((2nd call) 'forall) (t 'exists))
	      indicators (3rd call))
	   (append `(,quant /:pathframe (fextract-data (fdata ,(1st indicators)
	      ,(2nd indicators) ,(3rd indicators)))) (4th call))))

(macro framish-get (call)
	; (framish-get indicators)
	(for (indicators (2nd call))
	  `(fdata ,(1st indicators) ,(2nd indicators) ,(3rd indicators))))

(macro framish-use (call)
	      ;; (framish-use type indicators)
	      (for (indicators (3rd call))
	       `(funcall ',(caseq (2nd call) (*all* 'select-data) (*one* 'select-datum))
	                 (fdata ,(1st indicators) ,(2nd indicators) ,(3rd indicators)))))

(macro framish-say (call)
       ;; (framish-say indicators)
       ;; Note/: FDISCUSS-KEY1 expects a frame environment.
      (for (indicators (2nd call))
       `((lambda (:frame :slot :facet)
	   (discuss-key1 (*fdata :frame :slot :facet)))
	 ,(1st indicators) ,(2nd indicators) ,(3rd indicators))))

(defmacro framish-put (datum indicators)
	`(framish-is ,indicators ,datum))

(macro framish-replace (call)
	; (framish-replace indicators datum)
	(for (indicators (2nd call))
	  `(freplace ,(1st indicators) ,(2nd indicators) ,(3rd indicators) ,(3rd call))))

(macro framish-remove (call)
	; (FRAMISH-REMOVE datum indicators)
       ;; NB If datum = *ALL*, remove all data.
       ;; NB If slot of indicators is "slot" or "slots", remove slot, too
	(for (indicators (3rd call))
	     (cond ((or (equal (2nd indicators) ''slot) (equal (2nd indicators) ''slots))
		   `(fremove ,(1st indicators) ,(2nd call)))
	     (t `(fremove ,(1st indicators) ,(2nd indicators) ,(3rd indicators)
		    ,@(cond ((eq (2nd call) '*all*) nil)
			    (t (list (2nd call)))))))))

(macro framish-display (call)
	; (framish-display (indicators . detail-level) function)
       (cond ((and (dtpr (2nd call)) (member (cdr (2nd call)) '('frame 'slot 'facet)))
       (for (indicators (construct-indicator-path (car (2nd call)))
	 detail (caddr (2nd call)))
	 `(,(3rd call) ,(cond ((eq detail 'frame) `(frame ,(1st indicators)))
			      ((eq detail 'slot) `(assq ,(2nd indicators)
							(frame ,(1st indicators))))
			      (t `(fdata ,(1st indicators) ,(2nd indicators) ,(3rd indicators)))))))
       (t `(,(3rd call) ,(kwote-if-atom (2nd call))))))

;; Utilities for Framish access functions.


(defun construct-indicator-path (x)
       ;; Always returns a list of indicators/: (frame slot facet).
       (process-indicators (pathify x)))

(defun construct-indicator-path* (*path)
       ;; *path is (*path x y z).  The default facet is $VALUE, not /:Pathfacet.
       (if (null (pathfacet *path)) (rplac4 *path '$value))
       (process-indicators *path))

(defun process-indicators (path)
       ;; Path must be a path.
       ;; merge default context, quote if atom, unquote if indicated.
       (mapcar 'fix-quote-sign (merge-framish-context path)))

(defun fix-quote-sign (x)
       (cond ((quotable-atom x) (kwote x))		; Quote atoms.
	     ((eq (car x) 'UNQUOTE) (2nd x))		; See "the current <x>".
	     (t x)))

(defmacro unquote (x)
	  x)

(defun check-unquote (x)
       (cond ((and (atom x) (get x 'unquotable)) `(unquote ,x))
	     (x)))

(defun kwote-if-atom (x)
       (cond ((quotable-atom x) (kwote x))
	     (x)))

(defun quotable-atom (x)
       (and (atom x) (not (get x 'unquotable))))

(defun convert-path (p)
       ;; If P is a frame path, convert it to a retrieval function which expects
       ;; only ONE value.  Otherwise, undo any UNQUOTEs, and quote any atoms.
       (cond ((path/? p) `(framish-use *one* ,(construct-indicator-path p)))
	     (t (fix-quote-sign p))))

(defun convert-path-all (p)
       ;; If P is a frame path, convert it to a retrieval function which returns
       ;; a list of ALL values.  Otherwise, undo any UNQUOTEs, and quote any atoms.
       (cond ((path/? p) `(framish-use *all* ,(construct-indicator-path p)))
	     (t (fix-quote-sign p))))

(defun select-datum (data)
       ;; Checks to see if data might in fact be a datum;
       ;;  else that the 1st and only element in the list is a datum.
       ;; The test for data-ness is the presence of an "IN/:" comment which
       ;; all frame data possess.
       (cond ((atom data)			  ; Punt, and return it.
	      data)
	     ((fcomment/? data 'in/:)		  ; A single datum element.
	      (fextract-datum data))
	     ((fcomment/? (1st data) 'in/:)	  ; A list of data;
	      (fextract-datum
	       (cond ((cdr data)		  ;  more than one if fact.
		      (request-choice data))
		     (t
		      (1st data)))))
	     (t					  ; Just a list, return it.
	      data)))

(defun select-data (d)
       (cond ((atom d)				  ; return as data.
	      (list d))
	     ((fcomment/? d 'in/:)			  ; A single datum.
	      (list (fextract-datum d)))
	     ((fcomment/? (1st d) 'in/:)		  ; A list of data.
	      (fextract-data d))
	     (t					  ; A list, assume this is the data.
	      d)))

(defun request-choice (choices)
       (prog (choice)
     a  (setq choice (request2 '|Choose one of these.| choices))
	(if (member choice choices) (return choice))
	(shout0 '|You must select one of the choices.|)
	(go a)))

;;; Top level loops for testing Framish.

(defun FRAMISH-LISTEN ()
       ;; Reads Framish phrases and evaluates them, printing the result.
     (shout0 '|To stop talking FRAMISH, say {DEAF}.|)
     (catch
       (do () (nil) (/#print (request-pidgin '|<Listening>|)))
       framish-listen))


(defun FRAMISH-LISTEN-TEST ()
       ;; Reads Framish phrases and prints the Lisp translation.  They are not evaluated.
     (shout0 '|To stop talking FRAMISH, say {DEAF}.|)
     (catch
       (do () (nil)
	(catch (/#print (feed-string-to-pidgin (bracket-string (nreverse (readl2 '|{ |)))))
	       request))
       framish-listen))


(nilfix DEAF (denfunc '(throw nil framish-listen)))


(defun FTALK ()
       ;; Loads and initializes the Framish package.
       ;; Framish includes the files PIDGIN, FTALK, FTALK0, FRMISH, RULISH, TIMISH.
       ;; This is an autoload property.
       ;;(crunit dsk pa)
       (mapc '/#fload '( (rulish) (timish) ))   ; PIDGIN, FTALK0, FRMISH have been loaded
       (talk)				       ; Initialize readtable (/:PIDGINSYN)
       '*)


