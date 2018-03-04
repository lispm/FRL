(include declar)
;;; (version)						; -*-Lisp-*-
;;; (declare (pa-readtable))

;;;*****************************************************************
;;;                       The ASK function 
;;;*****************************************************************

;;; ASK is widely used IF-NEEDED method for filling in the value of a slot.
;;; True to its name, it does so by requesting a value from the user.

(declare (special *ask1 *ask2 *ask-options *ask-request-fcn	; Hooks in ASK function
                  reply options option-structures	; Make ASK's local variables
		  count query replace/?))		; available to the "hooks".

;;; The hooks are *ASK-OPTIONS, *ASK2, *ASK-REQUEST-FCN and *ASK1.
;;;  *ASK-OPTIONS is evaluated to produce options to be presented to the user.
;;;  *ASK2 is evaluated to discuss the options to be presented to the
;;;   user, or the properties the reply should possess.
;;;  *ASK-REQUEST-FCN is used to query the user and read the response from the terminal.
;;;  *ASK1 is evaluated after the reply is typed in but before adding it to the slot.  

;;; >>>>> Improvement/: Make these hooks optional arguments, instead of global variables. <<<<<
;;; >>>>> Improvement/: Combine *ASK2 and *ASK-OPTIONS. <<<<<

(declare (special *ask-delete* *ask-use-suggestions* *confirmation-required* *ask-quietly*))

(ifnot (boundp '*ask-delete*) (setq *ask-delete* NIL))
(ifnot (boundp '*ask-use-suggestions*) (setq *ask-use-suggestions* NIL))
(ifnot (boundp '*confirmation-required*)(setq *confirmation-required* NIL))
(ifnot (boundp '*ask-quietly*) (setq *ask-quietly* NIL))

;;; Four flags are referenced inside ASK/: *ASK-DELETE*, *ASK-USE-SUGGESTIONS*,
;;; *CONFIRMATION-REQUIRED* and *ASK-QUIETLY*.
;;;  *ASK-USE-SUGGESTIONS* -- Should ASK use suggestions and not bother to ask for responses/?
;;;  *ASK-DELETE* -- Should ASK try to delete values if response to query is NIL/?
;;;  *CONFIRMATION-REQUIRED* -- Should ASK ask user to verify input/?
;;;  *ASK-QUIETLY* -- Should ASK print a prompt/?
;;; Initially, these modes are OFF.



(defun ASK args

       ;; (ASK query) requests a value and then FREPLACEs it as the value
       ;;   in the current /:FRAME and /:SLOT.
       ;; (ASK query 5) requests 5 values and FPUTs them in the current slot.
       ;; (ASK query NIL) continues until the user responds "No".

       (prog (reply replies remaining-replies count options query replace/?)
	   (declare (fixnum count))
           (setq count (cond((> args 1)			       ; count = how many values
                             (setq replace/? nil)	       ; replace or add values/?
                             (cond((null (arg 2)) (minus 1))   ; negative count => "several"
                                  ; count=-1 if arg2 is nil
                                  ((or (zerop (arg 2)) (minusp (arg 2))) (return nil))
				  (t (arg 2))))
                            ;; One arg => count = 1.
                            (t (setq replace/? T) 1)))

           (setq query  (cond((and (plusp args) (arg 1)))
                             ;; The default query follows/:
                             (t `(|What is| ,(or (and replace/? '|the|) '|a|)
				  ,:slot |of| ,(fname :frame) /?))))
           (ifnot *ask-quietly* (shout0 query))

           (setq option-structures (cond((and (boundp '*ask-options) *ask-options)
					 (funcall *ask-options :frame :slot))
					(T (ask-discuss-values :frame :slot)))
           ;; ASK-DISCUSS-VALUES returns relevant default structures, if any, after
           ;;   presenting them to the user.
		 options (setifyq (fextract-data option-structures)))

           ;; Here is the place to comment about the upcoming request for value.
           ;; Eg, Present the applicable preferences. and/or requirements.
           ;; Any functional value of *ASK2 is run (OPTIONS is bound by this time).

           (if (and (boundp '*ask2) *ask2) (funcall *ask2 :frame :slot))

    A      (setq replies (catch
			  (cond ((and *ask-use-suggestions* options
				      (ask-use-suggestions options)))
				(*ask-use-suggestions*		; Quit if suggestions failed.
				 (ifnot *ask-quietly* (shout0 `|Suggestions failed.|))
				 NIL)
				((and (boundp '*ask-request-fcn) *ask-request-fcn)
				 (funcall *ask-request-fcn nil options
					  (ask-says-options options)))
				(t
				 (REQUESTS nil options (ask-says-options options))))
			  ASK-CATCH-SUGGESTIONS))

           ;; T or NIL as replies cause immediate return, though NIL first allows chance
           ;; to delete any local values.
           (cond((eq replies T)
                 (return nil))
                ((eq replies NIL)
                 (cond((and *ask-delete*		; Enabled/?
                            (fvalue/? :frame :slot)	; There are values/?
                            (ok/? '|Delete values/?|))
                       ;;;>>> fix this to loop through values if there is > 1.
		       (fremove :frame :slot '$value)))
                 (return nil))
		((and (boundp '*ask-request-fcn) *ask-request-fcn) ; Assume only one returned.
		 (setq replies (list replies))))
	   (setq remaining-replies replies)

b          (setq reply (car remaining-replies)
		 remaining-replies (cdr remaining-replies))
           (if (null reply) (return replies))

           ;; Here is the place for a function to massage the reply if it so
           ;; desires.  Any non-nil *ASK1 is applied to Reply.
           ;; Only in the case it returns T does Reply get added to the slot.
           (and (boundp '*ask1) *ask1
                (cond((not (funcall *ask1))
                      (shout0 `(,reply |-- Bad value for| ,:slot /, |try again.|))
                      (go a))))

           (cond((confirm reply)
                 ;; Recall/: CONFIRM obeys the *confirmation-required* flag.
                 (cond((not replace/?)
                       (FPUT-VALUE :frame :slot reply 'source/: (user-frame)))
                      (t (FREPLACE-VALUE :frame :slot reply 'source/: (user-frame)))))

                ;; Not confirmed/: so ask all over again.
                (t (shout0 `(,:slot /?))
                   (go a)))

	   ;; This is to handle the case of non-standard request function and
	   ;; multiple requests; (arg 2) = nil.
	   (if (and (minusp count)
		    (boundp '*ask-request-fcn)
		    *ask-request-fcn)			; Assumed to return >1 values.
	       (go a))
	   (go b)))

(defun ASK* args
       ;; (ASK* query) == (ask query nil), ie keep prompting for values.
       (funcall 'ask (arg 1) nil))

;;; LASK and LASK* differ from ASK and ASK* in using the line-editing reader
;;; as a request function.

(defun LASK args
       (for (*ask-request-fcn 'request4)
            (apply 'ask (listify args))))

(defun LASK* args
       (for (*ask-request-fcn 'request4)
            (lask (cond((plusp args)(arg 1))) nil)))

(defun CONFIRM (input)
       ;; If *confirmation-required* is non-nil, a reply is requested.
       ;;   <input> is returned if the reply is affirmative (see function above).
       ;; If *confirmation-required* is NIL, the input falls through immediately.
       (cond(*confirmation-required*
              (cond((for (*request-prompter* '|Confirm/: |)
                      (affirmative/? (request input)))
                    input)))
            (input)))

;;;*****************************************************************
;;;		   SUGGESTIONS (Using the $SUGGEST facet)
;;;*****************************************************************

(defun ASK-SUGGEST-VALUES (:frame :slot)
       ;; Suggestions are often keyed to restrictions; but for now are
       ;; assumed to reside on the $SUGGEST facet along  with
       ;; appropriate comments about their connection with the
       ;; requirements themselves.  Return a list of suggested values.
       ;; Note that the suggestion methods themselves may "discuss"
       ;; the values they return.  The convention is that the
       ;; *DISCUSS* flag enables this extra talk.
       (mapappend (function (lambda (x) (feval x :frame :slot '$suggest)))
		  (for (*discuss* nil)	; No discussion of $Suggest facet itself.
		    (fextract-data (select-relevant-data :frame :slot '$suggest)))))

;; One suggestion is to SUGGEST-USE-DEFAULTS!


(defun ASK-DISCUSS-VALUES (:frame :slot)
       (or (discuss-key :frame :slot '$value)
	   (select-relevant-data :frame :slot '$default)))

;;  Plan/:
;; CHOOSE SUGGEST METHODS
;; RUN SUGGEST METHODS AND COLLECT SUGGESTIONS.

(defun ASK-USE-SUGGESTIONS (suggestions)
       ;; returns list of Suggestions if acceptable in number; else NIL.
       ;; Currently relies on the $SET facet to gather information on how many
       ;; values there should be.
       (prog ($set max min count)
	 (declare (fixnum max min count))
	 (if (null suggestions) (return nil))		; None to choose.
	 (setq $set (fdatum :frame :slot '$set))	; *** Use relevant information /?/?/?
	 (if (null $set)				; If no $SET information, select one.
	     (if *discuss* (shout0 `(|Choosing| ,(1st suggestions) /.)))
	     (return (list (1st suggestions))))		; Choose the first.
	 (setq max (or (fextract-message $set 'maximum/:) 100.)
	       min (or (fextract-message $set 'minimum/:)   0.)
	       count (length suggestions))		; Count >= 1
	 (cond ((< count min)
		(if *discuss* (shout0 `(|At least| ,(number-phrase min (lowercase :slot))
				        ,(verb-plural-form '|is| (> min 1))
					|required and only| ,count
					,(verb-plural-form '|was| (> count 1)) |found| /.)))
		(return nil))				; Not enough suggestions.
	       ((> count max)
		(setq suggestions (firstn max suggestions))
		(if *discuss* (shout0 `(|Only| ,(number-phrase max (lowercase :slot))
				        ,(verb-plural-form '|is| (> max 1)) |needed|
					/; |choosing| ,@(conjunctivize1 suggestions) /.)))
		(return suggestions))			; Too many suggestions.
	       (t
		(if *discuss* (shout0 `(|Choosing| ,@(conjunctivize1 suggestions) /.)))
		(return suggestions)))))

;;; Selection criteria for relevant sources of information about a slot.

;; Future plans -- In collecting defaults, preferences authored by different (non-resident)
;; frames,  effectively do a FDATA on each author.  Therefore if I express a
;; Preference for task, it will be overridden by a preference for Heater-maintenance.
;; Eg,  ((subtask place) $default (storeroom (finherit/: continue)))
;; means that MY default for Subtask is appended to MY default for Task.

(or (boundp '*DISCUSS*)		; Used often to enable discussion of values being returned.
    (setq *DISCUSS* T))


(defun SELECT-RELEVANT-DATA (:frame :slot :facet)
 ;; Obeys the *discuss* flag.  Returns list of "relevant" data (structures).
 (for (relevant-data
       (filter-data-by-source
	(caseq :facet ($value
		         (fdata :frame :slot :facet))
	              (($default $prefer $require $suggest)
		         (fdata-heritage :frame :slot :facet))
		      (T
		         (fdata :frame :slot :facet)))
	(relevant-sources :frame :slot :facet)))
   (if *discuss* (discuss-key1 relevant-data))		; /:Facet must be bound.
   relevant-data))

(defun FILTER-DATA-BY-SOURCE (data-strs sources)
       ;; returns Data-strs that have an "IN/:" comment among Sources.
       (filter d-s data-strs (intersectq/? (fbucket (faccess d-s 'IN/:)) sources)))

(defun RELEVANT-SOURCES (:frame :slot :facet)
       ;; returns a list of frames which can occur in the "IN/:" comment
       ;; of data deemed "relevant".  Note that a source is specific
       ;; to an entire slot.  If not information is given, only the frame
       ;; and its AKO hierarchy are returned.
       (setifyq (mapappend (function (lambda (f) (if (frame/? f) (ffamily f 'ako))))
			   (or (mapcan 'feval		;  /:Frame and /:Slot are bound.
				       (fdata-only :frame :slot '$source))
			       (list :frame)))))

(defun PRESENT-AND-FILTER-OPTIONS (:frame :slot)
       (declare (special options))				; in ASK
       (discuss-key :frame :slot '$value)
       (setq options (setminusq options
				(filter-options-with-requirements
				 options (for (*discuss* nil)	; Say nothing.
					      (select-relevant-data :frame :slot '$require)))))
       (filter-options-with-preferences options
					(select-relevant-data :frame :slot '$prefer)))

(defun FILTER-OPTIONS-WITH-REQUIREMENTS (options requirements)
       ;; Returns a list of options that FAIL at least one of the requirements.
       ;; Requirements are data structures; ie with comments.
       (setify
	 (mapappend
	  '(lambda (r)
            (discuss-requirements-for-options
	     (select '(lambda (o) (false/? (fclassify-value r o))) options) (list r)))
	  (fextract-data requirements))))

(defun FILTER-OPTIONS-WITH-PREFERENCES (options preferences)
       ;; Returns a list of options that MEET at least one of the preferences.
       ;; Preferences are data structures; ie with comments.
       (setify
	 (mapappend
	  '(lambda (p)
            (discuss-preferences-for-options
	     (select '(lambda (o) (true/? (fclassify-value p o))) options) (list p)))
	  (fextract-data preferences))))


(defun DISCUSS-PREFERENCES-FOR-OPTIONS (options preferences)
       ;; Returns the options.  Assumes preferences are bare constraint data.
       ;; Obeys the *DISCUSS* flag.
       (prog (ops/? prefs/?)
         (cond ((and preferences options *discuss*)
		(setq ops/? (cdr options) prefs/? (cdr preferences))
		(shout0 `(|The| ,(noun-plural-form '|preference| prefs/?) |that| | |))
		(describe-constraints preferences)
		(shout1 `(| | ,(verb-plural-form '|suggest| prefs/?)
			    ,@(conjunctivize1 options) |would be|
			    ,(cond ((null ops/?) '|a good|)(t '|good|))
			    ,(noun-plural-form '|choice| ops/?) /.))))
	 (return options)))

(defun DISCUSS-REQUIREMENTS-FOR-OPTIONS (options requirements)
       ;; Returns the options.  Assumes requirements are bare constraint data.
       ;; Obeys the *DISCUSS* flag.
       (prog ()
	(cond ((and options requirements *discuss*)
	       (shout0 `(|The| ,(noun-plural-form '|requirement| (cdr requirements))
			 |that| | |))
	       (describe-constraints requirements)
	       (shout1 `(| | |would rule out| ,@(conjunctivize1 options) /.))))
	(return options)))

(defun SELECT-SUPER-PROPERTIES (:frame :slot :facet)
       ;; Returns data-structures appearring in SUPER frames (and beyond).
       ;; Discusses them as it finds them, if *DISCUSS* is T.
       (for (options
               (mapappend '(lambda (:frame) (discuss-key1 (fdata :frame :slot :facet)))
			  (fdescendants :frame 'super)))
             (cond ((and options *discuss*)
	            (shout0 `(|These| ,(noun-plural-form (expand-key-name :facet) T)
			      |were found in the Super-Plan of| ,(fname :frame) /.))
		    (for (unused-options
			  (setminus (setifyq (fextract-data options))
				    ;; list of corresponding values in fellow subs.
				    (mapappend '(lambda (sibling)
							(fdata-only sibling :slot :facet))
					       (fsiblings :frame 'super 'sub))))
		      (cond (unused-options
			     (shout1 `(|The following do not appear as|
				       ,(noun-plural-form :slot T) |in the Sub-Plan| /:
				       ,(conjunctivize1 unused-options) /.)))))))
	     options))

;;;*****************************************************************
;;;       DISCUSSION of FACETS (alias KEYS) SLOTS and FRAMES
;;;*****************************************************************


(defun DISCUSS-KEY (:frame :slot :facet)
       ;;NB includes heritage of information for all facets except $value
      (discuss-key1 (caseq :facet
		       ($value    (FDATA :frame :slot :facet))
		       (otherwise (FDATA-HERITAGE :frame :slot :facet)))))

;;;          Specialized discussion functions for keys.

(defun DISCUSS-KEY-LOCAL (:frame :slot :facet)
       ;; No inherited properties are included.
       (discuss-key1 (*FDATA :frame :slot :facet)))


(defun DISCUSS-KEY1 (data-structures)
  ;; Assumes /:FRAME, /:SLOT, and /:FACET are bound.
  (prog (data-strs source/.data describe)
    (setq data-strs (filter ds data-structures (fextract-datum ds)))
    (if (null data-strs) (Return nil))			; No non-nil data, exit.
    (setq source/.data (classify (function (lambda (ds) (1st (fbucket (fcomment/? ds 'IN/:)))))
				data-strs))
    (setq describe (caseq :facet
			  (($value $default) 'describe-value-structures)
			  (($require $prefer) 'describe-constraint-structures)
			  (otherwise 'describe-structures)))
    (mapc (function (lambda (s/.d)
		     (for (source (car s/.d)
			   data (cdr s/.d)
			   frame (fname :frame)
			   facet (expand-key-name :facet))
		      (shout0 `(,@(cond((or (null source) (eq source frame)) '(|The|))
				       (t `(,source |has|
					    ,@(and (null (cdr data))	; Only one datum.
						   (list (indefinite-article facet))))))
				,(noun-plural-form facet (cdr data))
				|for| ,frame || |'s| ,:slot /: | |))
		      (funcall describe data)
		      (shout1 '/.))))
	  source/.data)
    (return data-structures)))


;; See use of the following in FDISCUSS-KEY1.
(defun DESCRIBE-VALUE-STRUCTURES (pstrs)
       (describe-values :frame :slot (fextract-data pstrs)))

(defun DESCRIBE-STRUCTURES (data-strs)
       (mapc '/#print data-strs))

(defun expand-key-name (key)
       (declare (special *key-name-directory*))
       (or (cdr (assq key *key-name-directory*))	;return key if no name found
	   key))

(setq *key-name-directory*
      '(($value . |value|)
        ($default . |default|)
        ($prefer . |preference|)
        ($require . |requirement|)
        ($if-needed . |If-Needed method|)
        ($if-added . |If-Added method|)
        ($if-removed . |If-Removed method|)
        ($suggest . |suggestion|)
        ($discuss . |Discuss method|)
	($say . |familiar name|)))


;;;*****************************************************************
;;;		PRETTY-PRINTING VALUES (The $SAY facet)
;;;*****************************************************************

(defun FSAY narg
       ;; (FSAY frame) returns a string to be shouted as the name for frame.
       ;; (FSAY frame slot) returns a string describing the values
       ;;                   of slot (using FVALUES).
       ;; (FSAY frame slot objects) returns a string describing
       ;;			    the objects as values of Frame and Slot.
       ;; Uses the value of the $SAY facet to generate the output.
       (prog (frame slot objects)
	  (setq frame (arg 1))
	  (cond ((= narg 1)
		 (setq slot 'self objects (list (fname (arg 1)))))
		((= narg 2)
		 (setq slot (arg 2) objects (fvalues-only (arg 1)(arg 2))))
		((= narg 3)
		 (setq slot (arg 2) objects (arg 3))))
	  (return (say-values frame slot objects))))

(defun FSAY1 narg
       ;; Like FSAY, except returns the string inside a newly consed
       ;; list to make NCONC ing possible and relatively safe.  See eg
       ;; SAY-TIME1 for another use of this convention.
       (for (say (apply 'fsay (listify narg)))
	 (and say (list say))))

;;; *** Change over to $SAY procedures returning a list of tokens suitable
;;;     for SHOUTing instead of a string.  Need therefore a conjunctivizer
;;;     for lists of lists of tokens.

(defun FSAY-PROCEDURE narg
       ;;   (FSAY-PROCEDURE frame {slot})
       ;; returns a procedure which (when applied to an arg)
       ;; is expected to return a pretty-printing string as a name.
       ;; in
       (for (:frame (arg 1)
	     :slot  (cond((> narg 1)(arg 2))(t 'self)))
	  ; If more than one way to say, uses the 1st, ie the most specific.
	  (1st (fdata-only :frame :slot '$say))))

;;; >>>>> Problems yet to be worked out/: many methods for saying the
;;;       value or frame name.  We must choose one, or always return
;;;       them all!


;; SAY-VALUE(s) returns NIL if no way to say the values.

(defun SAY-VALUES (:frame :slot :values)
       (for (how-to-say (fsay-procedure :frame :slot))
	(if how-to-say
	    (conjunctivize (foreach :value :values (funcall how-to-say :value))))))

(defun SAY-VALUE (frame slot value)
       (say-values frame slot (list value)))


;; This differs from SAY-VALUES in actully printing the resulting descriptive string
;; and it always prints something, if only the original values.

(defun DESCRIBE-VALUES (f s values)
       ;; Prints values by looking up special $SAY facet corresponding to F and S.
       ;; It is assumes that "$SAY" procedures return a string.
       ;; The default way to say a value is the value itself.
     (for (how-to-say (fsay-procedure f s))
       (shout1 (newconjunctivizer
		(foreach value values
                  `(,value ,@(if (and how-to-say (frame/? value))
				 (for (string (funcall how-to-say value))
				  (and (not (null string))
				       (not (samepnamep string value))
				       `(/( /" || ,string || /" /)))))))
		'|and| -1))))

(defun ask-says-options (option)
       (foreach o option
	(cond ((frame/? o)
	       (for (say-option (fsay o))
		(cond ((or (null say-option)
			   (samepnamep say-option o))
		       '||)
		      (t `(/" || ,say-option || /")))))
	      (t '||))))

(defun Fsay-something (frame)
       ;; Always returns a string to pring as the name of frame.
       ;; The default is the name of Frame.
       (or (fsay frame) (fname frame)))


(defun DESCEND-TREE (f s filter depth)
       ;; Iteratively presents root (initially F) plus any children along the S link
       ;; which satisfy the filter, up to depth.
       ;; The filter is a predicate which will be applied to select allowable children.
       (do ((root f)
            (choices (ncons f) (cons root choices))
            (offspring))
           ((null (setq offspring
                        (if (frame/? root)	; It might not be a frame.
			    (nselect filter (cdr (collect-family root s depth))))))
            root)
           (setq root (request2 `(,(capitalize (noun-plural-form s t)) |of| ,root |are/:|)
				 offspring
				(foreach o offspring
				   (if (select1 filter (fchildren o s)) '|...|))))
           (cond((null root)
                 (return nil))
		((eq root t)		; the root is ok
		 (return (1st choices)))
                ((memq root choices)	; if ever name a root (current or past), that's it.
                 (return root)))))



(defun GENERATE-POSSESSIVE-GENITIVE (owner object)
       ;; Eg, "Ira's office".
       ;; Substitutes "your" if owner is user.
       ;; Returns just object of owner is nil.
       (cond ((null owner) object)
	     ((eq owner (user-frame)) (stringify '|your | object))
	     (t (stringify (or (get-familiar-name owner) owner)
			   '|'s |
			   object))))


(defun SUBSTITUTE-YOU (x)
       (for (user (user-frame))
	(cond ((memq user x) (cons '|you| (delq user x)))
	      (t x))))


