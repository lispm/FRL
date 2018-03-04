(include declar)
(declare (special ^Q))
(setq ^Q nil)
;;;-*-Lisp-*-


;;;                  FANCY TTY I/O FOR NUDGE -- READING


;;; The request functions have been changed in foil,demo and sutil to
;;; be usable until progv is fixed.


;;; The following request5 function does what request2 should do once
;;; progv is fixed in franz. See also the temporary stuff in init
;;; related to getting maclisp i/o to work. Note that unlike the function
;;; it replaces, request5 returns the letter and not the choice it
;;; designates. Consequently my functions have been changed over to
;;; match for this letter.
;;; This function takes as args a set. The first element is shouted
;;; as the request. The other elements are paired with the letters
;;; of the alphabet to offer choices. The letter typed as a reply is
;;; returned. Thus, (request5 [are you] 'steve 'jim) would return a
;;; for me.


(defun request5 args
  (prog nil
	(terpri)
	(shout (arg 1))
	(space 2)
	(setq option *options*)
	(setq request 2)
	loop 
	(shout [@(car option) /:  @(arg request)])
	(setq option (cdr option))
	(setq request (add1 request))
	(cond ((not (> request (arg nil)))
	       (terpri)
	       (go loop)))
	(space 2)
	(setq tag (read (print '>)))
	(return tag)))

;;; The following request6 function duplicates the request function and is
;;; to be used until rpogv is fixed in franz.

(defun request6 (x) (shout x) (space 2) (read))

;;; the following two definitions are here since both /#prin1 and prin1
;;; don't exist in Franz.

#+franz
(defun prin1 (x) (print x))
#+franz
(defun /#prin1 (x) (print x))

(declare (special request-read		; alternate read function for REQUEST
		  *options*))		; names for options within REQUEST
(defmvar request-read nil)

;; (REQUESTS {query} options} comments})
;;
;
;	n and arglist are used to fix the following function since
;	liszt will not allow fexpr to use the (arg) function inside
;	a lexpr. (the compiled code does not remember it was a lexpr and
;	thus the (arg ) function will not work.
;
(declare (special number arglist))

(defun REQUESTS args
  ((lambda (number arglist)
	   (progv 
	    *options* 
	    (cond ((> number 1) 
		   (cond((null (cadr arglist)) NIL)
			((atom (cadr arglist))(list (cadr arglist)))
			(t (cadr arglist)))))
	    (prog (readtable input result query /#defaults options-used comments)
		  (setq readtable /#readtable)
		  ;	 (declare (fixnum /#defaults))   ; wont't win in OQC
		  ;(if ^Q (return nil))	; can't request when reading from a file!
		  (setq query (cond((> number 0)
				    (car arglist))))
		  (setq /#defaults (cond ((> number 1) 
					 (cond((null (cadr arglist)) 0)       ; NIL is an atom.
					      ((atom (cadr arglist)) 1)
					      (t (number (cadr arglist)))))
					(t 0)))
		  (setq options-used (firstn /#defaults *options*))
		  (setq comments (cond ((> n 2) 
					(cond
					 ((null (caddr arglst)) nil)
					 ((atom (caddr arglist))
					  (list (caddr arglist)))
					 (t (caddr arglist))))))
		  Again
		  (catch
		   (progn
		    (if query (shout0 query))
		    ;;  Print out options along with their labels and comments.
		    (do ((count /#defaults (1- count))
			 (options *options* (cdr options))
			 (comments comments (cdr comments))
			 (pagepause nil))				
			;disable --MORE-- processing.
			((cond((zerop count))
			      ((null options)
			       (warning '|Too many options supplied -- REQUESTS|)
			       T)))
			;	     (declare (fixnum count))
			(terpri)
			(princ '/()
			       (princ (car options))
			       (princ '|) |)
			(princ (symeval (car options)))
			;;print the comment
			(if (car comments) (princ '/	)
			    (shout-indented (car comments))))
		    Read  
		    (catch
		     (progn
		      (setq input (catch (readl) request))
		      
		      (cond ((eq input '/^U) (clear-input t) (go again))  ; ^U throws this
			    ((cdr input))			              ; a multiword reply
			    ((yes-word? (car input)) (return T))	      ; a one word reply
			    ((no-word? (car input)) (return NIL))
			    ((eq (car input) 'ALL) (return (mapcar (function
								     (lambda (term)
								       (symeval term)))
								   options-used))))
		      (return
		       (mapcar
			(function
			  (lambda (term)
				 (cond ((memq term options-used)
					;; typed a label, return its value
					(symeval term))
				       ((atom term)
					term)
				       ((eq (car term) 'ALTMODE)
					;; FOO => (altmode foo) by a readmacro in /#READTABLE
					(eval term)
					(throw nil again))
				       ((eq (car term) 'PERCENTSIGN)
					;; %FOO => (percentsign foo) because of a readmacro.
					;; Evaluate and return the result, assuming it doesn't cause an error.
					(cond((setq result (errset (eval (2nd term)))) (1st result))
					     (t (throw nil again))))
				       (t
					term))))
			input)))
		     read)
		    ;	(go read)
		    )
		   again)
		  (go again))))
   args
   (listify args)))

;;   (REQUEST1 query list-of-options list-of-comments)
;; arg1 (optional) = query
;; arg2 (optional) = a single (atomic) label or a list of labels.
;; arg3 (optional) = a list of Comments, where a comment can be an atom or
;;                   a list whose elements are taken to be the text of the comment.
;; Prints the query; then the options, labelling them A,B,C etc.
;; followed be the corresponding comments from arg3.
;; Each option is locally bound to a label and can be referred to by name
;; within the response.
;; If a response which matches a label, its value is returned.
;; If the first character of the response is "@", the value of the following expression
;;  is returned.  To return A, for example, if A is used, do @'A.
;; Altmode commands are available. (Do $? to see them.)
;; used as names for options.
(defun REQUEST1 args
  ((lambda (number arglist)
	   (progv *options*
		  (cond((> number 1)
			(cond((null (cadr arglist)) NIL)
			     ((atom (cadr arglist))
			      (list (cadr arglist)))
			     ((cadr arglist)))))
		  (prog (input testinput result query /#defaults comments)  
			;(and ^Q (return nil))	; can't request when reading from a file!
			(setq query (cond ((> number 0)
					   (car arglist))))
			(setq /#defaults (cond ((> number 1)
					       (cond ((null (cadr arglist)) 0)	       ; NIL is an atom.
						     ((atom (cadr arglist)) 1)
						     ((NUMBER (cadr arglist)))))
					      ( 0 )))
			(setq comments (cond ((> number 2)
					      (cond ((null (caddr arglist)) nil)
						    ((atom (caddr arglist))
						     (list (caddr arglist)))
						    ((caddr arglist))))))
			
			Again (if query (shout0 query))
			;;  Print out options along with their labels and comments.
			(do ((count /#defaults (1- count))
			     (options *options* (cdr options))
			     (comments comments (cdr comments))
			     (pagepause nil))	;disable --MORE-- processing.
			    ((cond((zerop count))
				  ((null options)
				   (warning '|Too many options supplied -- REQUEST|)
				   T)))
			    ;           (declare (fixnum count))
			    (terpri)
			    (princ '/()
				   (princ (car options))
				   (princ '|) |)
			    (princ (symeval (car options)))
			    ;;print the comment
			    (if (car comments)
				(princ '/	)
				(shout-indented (car comments))))
			
			Read  
			(setq input (catch (cond(request-read (apply request-read nil))(t (read))) request))
			(return
			 (cond((atom input)
			       ;; The input is an atom.
			       (setq testinput (uppercasify input))
			       (cond((eq testinput '/^U)	;control-U interrupt throws this.
				     (clear-input t)
				     (go again))
				    ((memq testinput (firstn /#defaults *options*))
				     ;; typed label, return value.
				     (symeval testinput))
				    ((yes-word? testinput) T)	; "yes" word transmuted to T.
				    ((no-word? testinput) NIL)	; "no" word transmuted to NIL.
				    (T input)))
			      ;; The input is a list.
			      ((eq (car input) 'PERCENTSIGN)
			       ;; %foo => (percentsign foo) because of a readmacro.
			       ;; Evaluate and return the result.
			       (cond((setq result (errset (eval (2nd input)))) (1st result))
				    (t (go read))))
			      ((eq (car input) 'ALTMODE)
			       ;; $foo => (altmode F) because of a readmacro.
			       ;; The function Altmode evaluates the *altmode property of its arg.
			       (eval input)
			       (go read))
			      (input))))))
   args
   (listify args)))

;;; CLEAN UP THIS MESS!!!

(defun REQUEST args
       ;; like request2 except that its 2nd argument, if present and non-nil,
       ;; is considered to be the one and only default.  It will
       ;; be labelled "a" and be returned if one responds
       ;; with "a" to the query.
       ;; (Historically, a predecessor of REQUEST2; thus the name.)
       (request2
             (cond((> args 0)(arg 1)))
             (cond((and (> args 1) (arg 2))
                   (list (arg 2))))))

(defun REQUESTS1 args
       (prog (reply)
	(setq reply (apply 'Requests (listify args))) 
  again (cond ((atom reply) (return reply))
	      ((null (cdr reply)) (return (car reply)))
	      (t (shout0 '|One reply only; pick one.|)
		 (setq reply (requests nil reply))
		 (go again)))))

(defun REQUEST2 args
       (for (request-read (function (lambda () ;(cursorpos 'A)
				       (terpri)
				       (princ (/#answer *request-prompter*))
				       (read1)))
             readtable (makereadtable nil))
          (apply 'request1 (listify args))))

(defun REQUEST4 args
    ;; Request a string, terminated by CR.  Detects $ and % as readmacros if they are
    ;; the first character in the line.
    (for (prompt (/#answer '/_))
       (for (request-read
	          (function (lambda ()
			     (for (readtable /#readtable)
                               (for (input (nreverse (readl1 prompt nil)))
                                  (and input
                                       (or (if (= (car input) 27.)	; ALTMODE
                                               (cons 'altmode (list (readlist (cdr input)))))
					   (if (= (car input) 37.)	; PERCENTSIGN
					       (cons 'percentsign
						     (list (readlist (cdr input)))))
                                           (implode input)))))))
             *request-prompter* prompt)
          (apply 'request1 (listify args)))))

(defun REQUEST-PIDGIN args
    ;; Request a string, terminated by "}"; ie CRs are ignored.
    ;; Detects  if it is the first character in the line.
    ;; Otherwise, the line is interpreted as a PIDGIN phrase; it is
    ;; converted to LISP and a % added as a prefix before sending it on to
    ;; request.  It is therefore exactly like returning an evaluated LISP expression.
    (for (prompt (/#answer '|{ |))
       (for (request-read
	     (function (lambda ()
              (for (input (nreverse (readl2 prompt)))
	       (and input
		    (or (if (= (car input) 27.)	; ALTMODE
			    (cons 'altmode (list (readlist (cdr input)))))
			;; otherwise, interpret as PIDGIN phrase.
			(for (parse (feed-string-to-pidgin (bracket-string input)))
			  (cond ((atom parse) parse)	; Atoms are effectively quoted.
;				((yes-word? parse) 'yes)    ; Unfortunate, must catch this
;				((no-word? parse)  'no)	    ; now, since must send Percentsign.
				(t (cons 'percentsign (list parse))))))))))
             *request-prompter* prompt)
          (apply 'request1 (listify args)))))

(defun readl2 (prompt)
       ;; Reads several lines, returning as one reversed string, sans CR.  Terminate with }.
       (do ((string nil (nconc piece string))
	    (piece (readl1 prompt 'nil)
		   (readl1 '|(...) | 'nil) ) )
	   ((and piece (= 125. (car piece)))		       ; }
	    (nconc (cdr piece) string))))

(defun feed-string-to-pidgin (s)
       ;; S is an exploded string beginning with { and ending with }.
       ;; The /:LISPSYN readtable has "{" defined as call to PIDGIN-READ.
       ;; Returns the Lisp expression produced by interpreting the string.
       (for (readtable /:LISPSYN)
        (for (parse (errset (readlist s)))
	 (cond (parse (car parse))		  ; No problems.
	       (t     (throw '/^u request))))))

(defun bracket-string (s)
       ;; S is modified by wrapping { and } around it.
       (cons '/{ (nconc s (list '/}))))

(declare (special /#answer))
(defun /#ANSWER (suffix)
       ;; If the /#ANSWER flag is T, affixes numerical prefix. else just returns suffix.
       (cond ((null /#answer) suffix)
	     (t (for (base 10. *nopoint t)
		 (stringify (putprop '/#answer (1+ (get '/#answer '*prefix*)) '*prefix*)
			    suffix)))))

(defun /#ANSWER-RESET ()
       (putprop '/#ANSWER 0 '*prefix*))

(/#answer-reset)
(defmvar /#answer nil)



(defun RE-REQUEST (f-unused ch-unused)
        ;; interrupt routine for ^U (initially) recognizable from within
       ;;  the REQUEST function.
       ;; It throws a ^U (<uparrow>U) to REQUEST, which is caught by the read loop there.
       (tyi tyi)		;swallow the ^U which caused the interrupt.
       (errset (throw '/^U request) nil)
       ;; fall through errset if no catch exists for this throw.
       (shout0 '|^U is only meaningful during prompt a ">", ">>" or "_".|)
       (terpri))



(defun OK? args
       (affirmative? (apply 'request2 (listify args))))

(defun AFFIRMATIVE? (x)
       ;; returns X if it is not one of a collection of 
       ;; negative-sounding replies; else NIL.
       (cond( (no-word? x) NIL )
            ( x )))


(defun YES-WORD? (x)
       (memq x '(t y yes)))

(defun NO-WORD? (x)
       (memq x '(nil n no)))

(defvar *options*
      ; Used for labeling options by request2
      '(a b c d e f g h i j k l m n o p q r s u v w x y z))
;; Note -- T has been been omitted from this list; and
;;         N and Y will perhaps have to go.
;;  T is used by Lisp, of course.
;;  N is allowed as an abbreviation for NO, which usually ends up getting changed to NIL
;;  Y goes to T the same way (via YES).
;;  Currently, labels are checked first!

(defvar *request-prompter* '>)



(defun READLINE args
       ;; (READLINE '|Now what?|) prints/:
       ;;    Now what?
       ;;    _
       ;;
       ;; and reads in a line from the tty (ie, up to the first CR) as a |...| atom.
       ;;   RUBOUT works.
       ;;   ^K erases the line and reprompts.
       ;;   ^N erases the previous word.
       ;;   ^Y erases the previous list.
       ;;   ^L redisplays the line.
       ;;   / quotes the next character.

       (if (plusp args) (shout0 (arg 1)))
       (maknam (nreverse (readl1 '/_ nil))))



(defun READL ()
       ;; Reads a line from the TTY, terminated by CR and
       ;; parses it as a list of S-expressions using the primitive function READLIST.
       ;; Tries again if unbalanced parentheses.
       (prog (return charlist)
     A  (setq charlist (readl1 (/#answer '|>> |) charlist))
        (if (setq return (errset (readlist (cons 40. (reverse (cons 41. charlist))))))
	    (return (car return)))
	(go A)))

(defun READ1 ()
       ;; A READ which flushes any trailing break character.
       ;; Useful to avoid leaving stray spaces, rubouts (and CRs if so declared)
       ;; in the input buffer to screw up such as the --MORE-- processor.
       ((lambda (read)
            (and (atom read)
                 (not (zerop (boole 1 32768.	; = 100000(octal)
                                    (eval `(status syntax ,(ascii (tyipeek)))))))
                 (tyi))
            read)
        (read)))


(defun READL1 (prompt string0)
       ;; STRING0 is assumed to be a destructible list of character fixnums
       ;; such as that returned by this function.
       (NEWLINE)
       (princ prompt)			; Print the prompt character
       (if string0 (setq string0 (nreverse (mapc (function
						   (lambda (c) (tyo c)))
						 (nreverse string0)))))
       (do ((ch (tyi) (tyi))
            (plength (flatc prompt))	; Need this information to do proper cursorpos'ing
            (string string0)) ; A list of character fixnums.
	   (nil)
	   (declare (fixnum plength ch))
	   (cond ((= ch 13.)	; CR returns the reversed string as a list of characters.
		  (return string))

		 ((= ch 47.)	; SLASH quotes the next character.
                  (setq string (cons (tyi) string)))

                 ((= ch 12.)	; ^L cleared the screen, so redisplay.
                  (princ prompt)	; ?? printing ?? newline first!
                  (mapc (function (lambda (ch) (tyo ch)))
			(reverse string)))

                 ((= ch 11.)	; ^K deletes the line and restarts prompt.
                  (cursorpos 'h plength)(cursorpos '/])
                  ;; and on printing terminals ??  newline and prompt
                  (setq string nil))

                 ((= ch 14.)	; ^N deletes the previous word
                  (cursorpos 'H (- (cdr (cursorpos)) 2))(cursorpos '/])	; remove the ^ and N
                  (setq string
                        (do ((string string (prog2 (cursorpos 'x) (cdr string))))
                            ((or (null string)
				 ; 32768. = 1000000octal = "blank"
                                 (zerop (logand 
					 (eval `(status syntax ,(1st string))) 32768.))
				 )
                             (do ((string string (prog2 (cursorpos 'x) (cdr string))))
                                 ((or (null string)
                                      (not (zerop (logand 
						   (eval `(status syntax ,(1st string)))
							  32768.)))
				      )
                                   string))))))
                            
		 ((= ch 25.)	; ^Y deletes the previous list
                  (cursorpos 'H (- (cdr (cursorpos)) 2))(cursorpos '/])	; remove the ^ and Y
		  (setq string
			(do ((string string (prog2 (cursorpos 'X) (cdr string))))
			    ((cond ((null string))
				   ; 16384. = 40000octal = left parenthesis
				   ((not (zerop (logand (eval `(status syntax ,(car string))) 16384.)))
				      (cursorpos 'X)(prog1 (car string)
							   (setq string (cdr string)))
				      t)
				   )
			     string))))

                 ((= ch 127.)	; RUBOUT
                  (cond (string (cursorpos 'x)
				;; ?? do what on printing terminal ?? echo.
				(setq string (cdr string)))))

                 (T (setq string (cons ch string))))))

;;; Improvements/:
;;;   A subroutine RUBOUT with arg = ascii that knows how many columns to flush
;;; could take a string and do it by computing movement required only.
;;;   To delete, compute final cursorpos and delete to end of line.
;;;   Fix up mess caused by editing in the dribblefile, if any.

;;;*************************************************************************
;;;			       PRINTING FUNCTIONS
;;;*************************************************************************


(declare (special shoutvector))

(declare (fixnum (shoutgap notype notype)
		 (shoutroom notype notype notype)
		 (shoutflatc notype)
		 (shoutchrct notype))
	 (notype (shoutblank fixnum)))

(defun SHOUT-INDENTED args
  ;; (SHOUT-INDENTED <list of tokens> {indentation})
  ;;   /#PRINCs each element its first argument , indenting to the
  ;; original column (or the optional indentation given as the second
  ;; argument) if a newline is required.  SHOUT uses SHOUTVECTOR to
  ;; determine how much blank space to leave around punctuation. 

 (declare (fixnum indentation))
 (for (tokens (cond ((atom (arg 1)) (list (arg 1))) (t (arg 1)))
       indentation (cond ((> args 1) (arg 2))	; Current indentation is default.
			 (t (shoutchrct t))))
  (do ((previous '*terminate*
		 this)
       (this (car tokens)
	     next)
       (next (cond ((cdr tokens) (cadr tokens))
		   ('t '*terminate))
	     (cond ((cdr rest) (car rest))
		   ('t '*terminate)))
       (rest (cddr tokens)
	     (cdr rest)) )
      ((eq this '*terminate*))				; Returns NIL

      (cond ((> (shoutroom previous this next) (shoutchrct t))
	     (terpri) (indent-to indentation))
	    (t (shoutblank (shoutgap previous this))))
      (/#princ this))
  (ascii 0)))

;; Perhaps (to prevent a newline when the first thing is too long for the line)
;; we should enter by PRINCing the first token.  We should only
;; do this if the new indentation point would provide more space or allow the line 
;; to be entered without a break.

(defvar SHOUTVECTOR
  '((*terminate* 0 0)
    (/ 0 0)			; no space before or after characters
    (/
 0 0)			;  that move the cursor.
    (/ 0 0)
    (/	 0 0)			;   TAB
    (/   0 0)			;   SPACE
    (||  0 0)
    (/. 0 2)			; no space before and
    (/? 0 2)			;  2 spaces after {. ? !}
    (/! 0 2)
    (/% 0 )			; no space before {% , ; /:}
    (/, 0 )
    (/: 0 )
    (/; 0 )
    (/( nil 0)			; no space after {( [ { `}
    (/[ nil 0)
    (/{ nil 0)
    (/` nil 0)
    (/) 0 )			; no space before {) ] } '}
    (/] 0 )
    (/} 0 )
    (/' 0 )))
;; + possible additional syntax for {/# $ _ ^ ~}
;; NOTE can't do anything smart about "..." or |...| because context dependent.

(defun shoutgap (previoustoken thistoken)
  (cond ((cadr (assq thistoken shoutvector)))
	((caddr (assq previoustoken shoutvector)))
	(1)))

(defun shoutroom (previoustoken thistoken nexttoken)
  (+ (shoutgap previoustoken thistoken)	; space before current token.
     (shoutflatc thistoken)		; the current token's length.
     (cond ((zerop (shoutgap thistoken nexttoken))
	    (shoutflatc nexttoken))	; the next token, if it abuts 
	   (0))))			;  the current token.

(defun shoutflatc (x)
       (cond ((eq x '*terminate*) 0)
	     (t (flatc x))))	; + Change to /#FLATC ?

(defun shoutblank (n)
       (declare (fixnum i n))
       (do i 1 (1+ i) (> i n) (tyo 32.)))

(defun shoutchrct (tty)
       (- (linel tty) (charpos tty)))


#-lispm
(eval-when (load compile)
  (sstatus tabsize 8))

(macro tabsize (call)
       ; Get tabsize of current machine.
       (status tabsize))

(defun INDENT-TO (nn)
       ;; CHRCT, the space remaining on the line, is set to NN.
       (declare (fixnum nn nct linel tab))
       (for (nct (chrct t) linel (linel t) tab 0.) 
	(cond ((> nn nct)			  ;Can't indent properly on this line.
	       (terpri)
	       (setq nct linel)))
	(cond ((< nn nct)			  ;Some indentation is necessary.
	       (setq tab			  ;Position as a result of first tab.
		     (+ nct (- (tabsize)) (\ (- linel nct) (tabsize))))
	       (cond ((< tab nn)		  ;Tabs do not move 8, but
		      (NTYO (- nct nn) 32.))	  ;to nearest multiple of 8.
		     ((tyo 9.)
		      (setq nct tab)
		      (cond ((< nn nct)
			     (NTYO (// (setq nct (- nct nn)) (tabsize)) 9.)
			     (NTYO (\ nct (tabsize)) 32.)))))))))

(defun NTYO (nn x)
       (declare (fixnum mm nn))
       (do mm nn (1- mm) (zerop mm) (tyo x)))

(defun SPRINT (x l unused)
       ; An old GRINDEF function -- used in SHOUT.
       (INDENT-TO L)
       (/#prin1 x))

(defun PRINTBLOCK (x)
  ($prpr x))


;;; More PRINTING functions.

(defun SHOUT0 (x)
       ;; This will supplant SHOUT, I hope.
       (newline)
       (/$prpr x))

(defun SHOUT1 (x)
  (/$prpr x))
;;       (shout-indented x (linel t)))

(defun SHOUT-CENTERED args
       ;; (SHOUT-CENTERED x {width})
       ;; prints the elements of X centered in width (if missing, use (LINEL T)).
       (declare (fixnum width linel))
       (prog (width linel)
	(setq linel (linel t)
	      width (cond((and (> args 1)(arg 2))(arg 2))(t linel)))
        (indent-to (min (// (+ linel linel (minus width) (flatc (arg 1))) 2) linel))
	(shout-indented (arg 1))))


(defun SHOUT args
  ;; (SHOUT x {enable})
  ;; Starts a newline; then SHOUT0's X.
  ;; But only if the enabling second input is absent or non-nil.
  ;; Returns the null character.
  (cond((zerop args))
       ((and (> args 1) (null (arg 2))))
       (t (shout0 (arg 1))))
  (terpri)
  nil)
;; COULD REPLACE THIS WITH SHOUT0 ONCE CHECK USES OF ENABLE CROCK!

;; Two oft-used flags to control shouting of messages.

(defvar *verbose* nil)
(defvar *debug* nil)
(defvar *help* nil)

(defun FDEBUG args
       ;; non-nil or absent arg => *debug* <- T
       ;; returns old value of *debug*
  (let ((old *debug*))
    (setq *debug* (cond ((> args 0) (and (arg 1) t))
			(t)))
    old))

(defun FVERBOSE args
       ;; non-nil or absent arg => *verbose* <- T
       ;; returns old value of *verbose*
  (let ((old *verbose*))
    (setq *verbose* (cond((plusp args) (and (arg 1) t))
			 ( t )))
    old))

(defun FHELP args
       ;; non-nil or absent arg => *help* <- T
       ;; returns old value of *help*
  (let ((old *help*)
	(setq *help* (cond((plusp args) (and (arg 1) t))
			  ( t ))))
    old))

;;; Printing Utilities

(defun PRINC-LIST (l)
       ;; If all members of L are atoms, conjunctivizes them;
       ;; else, /#PRINT's each of them.
       ;; If L is an atom, just princ's it.
       (if (atom l) (setq l (ncons l)))
       (cond((forall i l (atom i))
             (shout-indented (conjunctivize1 l)))	; In Frame;FSPEAK.
            (t
	     (do-foreach i l (/#print i))))
       t)            

(defun UPPERCASIFY (string)				; used in REQUEST
       ;; converts string S to uppercase and interns it.
       ;; *** NOTE *** This does not work under Franz. It has been changed
       ;; to convert the characters to lower case, which is correct for Franz.
       ;;
       ;; wjl 6 Aug 1981
      (intern
       (maknam
	(nmapcar (function
		  (lambda (ch)
		   (declare (fixnum ch))
		   (cond((and (< 64. ch)(< ch 91.)) (+ ch 32.))(t ch))))
		 (exploden string)))))

#+maclisp
(defun FBUG nil
   ;; allows one to type in a message, line by line, which becomes the text
   ;; for a "/:BUG FRL ..." message.
   ;;;>>>>> Improvements/: To whom (language, system)
   ;;                     name of function causing error -- from the stack
   ;;                     copy of error message -- (errprint nil file)
   ;;                     prints most recent error message on file.
   ;;                     *, +
   ;;                     baktrace
  (prog (file dribble text)
       (declare (special dribblefile))
       (setq text
            (do ((line (readline '|Enter message.  End it with a single . on a line./
(Delete/: RUBOUT character, ^N word, ^Y list, ^K line.)|)
                       (readline))
                 (msg))
                ((and (= (flatc line) 1)(= (getcharn line 1) 46.)) (nreverse msg))
                (setq msg (cons line msg))))
       (if (null text) (return nil))
       ;; Send the message.
       (setq dribble (and (boundp 'dribblefile)
                          (memq dribblefile outfiles)
                          ^R
                          ;; dribbling, so record the name
                          (namestring (truename dribblefile))))
       (setq file (open '((dsk pa) _fbug_ _msg_) 'out))
       (for (outfiles (list file) ^R t ^W t)
            (do-foreach line text (princ line) (terpri))
            (if (and (boundp '*version*) *version*)
                (shout0 `(/[ ,*version* ,(version? *version*) /])))
            (if dribble (shout-indented `(|   Dribblefile/:| ,dribble))))
       (close file)
       (shout0 '|Mailing...|)
       (valret (stringify '|:mail bug-frl /gdsk:pa/;_fbug_ _msg_| (ascii 3.)	; ^C 
			  '|/:job | (status jname)
			  '|/:vp|))
       (deletef file)
       (shout-indented '|  /;Sent|)))

;;
;; 	in unix, people real should just use mail, but anyway this simple
;;		command exists for franz.
;;
#+Franz
(defun fbug nil 
  (patom '|Sending Mail:|)
  (terpri)
  (exec mail |steve/@lbl-unix| |douglas/@lbl-unix|))


(comment TTY Control)

;;;*****************************************************************
;;;                      --MORE-- Processing
;;;*****************************************************************

;;; a --MORE-- handler; slight modification of function from
;;;   liblsp;split rich5

(declare (special *rset pagepause))
;;; This maclisp function is not implemented in Franz.
;;; (endpagefn t 'pagepause)

(defun PAGEPAUSE (tty)
       ;;; Pauses at bottom of page.
       ;;; TTY is output tty which is interrupted.
       ;;; Output proceeds only if a <sp> or <cr> typed;
       ;;; anything else aborts type-out.
       ;;;   <sp> rolls typeout
       ;;;   <cr> clears screen and continues at top
       ;;; NOTE/: (1) characters typed are NOT gobbled
       ;;;            (Exceptions/: SPACE CR RUBOUT)
       ;;;       (2) ctl-G or ctl-S interrupts can be used
       ;;; Only pauses if the value of the PAGEPAUSE variable is non-nil.
 (cond(pagepause	;If pagepause is NIL, do no MORE.
       (clear-input t)
       (princ '--MORE-- tty)
       (nointerrupt nil)
       ((lambda (char)
                (cond ((= 32. char)(tyi)(cursorpos 'top tty)(cursorpos 'l tty))
                      ((= 13. char)(tyi)(cursorpos 'c tty))
                      (T (IF (= 127. CHAR) (princ '|Flushed| tty))
                         (cursorpos 'top tty)
                         (cursorpos 'l tty)
                         (IF (= 127. char) (tyi) (prog (*rset) (error nil))))))
        (tyipeek)))))

;; Binding pagepause to NIL, allows one to easily temporarily inhibit MORE processing.
(or (boundp 'pagepause)(setq pagepause t))

;;; Put on Control-F in PA;INIT.

; save the last frame printed by PRINT-FRAME.
(defvar print-frame nil)

(defun PRINT-FRAME (fileobj char-unused)
       ;; To be attached to a control char to print out a frame.
       ;; Reads a frame name or a frame path.
       ;; Saves last arg in the atom PRINT-FRAME; if input is <space>, uses this.
       (nointerrupt nil)	; re-enable interrupts.
       (tyi fileobj)					;gobble control char
       (for
	(spec (cond ((= (boole 1 127. (tyipeek nil fileobj)) 32.)	;note masking for 7 bit
		     (tyi fileobj)			;gobble space
		     ;; If space typed then just use default.
		     ;; NOTE/: BUG in Lisp reader if enter single character function name.
		     print-frame)
		    ((for (spec1 (read fileobj))
		       (if (atom spec1) (TYI fileobj))		; gobble space
		       spec1))))
	(/#print (cond((atom spec) (frame? spec))
		     ((frame? (car spec)) (apply 'flistget spec))))
	(setq print-frame spec))
       (terpri))


