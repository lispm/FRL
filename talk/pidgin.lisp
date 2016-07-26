From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:10:19 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08173; Thu, 2 Jun 88 14:10:18 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04381; Thu, 2 Jun 88 13:32:11 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08130; Thu, 2 Jun 88 12:45:56+0900
Date: Thu, 2 Jun 88 12:45:56+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020345.AA08130@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: pidgin.l.frl
Status: RO


(include declar)
;; Condensed from MITCH; @GLANG 240 on April 7, 1977 by RBR

;;*** Requires PA;UTIL and PA;%CNTRL for compilation due to presence of these forms/:
;;    PROG1, WARN

;;; All special variables, throw tags and indicators used by the parser have prefix "/:".

(DECLARE (SPECIAL /:pidginSYN	; the readtable for {} reader
                  /:LISPSYN	; the standard lisp readtable
		  /:pidgin-character	; must make these into single-char-obj
                  /:EOF		; end-of-file marker returned by READ
                  /:TOKEN	; the "current" token
                  /:LEFT		; the "previous" parsed structure
                  /:DRBP		; declared in DEFFIX; used in ISF, RIGHT and RIGHTLIST
                  /:FUN		; declared in DEFFIX; used in ISF
                  /:ISFUN 	; declared in DEFFIX; used in ISF
                  /:GTEST	; used by GT to hold test phrase
                  ))
(declare (*fexpr den denfun buildfun pidgin-to-lisp gt))
(declare (*lexpr variants))
(declare (*lexpr concatenate))
(declare (special readtable))
(declare (muzzled nil))
(declare (macros t))

(setq /:EOF (ncons nil))


(declare (sstatus macro /% '(lambda () ((lambda (/%) (eval /%) /%) (read)))))

;; Expressions preceded by % are read and digested by the compiler, as well as being
;; processed and passed through.  Use % before functions that will be needed by macros
;; expanding at compile-time.

;;; I have taken out these % characters, and commented where this occurs.
;;; To compile in Franz, I may need to do something equivalent.


(setq /% nil)				; % is a NO-OP to the interpreter.




;;; PIDGIN uses a Top Down Operator Precedence Parser adapted from PRATT;CGOL.
;;; For other documentation, see AI Working Paper /#121, and
;;; Pratt, V.R. "Top-down Operator Precedence." SIGACT/SIGPLAN Symposium
;;;  on Principles of Programming Languages, Boston, 1973, 41-51.

;;; To use/: Do (TALK) initially to set up the parser's readtable.  Henceforth, anything
;;; inside { ... } will be interpreted by the parser instead of the normal LISP reader.

(DEFUN /:PARSE (RBP) 
       (declare (fixnum rbp))
       (ASSOCIATE RBP (COND ((AND (ATOM /:TOKEN) (GET /:TOKEN '/:NUD))
			     (FASTCALL (PROG1 (GET /:TOKEN '/:NUD) (READ-NEXT-TOKEN))))
                            ((PROG1 /:TOKEN (READ-NEXT-TOKEN))))))

(DEFUN ASSOCIATE (RBP /:LEFT) 
       (declare (fixnum rbp))
       (COND ((< RBP (OR (GET /:TOKEN '/:LBP) 0.))
	      (ASSOCIATE RBP (FASTCALL (OR (VERIFY (GET /:TOKEN '/:LED)) (LEDERR)))))
	     (/:LEFT)))

;; Note/:  READ-NEXT-TOKEN flushes "THE", "AN" and "A" from the input stream.

(DEFUN READ-NEXT-TOKEN NIL
       ;; Get next token.
       (COND ((EQ (SETQ /:TOKEN (READ /:eof)) /:eof)
              (eoferr))
             ((memq /:token '(the an a))
	      (READ-NEXT-TOKEN))
             (/:TOKEN)))

(DEFUN VERIFY (DEN)
       ;; If there is a denotation (from the current token), advance.
       (COND (DEN (READ-NEXT-TOKEN) DEN)))

(defun FASTCALL (den)
       (cond ((eq (car (plist den)) 'subr)
              ;; Optimal case/: SUBR is first on the plist.
              (subrcall nil (cadr (plist den))))
             ((for (temp (getl den '(expr subr)))
		   (cond ((eq (car temp) 'subr)
                          ;; SUBR property, but not at head of plist; so
                          ;; put it there before calling it.  Better luck next time.
                          (remprop den 'subr)
		          (putprop den (cadr temp) 'subr)
			  (subrcall nil (cadr temp)))
			 (t
			   (funcall den)))))))

;;; IMPROVEMENT/: Clobber SUBR property of /:LED-token to /:LED-SUBR property of <token>, etc. 

;; The LBP, LED and NUD of a token are stored on the atom's property list
;; under the indicators/: /:LBP, /:LED and /:NUD.  /:LBP is a fixnum.  /:LED and /:NUD
;; are names of denotations conventionally constructed as /:LED-<token> and /:NUD-<token>.
;; These in turn have a SUBR or EXPR property which in fact is the denotation for the token.
;; A feature of this indirection scheme is that denotations are traceable.

(declare (*lexpr /#print /#princ))
(DEFUN LEDERR NIL
       (print `(ERROR /- ,/:TOKEN |does not accept a left argument.|))
       (print '| Left  =|) (print /:left)
       (print '| Right =|)
       (DO ((I 25. (1- I)) (ch (if (plusp (listen t))(tyipeek nil t))
                               (if (plusp (listen t))(tyipeek nil t))))
           ((or (null ch) (zerop I) (= ch 125.)) (terpri))
           (declare (fixnum i))
           (TYO (tyi)))
       (clear-input t)					       ;flush typeahead.
       (throw 'lederr-bug /:bug))

(defun EOFERR nil
       (print '|EOF encountered while trying to read a token.|)
       (throw 'eof-bug /:bug))


(defun TALK nil 
  ;; Set up readtable for use during PIDGIN-READ.
  (setsyntax '/{ 'macro 'pidgin-read)
  (SETQ /:LISPSYN (MAKEREADTABLE nil))
  ;; ie, "current" readtable
  ;; Some individual characters have denotations, so make
  ;; them into single character objects.
  (MAPC '(LAMBDA (X) 			;;(SETSYNTAX X 'MACRO NIL)
		 (setsyntax x 66.))
	/:pidgin-character)
  (SETSYNTAX '/. 66.)	; . is decimal point, but not dotted pair dot.
  (SETSYNTAX '/} 66.)		

  (setsyntax '/? 66.)
  (SETSYNTAX '/# 2.)	
  (SETSYNTAX 13. 204.)		; CR is like space.
  (SETSYNTAX '/% 'splicing 'pidgin-/%-MACRO)	; comments are % ... %
  (setsyntax '/! 'macro 'pidgin-/!-macro)	; reads a single S-exp using
  (SETQ /:pidginSYN (makereadtable nil))
  ; LISP syntax
  (setq readtable /:lispsyn)
  ;;(ascii 0)
  nil)

(defmvar /:pidgin-character '(/{ /}))

(defun define-pidgin-character (ch)
       (or (memq ch /:pidgin-character )
	   (push ch /:pidgin-character )))

(defun PIDGIN-/%-MACRO NIL 
       ;; Splice out comments; anything inside % ... %.
       (DO ((CH (ASCII (TYIPEEK nil T)) (ASCII (TYIPEEK nil T))))
	   ((EQ CH '/%) (TYI T) nil)
	   (READC T))) 

(defun PIDGIN-/!-MACRO NIL
       ;; Read one S-expression using normal LISP syntax.
       ((LAMBDA (READTABLE) (READ T)) /:LISPSYN))

(declare (special iterator *debug-pidgin*))
(defmvar *debug-pidgin* T)

(defun PIDGIN-READ nil
       ;; The new READ function. 
       (prog (readtable bug iterator sentence) 
	     (setq readtable /:pidginsyn)
	     (if (setq bug (catch (progn (READ-NEXT-TOKEN) (setq sentence (/:PARSE -1.))
				(return (cond (iterator `(mapcar '(lambda (,(car
					iterator)) ,sentence) ,(cdr iterator)))
					(sentence))) /:bug)))
		 (cond (*debug-pidgin*
			(setq readtable /:lispsyn)
			(cond ((eq bug 'eof-bug)
			       (break eof-bug))
			      ((eq bug 'lederr-bug)
			       (break lederr-bug))))
		       ((error))))))

;;; **** BUG to be fixed/: If end phrase (}) before complete parse, pidgin-read will
;;;      just hang waiting for another token.

(DEFPROP /} -1. /:LBP)			; The end of a PIDGIN expression.

;; Utilities for defining translation functions.

(macro DEFFIX (L)
       ;; (DEFFIX dentype /:isfun /:fun lbp rbp code)
       ;;    where DENTYPE is { /:LED /:NUD }
       ;;          /:ISFUN  is { ISN ISS ISP ISI ISM }
       ;;          /:FUN    is token whose syntax is being defined
       ;;          LBP and RBP are binding powers
       ;;          CODE (this argument is evaluated) is the denotation
       (PROG (DENTYPE /:ISFUN /:FUN DENNAME /:DRBP) 
	     (SETQ L (CDR L))
	     (SETQ DENNAME (concatenate (SETQ DENTYPE (CAR L))
					'/-
					(SETQ /:FUN (CADDR L))))
	     (SETQ /:ISFUN (CADR L) L (CDDDR L))
	     (SETQ /:DRBP (CADR L))
	     (RETURN (LIST 'PROGN
			   '(quote COMPILE)
			   (LIST 'DEFPROP /:FUN DENNAME DENTYPE)
			   (LIST 'DEFUN DENNAME NIL (EVAL (CADDR L)))
			   (IF (CAR L)
			       (LIST 'DEFPROP /:FUN (CAR L) '/:LBP)))))) 

(defmacro NILFIX (FUNC CODE)          `(DEFFIX /:NUD ISN ,FUNC NIL NIL ,CODE)) 
                 ; no arguments

(defmacro PREFIX (FUNC RBP CODE)      `(DEFFIX /:NUD ISP ,FUNC NIL ,RBP ,CODE)) 
                 ; one argument follows

(defmacro SUFFIX (FUNC /:LBP CODE)     `(DEFFIX /:LED ISS ,FUNC ,/:LBP NIL ,CODE)) 
                 ; one argument precedes

(defmacro INFIX  (FUNC BP CODE)       `(DEFFIX /:LED ISI ,FUNC ,BP ,BP ,CODE)) 
                 ; left and right binding powers equal

(defmacro INFIXR (FUNC BP CODE)       `(DEFFIX /:LED ISI ,FUNC ,BP ,(1- BP) ,CODE)) 
                 ; right associative

(defmacro INFIXD (FUNC /:LBP RBP CODE) `(DEFFIX /:LED ISI ,FUNC ,/:LBP ,RBP ,CODE)) 
                 ; different left and right binding powers 

(defmacro INFIXM (FUNC BP CODE)       `(DEFFIX /:LED ISM ,FUNC ,BP ,BP ,CODE))
                 ; multiple arguments

(defmacro DELIM  (SYMBOL)             `(DEFPROP ,SYMBOL 0. /:LBP)) 
                 ; delimiter

;; TO DO/: include examples of each.
;;; These functions (till DENFUNC) were originally preceeded by % as these
;;; were needed by macros which expanded at compile time.


 (DEFUN ISF (LISPFUNC) 
         ;; NB Assumes /:FUN, /:ISFUN and /:DRBP bound. (DEFFIX does this)
        (if (atom lispfunc) (setq lispfunc (list 'quote lispfunc)))
        (nconc (LIST /:ISFUN LISPFUNC /:DRBP)
 	      (COND ((EQ /:ISFUN 'ISM)
 		     (LIST (list 'quote /:FUN))))))
 
 (defun ISN  (FCN RB) (LIST FCN))  
 
 (defun ISS  (FCN RB) (LIST FCN /:LEFT))  
 
 (defun ISP  (FCN RB) (LIST FCN (/:PARSE RB)))  
 
 (defun ISI  (FCN RB) (LIST FCN /:LEFT (/:PARSE RB)))  
 
 (defun ISM  (FCN RB CONT) (CONS FCN (CONS /:LEFT (/:PARSELIST RB CONT))))  

(defun DENFUN fexpr (l)
        (cons 'buildfun (denfunc l)))

(DEFUN DENFUNC (CODE) 
        ;; allow use of RIGHT, RIGHTLIST, and TOKEN in code by expanding them
        ;; into appropriate functions calls. Also, substitute actual /:DRBP for the variable
        ;; of the same name in code.  /:DRBP is bound by DEFFIX.
       (SUBLIS (SUBST /:DRBP '/:DRBP
		      '((RIGHT     /:PARSE /:DRBP)
                        (token     eattoken)
			(RIGHTLIST /:PARSELIST /:DRBP '|,|)))
	       (subst /:drbp '/:drbp CODE)))


(DEFUN RIGHT NIL (/:PARSE /:DRBP)) 

(DEFUN RIGHTLIST NIL (/:PARSELIST /:DRBP '|,|)) 

(defmacro EATTOKEN nil `(prog1 /:token (READ-NEXT-TOKEN)))

(defmacro ISTOKEN (token)
          `(if (eq /:token ,token) (READ-NEXT-TOKEN) t))

(defmacro NOTTOKEN (token)
          `(not (eq /:token ,token))) 

(defun CHECK (DEL)
       (COND ((OR (AND (ATOM DEL) (EQ /:TOKEN DEL)) (MEMq /:TOKEN DEL))
	      (READ-NEXT-TOKEN))
	     ((Warning `(MISSING ,DEL INSERTED BEFORE ,/:TOKEN)))))

;;; This function originally had a % before it, for the use of the compiler.

(defun BUILDFUN fexpr (l)
        ;; returns a form by condensing its arguments as follows/:
        ;;  atom -> must match input stream, but not part of returned form
	;;  $    -> include the value of the next atom in the returned form
        ;;  "/:TOKEN" and "/:LEFT" are left to appear in the returned form
        ;;  list -> evaluate and include result in returned form
        ;;  < ... > -> optional
        ;;  < ... // Sexp > -> unless match, evaluate Sexp and include result
       (do ((pattail l (cdr pattail))
            (result) (optsw) (next))
           ((null pattail) (nreverse result))
	   (setq next (car pattail))
           (cond ((eq next '/>)
                  (setq optsw nil))
                 ((eq optsw 'ignore)
                  (if (eq next '//)
                      (pop pattail)
                      (push (eval (car pattail)) result)))
                 ((eq next '/<) (setq optsw t))
                 ((eq next '/$)
		  (pop pattail)
		  (push (eval (car pattail)) result))
                 ((eq next '/:left) (push /:left result))
                 ((eq next '/:token) (push /:token result))
                 ((atom next)
		  (cond (optsw (ifnot (istoken next) (setq optsw 'ignore)))
			((check next))))
                 ((push (eval next) result)))))



(defun /:PARSELIST (RB CONT)
       ;; collects phrases separated by CONT
       (CONS (/:PARSE RB)
	     (COND ((EQ /:TOKEN CONT) (READ-NEXT-TOKEN) (/:PARSELIST RB CONT)))))

(defun /:PARSELIST-UNLESS (rb cont stop)
       (cond ((eq /:token stop) nil)
             (t (cons (/:parse rb)
                      (cond ((istoken cont) (/:parselist-unless rb cont stop)))))))



;; Utility functions

;; Transfer Pidgin properties.
(defun VARIANTS args
       ;; (VARIANTS word v1 v2 ...)
       ;; Each variant gets the corresponding PIDGIN property of WORD.
       (do ((i 2 (1+ i))
	    (/:lbp (get (arg 1) '/:lbp))
	    (/:nud (get (arg 1) '/:nud))
	    (/:led (get (arg 1) '/:led)))
	   ((> i args))
	   (if /:lbp (putprop (arg i) /:lbp '/:lbp))
	   (if /:nud (putprop (arg i) /:nud '/:nud))
	   (if /:led (putprop (arg i) /:led '/:led))))

;; Display property lists of denotations
(DEFUN DEN fexpr (words) 
      (do-foreach x words
       (terpri)
       (Shout0 `(|  Plist of| ,X))
       (apply 'DP (cons X '(/:lbp /:nud /:led)))
       (IF (GET X '/:NUD)
	   (for (den (concatenate '/:NUD- x))
	    (Shout0 `(|  Plist of| ,den))
	    (apply 'DP (list den))))
       (IF (GET X '/:LED)
	   (for (den (concatenate '/:LED- x))
	    (Shout0 `(|  Plist of| ,den))
	    (apply 'DP (list den))))))

;; Returns a list of all words having denotations.
(defun DENLIST nil
       (prog (den)
             (mapatoms '(lambda (x) (if (getl x '(/:led /:nud /:lbp))
					(push x den))))
             (return den)))

;; Removes any denotation associated with a word.
(defun DENFLUSH (w)
       (mapc '(lambda (indicator) (remprop w indicator))
	     (list* (concatenate '/:led- w) (concatenate '/:nud- w) '(/:led /:nud /:lbp)))
       t)

;; Removes all denotations (except }).
(defun FLUSHDENOTATIONS ()
       (mapc 'denflush (delq '/} (denlist))) t)

;; Prints words having denotations, ordered alphabetically.
(defun PRINTDEN nil
       (prinl (sort (denlist) 'alphalessp) 'tblock))

;; Print words ordered by LBP.
(defun PRINTLBP nil
       (prog (lbpwords otherwords indent)
	 (do-foreach d (foreach d (denlist) (cons (get d '/:lbp) d))
	  (cond ((null (car d)) (push (cdr d) otherwords))
		(t              (push d lbpwords))))
	 (setq lbpwords (sortcar lbpwords '>)
	       otherwords (sort otherwords 'alphalessp))
	 (setq indent (- (linel t) 6.))
	 (printc '|None|)(indent-to indent)(shout-indented otherwords indent)
	 (do ((ws lbpwords      (cdr ws))
	      (w (car lbpwords) (car ws))
	      (lbp 100.))
	     ((null w))
	     (ifnot (= lbp (car w))
		    (setq lbp (car w))
		    (print lbp)
		    (indent-to indent))
	     (shout-indented (cdr w) indent)(tyo 32.))
	 ;(return (ascii 0))
	 (return nil)))

;; Converts file of Pidgin to LISPese.
(defun PIDGIN-TO-LISP fexpr (l)
       (apply 'uread l)
       (uwrite)
       (do ((^q t) (^r t) (^w t) (eof (ncons nil)) (temp))
           ((eq (setq temp (read eof)) eof)
            (apply 'ufile (list (car l) 'lisp)))
           (print temp)))

(defun PIDGIN-TO-LISP1 fexpr (l)
       ;; (PIDGIN-TO-LISP1 /# file) expands (by simply reading it) and prints
       ;; the /#+1st {} expression in file.
       (if (cdr l) (apply 'uread (cdr l)))
       (for (^q t)
	    ; skip over /# pidgin expressions.
            (do ((i (cond(l (eval (car l)))
                         (t 0))
                    (1- i)))
		((zerop i))
                (finish-pidgin-read)))            
       (for (^q t)(setq /:gtest (read '($$$))))
       (cond ((equal /:gtest '($$$)) 
              (shout0 '|End of test file.|))
             (t (cursorpos 'c)
                (sprinter /:gtest))))

(defun finish-pidgin-read nil
     (do ((ch (tyi) (tyi))) ((= ch 125.))))

;; Gobbles the remaining tokens and the closing }.


