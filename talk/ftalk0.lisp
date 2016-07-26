From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:03:32 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08082; Thu, 2 Jun 88 14:03:31 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04292; Thu, 2 Jun 88 13:30:25 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08086; Thu, 2 Jun 88 12:45:25+0900
Date: Thu, 2 Jun 88 12:45:25+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020345.AA08086@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: ftalk0.l.frl
Status: RO


(include declar)
;;; (requiredf '((dsk pa) pidgin fasl))

;;;**********************************************************************
;;;			       Basic FRAMISH
;;;**********************************************************************

(mapc 'define-pidgin-character '(/( /) /; /, /" /` /< />))

;;; SCOPE
;;   "( ... )"    as prefix overrides natural binding powers.
;;   "foo(1,2,3)" translates into LISP as (foo 1 2 3).

(PREFIX |(|
	0. (denfunc '(prog2 nil right (check '|)|))))

(INFIXD |(|
	30. 0.
	(DENFUNC '(CONS /:LEFT
			(cond((istoken '|)|) nil)
                             ((for (args right)
                                 (CHECK '|)|)
                                 (checklist args))))))) 

(DELIM |)|) 




;;; ITERATION
;;    "< ... >" indicates that the enclosed expression represents a list of
;;		values which are to be substituted into the expression one
;;		at a time.

(DECLARE (special iterator))

(PREFIX |<|
	0. (denfunc
	   '(for (subvar (gensym) newiterator (prog (iterator sentence)
		 (setq sentence right)
		 (cond ((path/? sentence) (setq sentence `(fextract-data ,sentence))))
		 (return (cond (iterator
			       `(apply 'nconc (mapcar
			            '(lambda (,(car iterator)) ,sentence)
			            ,(cdr iterator))))
			 (sentence)))))
		 (setq iterator (cons subvar newiterator))
		 (check '|>|)
		 (putprop subvar '(t) 'unquotable)
		 subvar)))

(DELIM |>|)



;;; PROGN
;; Note that ";" has higher precedence than IF, and thus can
;; be used to PROGNify the action of an IF statement.
(infixm /; 1. (isf 'progn))


(declare (special /:if-flag))
(setq /:if-flag nil)			     ; Used to determine if inside an IF clause.

;;; IF... THEN... (ELSE...) (ANDTHEN...)
;;    The nearest IF takes an ELSE clause, and ANDTHEN ends the nearest IF.
(prefix IF
	0. 
        (denfunc '(for (ifclause   (list 'cond (buildfun (for (/:if-flag T) right) then right))
			elseclause (if (istoken 'else) right))
                    (if elseclause
			(setq ifclause (append ifclause
					       (cond ((and (not (atom elseclause))
							   (eq (car elseclause)
							       'cond))
						      (cdr elseclause))
						     ((ncons (ncons elseclause)))))))
		    (cond ((istoken 'andthen) (list 'prog2 ifclause right))
			  (t                  ifclause)))))


;;; LISTS
;;	A, B, C => (list-build a b c) => (a b c)
(infixm /, 17. (isf 'list-build))

(defmacro list-build (a . b) `(quote (,a . ,b)))




;;    The LISP quote/: `foo' => (quote foo)
;;    NB/: This is a grave, NOT an acute accent!  Sorry, apostrophes are nice too.
;;(prefix /`
;;	0 (denfun 'quote right |'|))


;;; QUOTE
;;    The literal FRAMISH quote/: "foo" => foo, despite any denotations FOO may have.
(prefix |"|
	2 (denfunc '(prog2 nil /:token (read-next-token) (check '|"|))))

(delim |"|)




;; Logical operators
(infixm AND 8. (isf 'and))
(infixm OR  7. (isf 'or))
(prefix NOT 9. (isf 'not)) 	; equivalent to "there is not ..."
(infix  ISN 9. (denfun /' t true that 'not right))

(infix  NOT 10.
	(denfunc '(list 'not (fastcall (or (verify (get /:token '/:led)) (lederr))))))
;; RBP of "NOT" is (1- LBP)
;; "not A not equal to B" => (not (not (equal A B)))




;;; Numerical comparisions
(infix GREATER 10. (denfun '> /:left than right))
(infix LESS    10. (denfun '< /:left than right))
(infix EQUAL   10. (denfun '= /:left  to  right))

;;; Arithmetic operators/: SUM (TOTAL), DIFFERENCE, PRODUCT, QUOTIENT.

(defmacro arithop (operator)		; for all arithmetic operations
	  `'(progn
	    (check 'of)
	    (for (arg right)
	      (mapc '(lambda (operand) (cond ((path/? operand) (*pathify operand))))
	      (cond ((and (not (atom arg))
			  (eq (car arg) 'list-build))	; list of arguments
		     (rplaca arg ,operator))
		    ((and (atom arg) iterator
			  (eq arg (car iterator)))	; iterative expression
		     (setq arg (cdr iterator))
		     (setq iterator nil)
		     (cond ((eq (car arg) 'fextract-data) (setq arg (cadr arg))))
		     `(apply ',,operator ,arg))
		    (t (check 'and)
		       (list ,operator arg right)))))))

(prefix SUM 16.
	(denfunc (arithop 'plus)))

(variants 'sum   'TOTAL)


(prefix DIFFERENCE 16.
	(denfunc (arithop 'difference)))

(prefix PRODUCT 16.
	(denfunc (arithop 'times)))

(prefix QUOTIENT 16.
	(denfunc (arithop 'quotient)))

;;; Utilities.

(defun CHECKLIST (x)
       ;; always returns list.  Knows about List-building syntax.
       (cond ((and (not (atom x))
		   (eq (car x) 'LIST-BUILD))
	      (cdr x))				  ; a,b,c => (list-build a b c).
	     (t (list x))))


