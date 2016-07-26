From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:03:23 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08064; Thu, 2 Jun 88 14:03:22 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04274; Thu, 2 Jun 88 13:30:14 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08120; Thu, 2 Jun 88 12:45:49+0900
Date: Thu, 2 Jun 88 12:45:49+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020345.AA08120@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: match.l.frl
Status: RO


;;
;;	A simple set of rules to define a simple match function.
;;
;;	Match is a domain which takes to patterns and a list of 
;;		current bindings.
;;

(defmacro get-match-rule-list (domain match1 match2 bindings)
  `(*fvalues-only 'match 'rule))

(fassert-rule-domain match
		     (ako ($value (rule-domain)))
		     ;;
		     ;; use system defaults for getrule, and putrule
		     ;;		and gather.
		     ;;(getrule ($value (poprule)))
		     ;;(putrule ($value (pushrule)))
		     ;;(gather ($value (get-match-rule-list)))
		     (scope ($value (lexical)))
		     (arguments ($value ((:match1 :match2 :bindings)))))

(defapply (match rule) (:match1 :match2 :bindings)
  (lambda (rule)
    `(and ,(condition rule)
	  ,(action rule))))

(defun newvar ()
  (list '*var* (gensym 'x)))

(defun is-var (x)
  (and (dtpr x)
       (eq (car x) '*var*)))

(defun read-var ()
  (list '*var* (read)))

#+lispm
(setsyntax '/? ':macro (function read-var))

#+franz
(setsyntax '/? 'macro (function read-var))

#+franz
(putprop '*var* '|?| 'printmacrochar)
;;
;;	Simple binding function.
;;		binds variables in a to their values in the assq list in
;;		bindings.
;;

(defun bind-to (a bindings)
  (cond ((atom a) a)
	((is-var a)
	 (let ((x (assq (cadr a) bindings)))
	   (cond (x (cdr x))
		 (a))))
	((cons (bind-to (car a) bindings)
	       (bind-to (cdr a) bindings)))))

;; 3 simple rules.

(fassert-rule try-recursively
	      (ako ($value (rule)))
	      (condition ($value ((and (not (atom :match1))
				       (not (atom :match2))
				       (not (is-var :match1))
				       (not (is-var :match2))))))
	      (action ($value ((let ((b1 (match (car :match1) 
						(bind-to (car :match2)
							 :bindings)
						:bindings)))
				 (cond (b1 (match (cdr :match1) 
						  (bind-to (cdr :match2)
							   b1)
						  b1)))))))
	      (domain ($value (match))))

(fassert-rule match-var
	      (ako ($value (rule)))
	      (condition ($value ((is-var :match2))))
	      (action ($value ((append (list (cons (cadr :match2)
						   :match1))
				       :bindings))))
	      (domain ($value (match))))

(fassert-rule is-equal
	      (ako ($value (rule)))
	      (condition ($value ((equal :match1 :match2))))
	      (action ($value ((or :bindings
				   (list nil)))))
	      (domain ($value (match))))

(compile-rule-domain match)


