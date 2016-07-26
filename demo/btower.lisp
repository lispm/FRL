From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 13:04:59 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA07794; Thu, 2 Jun 88 13:04:57 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA03676; Thu, 2 Jun 88 12:43:37 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA07918; Thu, 2 Jun 88 12:43:56+0900
Date: Thu, 2 Jun 88 12:43:56+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083@tansei.cc.u-tokyo.junet>
Message-Id: <8806020343.AA07918@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: dhl.btower.lsp
Status: RO


;;-*-Package:frl Mode:lisp-*-
;;
;;	Fast towers of hanoi example, same functionality
;;		as the file dhl//tower.l
;;
;;
;;	variables are ?atomname - used by the simple matcher in
;;		dhl//match.l
;;
(eval-when (compile)
  (load (frl-file-name match dhl)))

(fassert disk 
  (ako ($value (thing)))
  (size)
  (peg)
  (on)
  (underneath))

(fassert disk1 
  (ako ($value (disk)))
  (size ($value (1)))
  (peg ($value (peg1)))
  (on ($value (disk2))))

(fassert disk2 
  (ako ($value (disk)))
  (size ($value (2)))
  (peg ($value (peg1)))
  (on ($value (disk3)))
  (underneath ($value (disk1))))

(fassert disk3 
  (ako ($value (disk)))
  (size ($value (3)))
  (peg ($value (peg1)))
  (on ($value (disk4)))
  (underneath ($value (disk2))))

(fassert disk4 
  (ako ($value (disk)))
  (size ($value (4)))
  (peg ($value (peg1)))
  (on ($value (disk5)))
  (underneath ($value (disk3))))

(fassert disk5 
  (ako ($value (disk)))
  (size ($value (5)))
  (peg ($value (peg1)))
  (underneath ($value (disk4))))

(fassert peg (ako ($value (thing)))
	     (topdisk))

(fassert peg1 (ako ($value (peg)))
	      (topdisk ($value (disk1))))

(fassert peg2 (ako ($value (peg)))
	      (disk))

(fassert peg3 (ako ($value (peg)))
	      (disk))

(defun move (disk topeg)
  (let ((frompeg (fvalue-only disk 'peg)))
       (patom `(moving ,disk from ,frompeg to ,topeg))
       (terpr)
       (freplace disk 'peg '$value topeg)
       (let ((below (fvalue-only disk 'on)))
	    (cond (below (fremove below 'underneath '$value)
			 (freplace frompeg 'topdisk '$value below)
			 (fremove disk 'on '$value))
		  (t (fremove frompeg 'topdisk '$value))))
       (let ((oldtopdisk (fvalue-only topeg 'topdisk)))
	    (cond (oldtopdisk (freplace topeg 'topdisk '$value disk)
			      (fput oldtopdisk 'underneath '$value disk)
			      (fput disk 'on '$value oldtopdisk))
		  (t (fput topeg 'topdisk '$value disk))))))

(eval-when (compile load eval)
  ;;
  ;;	Information to help sort and select rules.
  ;;
  
  (fassert toh-rules 
	   (ako ($value (thing))))
  
  (defun slot-for-rule (:goal)
	 (let ((a (car :goal)))
	      (cond ((is-var a)
		     (length :goal))
		    (a))))
  
  ;;
  ;;	:value is the new rule being added.
  ;;
  
  (defun add-toh-rule ()
	 (fput 'toh-rules 
	       (slot-for-rule (lhs :value))
	       '$value
	       :value))
  
  (defun remove-toh-rule ()
	 (fremove 'toh-rules
		  (slot-for-rule (lhs :value))
		  '$value
		  :value)))

(defmacro best-select-toh-rule (domain :goal)
  `(cond ((*fvalues-only 'toh-rules (car ,:goal)))
	 ((*fvalues-only 'toh-rules (length ,:goal)))))

(defmacro poprule (stack)
  stack)

(defmacro pushrule (stack rule)
  `(append (list ,rule) ,stack))

(fassert-rule-domain toh
		     (ako ($value (rule-domain)))
		     (arguments ($value ((:goal))))
		     (scope ($value (lexical)))
		     ;;(putrule ($value (pushrule)))
		     ;;(getrule ($value (poprule)))
		     (gather ($value (best-select-toh-rule)))
		     (put ($value (add-toh-rule)))
		     (remove ($value (remove-toh-rule))))

(defapply (toh rule) (goal)
  (lambda (rule)
    `(let ((bindings (match goal ',(lhs rule) '(()))))
       (cond (bindings
	      (interpret (bind-to ',(rhs rule) bindings)))))))

(defun interpret (rhs)
  (cond ((atom rhs) rhs)
	((dtpr (car rhs))
	 (interpret (cons (interpret (car rhs))
			  (cond ((interpret (cdr rhs)))
				((cdr rhs))))))
	((eq (car rhs) 'quote)
	 rhs)
	((eq (car rhs) 'eval)
	 (apply (car rhs) (cdr rhs)))
	((eq (car rhs) 'and)
	 (do-and (cdr rhs)))
	((eq (car rhs) 'or)
	 (do-or (cdr rhs)))
	((toh (mapcar 'interpret rhs)))))

(defun do-and (args)
  (do ((andargs args (cdr andargs))
       (result t (interpret (car andargs))))
      ((null andargs)
       result)
      (cond ((null result)
	     (return nil)))))

(defun do-or (args)
  (cond ((null args) t)
	((do ((orargs args (cdr orargs))
	      (result nil (interpret (car orargs))))
	     (result result)
	     (cond ((null orargs)
		    (return result)))))))

(fassert-rule move-disk-rule
	      (ako ($value (rule)))
	      (lhs ($value ((move ?disk to ?peg))))
	      (rhs ($value ((and (legal-move ?disk to ?peg)
				 (eval (move '?disk '?peg))))))
	      (domain ($value (toh))))

(fassert-rule legal-move
	      (ako ($value (rule)))
	      (lhs ($value ((legal-move ?disk to ?peg))))
	      (rhs ($value ((or (and (or (?peg topdisk nil)
					 (> ((?peg topdisk) size)
					    (?disk size)))
				     (?disk underneath nil))
				(and (cleartop ?disk to ?peg)
				     (move-smaller-disks-than ?disk ?peg)
				     (legal-move ?disk to ?peg))))))
	      (domain ($value (toh))))

(fassert-rule move-smaller-disks
      (ako ($value (rule)))
      (lhs ($value ((move-smaller-disks-than ?disk ?peg))))
      (rhs ($value ((or (?peg topdisk nil)
			(> ((?peg topdisk) size)
			   (?disk size))
			(and (move (?peg topdisk) to
				   (otherpeg ?peg (?disk peg)))
			     (move-smaller-disks-than ?disk ?peg))))))
      (domain ($value (toh))))

(fassert-rule >-rule
	      (ako ($value (rule)))
	      (lhs ($value ((> ?a ?b))))
	      (rhs ($value ((eval (> (interpret '?a) 
				     (interpret '?b))))))
	      (domain ($value (toh))))

(fassert-rule cleartop
	      (ako ($value (rule)))
	      (lhs ($value ((cleartop ?disk to ?dontusethispeg))))
	      (rhs ($value ((or (?disk underneath nil)
				(and (move (?disk underneath) to
					   (otherpeg ?dontusethispeg (?disk peg)))
				     (cleartop ?disk to ?dontusethispeg))))))
	      (domain ($value (toh))))

(fassert-rule getvalue
	      (ako ($value (rule)))
	      (lhs ($value ((?x ?y))))
	      (rhs ($value ((eval (and (fframe (interpret '?x))
				       (fvalue-only (interpret '?x) 
						    (interpret '?y)))))))
	      (domain ($value (toh))))

(fassert-rule checkvalue
	      (ako ($value (rule)))
	      (lhs ($value ((?x ?y ?z))))
	      (rhs ($value ((eval (and (or (and (null (interpret '?z))
						(null (fvalues-only (interpret '?x)
								    (interpret '?y))))
					   (and (fframe (interpret '?x))
						(memq (interpret '?z)
						      (fvalues-only (interpret '?x)
								    (interpret '?y))))))))))
	      (domain ($value (toh))))

(fassert-rule otherpeg
      (ako ($value (rule)))
      (lhs ($value ((otherpeg ?peg1 ?peg2))))
      (rhs ($value ((eval (getotherpeg (interpret '?peg1)
				       (interpret '?peg2))))))
      (domain ($value (toh))))

(defun getotherpeg (peg1 peg2)
  (car (delete peg1 (delete peg2 (copy '(peg1 peg2 peg3))))))

(compile-rule-domain toh)


