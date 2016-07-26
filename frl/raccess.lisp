From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:10:30 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08198; Thu, 2 Jun 88 14:10:28 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04407; Thu, 2 Jun 88 13:34:05 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08148; Thu, 2 Jun 88 12:46:12+0900
Date: Thu, 2 Jun 88 12:46:12+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020346.AA08148@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: raccess.l.frl
Status: RO


(include declar)

;;
;;	Restricted domain and inheritance functions. (dhl - 8/81)
;;
;;	Here within this file is a set of put, get, and remove
;;	functions which work with restricted fields.
;;
;;	A restricted field can be roles or domains or anything else,
;;	the user desired by setting the global variables/:
;;	
;;	/:comment-field	/:not-comment-field
;;	$facet-field	$not-facet-field
;;	and slot-field	inherit-slot-field
;;
;;	The value of slot-field(default is role) of a frame, sets
;;	the limiting values by which all the functions work,
;;	If the limiting value is a member of the /:not-comment-field,
;;	or the $not-facet-field, then those corresponding values
;;	are not visible by any function in this file.
;;
;;	Also if one of the current limiting values are a not a member of
;;	of the /:comment-field or the $facet-field when these exist, the
;;	corresponding value also can not be seen by these functions.
;;
;;	The removal functions will only remove a value in the limiting
;;	view, thus if it was visible outside the current limiting view,
;;	it will only become invisible in the current limiting view and
;;	not outside it.
;;
;;	This file is used to implement roles, multiple inheritance,
;;	and to limit knowledge in domains.
;;
;;	Uses the comment field and extra slots to mask
;;	away information that shouldn't be seen.
;;
;;	Comment on roles/:
;; 	Frame functions which use multiple inheritance using the
;;	role slot, the $role facet, and the role/: comment.
;;	Rput also adds the (role/: role) comment.
;;	which rget uses to determine if something exits currently.
;;

(declare (special /:comment-field	/:not-comment-field
		  slot-field		inherit-slot-field
		  $facet-field		$not-facet-field))

;;
;;	Set default setting for using roles.
;;

(defvar /:comment-field		'role/:)
(defvar /:not-comment-field	'not-role/:)
(defvar $facet-field		'$role)
(defvar $not-facet-field	'$not-role)
(defvar slot-field		'role)
(defvar inherit-slot-field	'role)

(defmacro get-restricted-values (f)
  `(*fvalues-only ,f slot-field))

(defmacro put-restricted-value (f r)
  `(fput ,f slot-field '$value ,r))

(defmacro add-restricted-value (f r)
  `(fput ,f slot-field '$value ,r))

(defmacro set-restricted-value (f r)
  `(freplace ,f slot-field '$value ,r))

(defmacro change-restricted-value (f oldr newr)
  `(let nil
	(fremove ,f slot-field '$value ,oldr)
	(fput ,f slot-field '$value ,newr)))

;;
;;	Functions specific to roles.
;;

(defmacro getroles (f)
  `(*fvalues-only ,f 'role))

(defmacro putrole (f r)
  `(fput ,f 'role '$value ,r))

(defmacro addrole (f r)
  `(fput ,f 'role '$value ,r))

(defmacro setrole (f r)
  `(freplace ,f 'role '$value ,r))

(defmacro changerole (f oldrole newrole)
  `(let nil
	(fremove ,f 'role '$value ,oldrole)
	(fput ,f 'role '$value ,newrole)))

;;
;;	Intersection returns non-nil if there is an element in
;;	both sets.
;;

(defmacro includeall (set1 set2)
  `(do ((s1 ,set1 (cdr s1))
	(s2 (copy ,set2)))
       ((null s1) t)
       (cond ((memq (car s1) s2)
	      (delete (car s1) s2))
	     (t (return nil)))))

(defmacro intersection (set1 set2)
  `(do ((s1 ,set1 (cdr s1))
	(s2 ,set2))
       ((null s1))
       (cond ((memq (car s1) s2)
	      (return (car s1))))))

;;
;; Basic control structures for doing restricted work.
;;

(defmacro restrict-values (values restricted-values)
  `(mapcan '(lambda (value)
		    (let ((r:value
			   (assq /:comment-field (cdr value)))
			  (not-r:value
			   (assq /:not-comment-field (cdr value))))
			 (and (or ;
				  ; no restricted comments
				  ;
				  (null r:value)
				  ;
				  ; or an intersection with the comment and
				  ; the current value.
				  ;
				  (intersection (cdr r:value)
						,restricted-values))
			      (or r:value
				  ; 
				  ; no restricting-not comments
				  ;
				  (null not-r:value)
				  ;
				  ; or no intersection with the not 
				  ; comment and
				  ; the current value.
				  ;
				  (null (includeall ,restricted-values
						    (cdr not-r:value))))
			      ;
			      ; and the value is legal.
			      ;
			      (list value))))
	   ,values))

(defmacro restricted-work (frame slot func)
  `(let ((good-values 
	  (*fdata-only ,frame ,slot $facet-field))
	 (not-good-values
	  (*fdata-only ,frame ,slot $not-facet-field))
	 (restricted-values
	  (get-restricted-values ,frame)))
	(cond ;
	      ; no restricted values on this frame.
	      ;
	      ((null restricted-values)
	       ,func)
	      ;
	      ; restricted values are currently all not good.
	      ;
	      ((or (and good-values
			(not (intersection restricted-values
					   good-values)))
		   (and (null good-values)
			(includeall restricted-values 
				    not-good-values)))
	       nil)
	      ((restrict-values ,func restricted-values)))))

;;
;;	Rget -- restricted get checks the comment field 
;;	and the restricted slots and facets, which restricts access.
;;

(defun RGET macro (args)
  ;
  ;  (RGET frame slot} facet} keywords})
  ;
  (let ((length (length args)))
       (cond ((< length 3)
	      (error '|Too few arguments -- RGET| args))
	     ((= length 3)
	      `(let ((all-values (fget ,@(cdr args))))
		    (cons (car all-values)
			  (mapcan '(lambda (x)
					   (let ((y (restricted-work 
						     ,(cadr args)
						     ,(caddr args)
						     (cdr x))))
						(cond (y (list (cons (car x)
								     y))))))
				  (cdr all-values)))))
	     (t `(let ((all-values (fget ,@(cdr args))))
		      (restricted-work ,(cadr args) ,(caddr args)
				       all-values))))))
	     
;;
;;	Rput - restricted put , put which 
;;		adds the restricted comment field.
;;

(defun RPUT macro (args)
  ;
  ;  (RPUT frame slot} facet} datum} label} message})
  ;
  (let ((length (length args)))
       (cond ((< length 3)
	      (error '|Too few arguments -- RPUT| args))
	     ((> length 7)
	      (error '|Too many arguments -- RPUT| args))
	     ;
	     ; (rput frame slot)
	     ;
	     ((= length 3)
	      `(cond ((null (rget ,(cadr args)
				  ,(caddr args)))
		      (prog1 (fbuild-frame ,@(cdr args))
			     (mapcar '(lambda (x)
					      (fput ,(cadr args)
						    ,(caddr args)
						    $facet-field
						    x))
				     (get-restricted-values ,(cadr args)))))))
	     ;
	     ; (rput frame slot facet)
	     ;
	     ((= length 4)
	      `(fbuild-frame ,@(cdr args)))
	     ;
	     ; (rput frame slot facet value {comments})
	     ;
	     (t
	      `(cond ((null (assoc ,(cadr (cdddr args))
				   (rdata ,(cadr args)
					  ,(caddr args)
					  ,(cadddr args))))
		      (let ((restricted-values 
			     (get-restricted-values ,(cadr args))))
			   (cond ((null restricted-values)
				  (fput-datum ,@(cdr args)))
				 (t ;
				    ; put data and first comment in.
				    ;
				    (fput-datum ,@(cdr args))
				    ;
				    ; add the rest of the comments if any.
				    ;
				    (let ((x (fdata1 ,(cadr args)
						     ,(caddr args)
						     ,(cadddr args))))
					 (do ((y (assoc ,(cadr (cdddr args))
							x))
					      (z restricted-values
						 (cdr z)))
					     ((null z)
					      ,(cadr (cdddr args)))
					     (delete (car z)
						     (flistget
						      ,(cadr args)
						      ,(caddr args)
						      ,(cadddr args)
						      ,(cadr (cdddr args))
						      /:not-comment-field))
					     (fput-datum-comment 
					      y
					      /:comment-field
					      (car z)))))))))))))

;;
;;	rframe - returns the frame f with only the visible stuff in it.
;;
(defmacro RFRAME (f)
  `(visibleframe (frame ,f)))

(defmacro RFFRAME (f)
  `(visibleframe (fframe ,f)))

(defmacro RFRAME/? (f)
  `(visibleframe (frame/? ,f)))

(defmacro visibledatum (dtm r)
  `(let ((good-r (assq /:comment-field (cdr ,dtm)))
	 (bad-r (assq /:not-comment-field (cdr ,dtm))))
	(and (or (null good-r)
		 (intersection ,r (cdr good-r)))
	     (or good-r
		 (null bad-r)
		 (null (includeall ,r (cdr bad-r))))
	     (list ,dtm))))

(defmacro visiblefacet (fct r)
  `(list (cons (car ,fct)
	       (mapcan '(lambda (v)
				(visibledatum v ,r))
		       (cdr ,fct)))))

(defmacro visibleslot (oneslot r)
  `(let ((good-values (assq $facet-field (cdr ,oneslot)))
	 (bad-values (assq $not-facet-field (cdr ,oneslot))))
	(and (or (null good-values)
		 (intersection ,r (mapcar 'car
					  (cdr good-values))))
	     (or good-values
		 (null bad-values)
		 (not (includeall ,r (mapcar 'car
					     (cdr bad-values)))))
	     (list (cons (car ,oneslot) 
			 (mapcan '(lambda (ss)
					  (visiblefacet ss ,r))
				 (cdr ,oneslot)))))))

(defun visibleframe (f)
  (cond ((null f)
	 nil)
	(t ((lambda (r)
		    (cond (r
			   (cons (car f)
				 (mapcan '(lambda (s)
						  (visibleslot s r))
					 (cdr f))))
			  (t f)))
	    (get-restricted-values (car f))))))

(defun rshow (f)
  ($prpr (rframe f))
  (terpri)
  nil)

;;
;;	(rexec frame newrole command) --
;;		change the role of frame to newrole.
;;		execute command as if frame had therole newrole,
;;		and then change frame's role back to its
;;		previous value.
;;

(defmacro rexec (f temp command)
  `(prog (oldr result)
	 (setq oldr (get-restricted-values ,f))
	 (put-restricted-value ,f ,temp)
	 (setq result ,command)
	 (set-restricted-value ,f (car oldr))
	 (mapc '(lambda (x)
			(add-restricted-value ,f x))
	       (cdr oldr))
	 (return result)))

;;
;; rshowif shows frame f as if it had a restricted value r.
;;	does not change the restricted values of f,
;;	but allows one to see what f would look like if you
;;	did change its restricted values.
;;
(defmacro rshowif (f r)
  `(rexec ,f ,r '(show ,f)))

(defun rslots (f)
  (let ((r (get-restricted-values f)))
       (mapcan '(lambda (s)
			(let ((good-values (assq $facet-field (cdr s)))
			      (bad-values (assq $not-facet-field (cdr s))))
			     (and (or (null good-values)
				      (intersection r 
						    (mapcar 'car
							    (cdr good-values))))
				  (or good-values
				      (null bad-values)
				      (not (includeall r 
						       (mapcar 'car
							       (cdr bad-values)))))
				  (list (car s)))))
	       (cdr (frame f)))))

(defun rcopy (f)
  (copy (rframe f)))

(defmacro restricted-value/? (f r)
  `(memq ,r (get-restricted-values ,f)))

(defmacro role/? (f r)
  `(memq ,r (getroles ,f)))

;;
;; rinstantiate - does finstantiate and sets the restricted value of th
;;	frame to be the same as the ako.
;;	Usually used with roles.
;;

(defun rinstantiate (arglist)
  `(prog (result)
	 (setq result (finstantiate ,@(cdr arglist)))
	 (set-restricted-value result (fget ,@(cadr arglist) 'ako '$value))
	 (return result)))

;;
;;	Functions for frames with restricted inheritance.
;;
;;	Rule on names of functions/:
;;	Functions with a name `f/?function' in `frl'
;;		are called `r/?function' here.
;;		or sometimes `rf/?function' depending on
;;		the name itself.
;;
;;	the rdata and rinherit functions below
;;		are taken from fherit.l and faccess.l
;;		and basically call rfframe instead of fframe.
;;		This way, only information seen in the current
;;		role is available.
;;		One other change is that the inherit
;;		functions (rinherit, rinherit1, rinherit2)
;;		use the restricted value (inherit-slot-field)
;;		chain if it exists instead of
;;		the AKO chain.  In the cases where a frame
;;		has no restricted values, then rinherit
;;		behave exactly like finherit and look along
;;		the AKO chain.
;;
;;	This can be used with roles.
;;

(defmacro onevisibleslot (oneslot sfacet r)
  `(let ((s ,oneslot))
	(let ((good-values (assq $facet-field (cdr s)))
	      (bad-values (assq $not-facet-field (cdr s))))
	     (and (or (null good-values)
		      (intersection ,r (mapcar 'car (cdr good-values))))
		  (or good-values
		      (null bad-values)
		      (not (includeall ,r (mapcar 'car (cdr bad-values)))))
		  (visiblefacet (assq ,sfacet (cdr s))
				,r)))))

(defun rlistget (aframe aslot afacet)
  (let ((r (get-restricted-values aframe)))
       (cond (r
	      (car (onevisibleslot (assq aslot (cdr (fframe aframe)))
				   afacet
				   r)))
	     ((flistget aframe aslot afacet)))))

(defun RINHERIT (:frame :slot :facet)
  ;
  ; returns list of datum structures under given frame, slot
  ; and key by following restricted or AKO links.
  ; Stops at the first frame along
  ; the path which has any non-NIL data (before processing).
  ; Note/: UNLIKE RINHERIT1 and RINHERIT2, this treats all keys alike
  ; ie, it does linear inheritance.
  ;
  (for (local-props (fbucket (rlistget :frame :slot :facet)))
       
       (cond ((or (null local-props)
		  (exists prop-str local-props
			  (fcomment/? prop-str 'finherit/: 'continue)))
	      
	      ;; continue lookup only if there are no local data 
	      ;; -or- at least one says to continue.
	      (for (parents (or (*fvalues-only :frame inherit-slot-field)
				(*fvalues-only :frame 'ako)))
		   ;; First, put together list of all immediate AKOs.
		   (cond ((null parents)
			  (funcall *mark-data-fn* local-props :frame))
			 ((apply 'union
				 (cons local-props
				       (mapcar '(lambda (:frame)
							(rinherit :frame :slot :facet))
					       parents)))))))
	     
	     ;; otherwise return local data
	     (t (funcall *mark-data-fn* local-props :frame)))))

;;
;; rinherit1 and rinherit2 currently look up the restricted slot
;;	(inherit-slot-field) for inheritance and then look up the ako slot.
;;

(defun RINHERIT1 (:frame :slot :facet)
  ;; returns list of datum structures under given frame, slot
  ;; and key by following AKO links.
  ;; note special check on $default if key is $value
  (for (local-props (or (fbucket (rlistget :frame :slot :facet))
			(and (eq :facet '$value)
			     (fbucket (rlistget :frame :slot '$default)))))
       (cond ((or (null local-props)
		  (exists prop-str local-props
			  (fcomment/? prop-str 'finherit/: 'continue)))
	      (for (parents (or (*fvalues-only :frame inherit-slot-field)
				(*fvalues-only :frame 'ako)))
		   (cond ((null parents) local-props)
			 ((apply 'union
				 (cons  local-props
					(mapcar
					 '(lambda (:frame)
						  (RINHERIT1 :frame :slot :facet))
					 parents)))))))
	     (t (funcall *mark-data-fn* local-props :frame)))))

(defun RINHERIT2 (:frame :slot :facet)
  ;; Looks first up the AKO as far as it can for a value, and if none is found,
  ;; repeats the process, looking exclusively in the $DEFAULT facet.
  (or (rinherit :frame :slot :facet)
      (and (eq :facet '$value) (rinherit :frame :slot '$default))))

;;;*****************************************************************
;;;		     DATUM RETRIEVAL using RINHERIT
;;;*****************************************************************

;; *** Do not make the DEFUNs into DEFMACROs; the bindings are needed ***

;;; These functions attempt to find a datum using RINHERIT, if no datum
;;; is found in the named frame.  Otherwise they return NIL.  Any ambiguity
;;; between a nonexistent datum and an Evaluated datum which returns NIL
;;; can be resolved by inspecting the facet with RDATUM/?.

(defun RDATA (:frame :slot :facet)
  ;; returns list of datum structures under given frame, slot, and facet.
  ;; Allows inheritance, evaluation and indirection.
  (fprocess-indics1 (rinherit :frame :slot :facet)))


(defmacro RDATA-ONLY (:frame :slot :facet)
  ;; returns list of data (without attached comments) under
  ;; given frame, slot and facet.
  ;; Allows inheritance along AKO, evaluation and indirection.
  `(findicators1 (rdata ,:frame ,:slot ,:facet)))


(defmacro RDATUM/? (:frame :slot :facet)
  ;; non-nil iff there is at least one datum (possibly inherited).
  `(rinherit ,:frame ,:slot ,:facet))


(defmacro RDATUM-ONLY (:frame :slot :facet)
  ;; same as FDATA-ONLY except expects only one and returns it directly.
  ;; Course of action if >1 piece of data found is adjustable via the
  ;; variable *DATUM-ERROR-RECOVERY* 
  `(inspect-datum (rdata-only ,:frame ,:slot ,:facet) 'rdatum-only))


(defmacro RDATUM (:frame :slot :facet)
  ;; same as FDATA except expects only one and returns it directly.
  `(inspect-datum (rdata ,:frame ,:slot ,:facet) 'rdatum))


;;;**********************************************************************
;;;		     DATUM retrievial with NO Inheritance
;;;**********************************************************************

;;; These retrieval functions look only in the named frame (1st argument)
;;; for a datum.  They return NIL if none is found.


(defun *RDATA (:frame :slot :facet)
  ;; returns list of local data under given frame, slot, and facet.
  ;; process evaluation and indirection, but not inheritance
  (fbucket (fprocess-indics (rlistget :frame :slot :facet))))


(defmacro *RDATA-ONLY (:frame :slot :facet)
  ;; returns list of local data (without attached comments) under
  ;; given frame slot and facet.
  ;; processes evaluation and indirection, but not inheritance
  `(findicators1 (*rdata ,:frame ,:slot ,:facet)))


(defmacro *RDATUM/? (:frame :slot :facet)
  `(fbucket (rlistget ,:frame ,:slot ,:facet)))


(defmacro *RDATUM-ONLY (:frame :slot :facet)
  ;; same as *rdata-only, except expects only one
  `(inspect-datum (*rdata-only ,:frame ,:slot ,:facet) '*rdatum-only))

(defmacro *RVALUES (:frame :slot)
  ;; returns list of local values structures, but processing
  ;; indirection and evaluation.
  `(*Rdata ,:frame ,:slot '$value))

(defmacro *RVALUES-ONLY (:frame :slot)
  ;; returns list of local values only, but processing
  ;; indirection and evaluation.
  `(*Rdata-only ,:frame ,:slot '$value))

(defmacro *RVALUE/? (:frame :slot)
  ;; returns non-nil iff a value is present in the named frame and slot.
  `(*rdatum/? ,:frame ,:slot '$value))

(defmacro *RVALUE-ONLY (:frame :slot)
  ;; same as *RVALUES-ONLY, except expects only one value
  `(inspect-datum (*rvalues-only ,:frame ,:slot) '*rvalue-only))

(defmacro *RVALUE (:frame :slot)
  ;; same as *RVALUES, except expects only one value.
  `(inspect-datum (*rvalues ,:frame ,:slot) '*rvalue))

;;;**********************************************************************
;;;		   Utilities for accessing VALUES
;;;**********************************************************************

;; *** Do not make the DEFUNs into DEFMACROs.  The bindings are needed! ***

;;; In addition to not requiring a facet as an argument ($VALUE is assumed),
;;; these functions attempt to find a value using RINHERIT1
;;; if the named frame and slot has none.  Otherwise, NIL is returned.

(defun RDATA1 ( :frame :slot :facet )
  (fprocess-indics1 (rinherit1 :frame :slot :facet)))


(defun RVALUES1 (:frame :slot)
  ;; returns a list of the $value datum structures of given frame and slot
  ;; following indirection, evaluation, and inheritance
  (fprocess-indics1 (rinherit1 :frame :slot '$value)))


(defmacro RVALUES-ONLY1 (:frame :slot)
  ;; returns a list of the $value data of given frame and slot
  ;; following indirection, evaluation, and inheritance.
  `(findicators1 (rvalues1 ,:frame ,:slot)))


(defmacro RVALUE1/? (:frame :slot)
  `(rinherit1 ,:frame ,:slot '$value))


(defmacro RVALUE-ONLY1 (:frame :slot)
  ;; same as rVALUES-ONLY1 except expects only one value
  ;; and returns it directly (or NIL if none).
  `(inspect-datum (rvalues-only1 ,:frame ,:slot) 'rvalue-only1))


(defmacro RVALUE1 (:frame :slot)
  ;; same as RVALUES1 except expects only one value structure.
  `(inspect-datum (rvalues1 ,:frame ,:slot) 'rvalue1))



;;; Using FINHERIT2 for inheritance.

(defun RVALUES2 (:frame :slot)
  ;; returns a list of the $value datum structures of given frame and slot
  ;; following indirection, evaluation, and inheritance
  (fprocess-indics1 (rinherit2 :frame :slot '$value)))


(defmacro RVALUES-ONLY2 (:frame :slot)
  ;; returns a list of the $value data of given frame and slot
  ;; following indirection, evaluation, and inheritance.
  `(findicators1 (rvalues2 ,:frame ,:slot)))


(defmacro RVALUE2/? (:frame :slot)
  `(rinherit2 ,:frame ,:slot '$value))


(defmacro RVALUE-ONLY2 (:frame :slot)
  ;; same as RVALUES-ONLY2 except expects only one value
  ;; and returns it directly (or NIL if none).
  `(inspect-datum (rvalues-only2 ,:frame ,:slot) 'rvalue-only2))


(defmacro RVALUE2 (:frame :slot)
  ;; same as RVALUES2 except expects only one value structure.
  `(inspect-datum (rvalues2 ,:frame ,:slot) 'rvalue2))


;;; DATUM retrieval using RINHERIT.

(defmacro RVALUES (:frame :slot)
          ;; returns list of values structures (local or inherited), processing
          ;; indirection and evaluation.
          `(rdata ,:frame ,:slot '$value))

(defmacro RVALUES-ONLY (:frame :slot)
          ;; returns list of local values only, but processing
          ;; indirection and evaluation.
          `(rdata-only ,:frame ,:slot '$value))

(defmacro RVALUE/? (:frame :slot)
          ;; returns non-nil iff a value is present in the named frame and slot.
          `(rdatum/? ,:frame ,:slot '$value))


(defmacro RVALUE-ONLY (:frame :slot)
          ;; same as rVALUES-ONLY, except expects only one value
          `(inspect-datum (rvalues-only ,:frame ,:slot) 'rvalue-only))

(defmacro RVALUE (:frame :slot)
          ;; same as RVALUES, except expects only one value.
          `(inspect-datum (rvalues ,:frame ,:slot) 'rvalue))


;;
;; Tree, fringe , children function are quickly defined,
;;	by using the ftree functions and changing them 
;;	to call rlistget instead of rlistget underneath.
;;


(defun RTREE (:frame :slot)
  ;; returns the tree formed by starting at :frame
  ;;    and following the :slot link eg, AKO, INSTANCE, SUB.
  ;; A tree is represented as a list/: (root (subtree1)(subtree2)...(subtreeN)).
  ;; NOTE/: links are not inherited, to prevent loops on AKO, etc.
  (cons (fname :frame)
	(mapcar '(lambda (f) (rtree f :slot))
		(*rvalues-only :frame :slot))))


(DEFUN RFRINGE (F S)
  ;; returns a list of the terminal nodes of the tree defined by the S link
  ;; with root F.
  ;; *** Improve this later; I'm in a hurry. ***
  (rfringe1 (rtree f s)))


(defun RFRINGE1 (tree)
  ;; returns a list of the terminal nodes of tree.
  ;; where a tree is/: (root (subtree1)(subtree2)...(subtreeN)).
  (cond((null tree) nil)
       ((null (cdr tree)) (list (car tree)))
       ((mapcan '(lambda (subtree) (rfringe1 subtree)) (cdr tree)))))


(defun RCHILDREN (frame slot)
  ;;returns a list of the immediate inferiors (children, that is) of the 
  ;;frame F along given slot link
  (*rvalues-only frame slot))


(defun RDESCENDANTS (frame slot)
  ;; returns a list of ALL descendents of frame along given link(slot);
  ;; similar to RTREE except returns list, and doesn't include root.
  ((lambda (children)
	   (setifyq
	    (append children
		    (mapcan '(lambda (f) (rdescendants f slot))
			    children))))
   (rchildren frame slot)))

(defun RFAMILY (frame slot)
  (cons (fname frame) (rdescendants frame slot)))

(defun R-COLLECT-FAMILY (f s depth)
  ;; like RFAMILY except only goes DEPTH generations along the link S.
  (cond ((null f) nil)
	((zerop depth) (ncons (fname f)))
	(t (cons (fname f)
		 (mapcan '(lambda (frame) (R-COLLECT-FAMILY frame s (1- depth)))
			 (rchildren f s))))))

(defun RSIBLINGS (frame slot inverseslot)
  ;; returns a list of all the other "children" of your "parents",
  ;; where Slot = parent and InverseSlot = child.
  (delq (fname frame)
	(setifyq (mapappend '(lambda (parent)
				     (rchildren parent inverseslot))
			    (rchildren frame slot)))))

;;
;;	Rdelete and Rremove - similar to fdelete and fremove,
;;	
;;	rdelete does not run the $if-deleted deamons,
;;	but rremove does.
;;

(macro RDELETE (call)
  ;;
  ;;  (RDELETE frame slot} facet} datum} label} message})
  ;;
  (let ((length (length call)))
       (cond ((< length 3) 
	      (error '|Too few arguments -- RDELETE| call))
	     ((> length 7)
	      (error '|Too many arguments -- RDELETE| call))
	     ((= length 3)		   ; (RDELETE frame slot)
	      (rplaca call 'rdelete-slot))
	     ((= length 4)		   ; (RDELETE frame slot facet)
	      (rplaca call 'rdelete-facet))
	     ((= length 5)		   
	      ; (RDELETE frame slot facet datum)
	      (rplaca call 'rdelete-datum))
	     ((> length 5)		   
	      ; (RDELETE frame slot facet datum label ...)
	      ;
	      (rplaca call 'flistdelete)))))

(macro RREMOVE (call)
  ;;
  ;;  (RREMOVE frame slot} facet} datum} label} message})
  ;;
  (let ((length (length call)))
       (cond ((< length 3) 
	      (error '|Too few arguments -- RREMOVE| call))
	     ((> length 7)
	      (error '|Too many arguments -- RREMOVE| call))
	     ((= length 3)		   ; (RREMOVE frame slot)
	      (rplaca call 'rremove-slot))
	     ((= length 4)		   ; (RREMOVE frame slot facet)
	      (rplaca call 'rremove-facet))
	     ((= length 5)		   
	      ; (RREMOVE frame slot facet datum)
	      (rplaca call 'rremove-datum))
	     ((> length 5)		   
	      ; (RREMOVE frame slot facet datum label ...)
	      ;
	      (rplaca call 'flistdelete)))))

(macro RREPLACE (call)
  ;;
  ;;  (RREPLACE frame slot} facet} datum} label} message})
  ;;
  (let ((length (length call)))
       (cond ((< length 5)
	      (error '|Too few arguments -- RREPLACE| call))
	     ((> length 7)
	      (error '|Too many arguments -- RREPLACE| call))
	     (t `((lambda ()
			  (rremove ,(cadr call)
				   ,(caddr call)
				   ,(cadr (cddr call)))
			  (rput ,@(cdr call))))))))

(defun is-same (x y)
  (do ((x1 (copy x))
       (y1 (copy y)))
      ((equal x1 y1)
       t)
      (cond ((and (null x1)
		  y1)
	     (return nil))
	    ((member (car x1) 
		     y1)
	     (delete (car x1) y1 1)
	     (delete (car x1) x1 1))
	    (t (return nil)))))

(defun rremove-slot (frame slot)
  (let ((r (get-restricted-values frame)))
       (cond ((null r)
	      (fremove-slot frame slot))
	     (t (let ((good-values (*fdata-only frame slot $facet-field)))
		     (cond ((is-same good-values r)
			    (fremove frame slot))
			   (t (mapc '(lambda (x)
					     (run-if-removed-methods
					      x
					      frame
					      slot))
				    (rvalues-only frame slot))
			      (mapc '(lambda (x)
					     (fremove frame
						      slot
						      $facet-field
						      x)
					     (fput frame 
						   slot 
						   $not-facet-field 
						   x))
				    r))))))))

(defun rdelete-slot (frame slot)
  (let ((r (get-restricted-values frame)))
       (cond ((null r)
	      (fdelete-slot frame slot))
	     (t (let ((good-values (*fdata-only frame slot $facet-field)))
		     (cond ((is-same good-values r)
			    (fdelete frame slot))
			   (t (mapc '(lambda (x)
					     (fdelete frame
						      slot
						      $facet-field
						      x)
					     (fput frame 
						   slot 
						   $not-facet-field 
						   x))
				    r))))))))

(defun rremove-facet (frame slot facet)
  (let ((r (get-restricted-values frame)))
       (cond ((null r)
	      (fremove-facet frame slot facet))
	     (t (let ((s (*rdata frame slot facet))
		      (good-values (*fdata-only frame slot $facet-field)))
		     (cond ((is-same good-values r)
			    (fremove-facet frame slot facet))
			   (t (mapc 
			       '(lambda 
				 (x)
				 (let ((l (cdr (assq /:comment-field 
						     (cdr x)))))
				      (cond ((is-same l r)
					     (fremove-datum
					      frame slot 
					      facet (car x)))
					    (t (cond ((eq facet '$value)
						      (run-if-removed-methods
						       (car x)
						       frame
						       slot)))
					       (mapc 
						'(lambda 
						  (y)
						  (delete y (flistget
							     frame slot
							     facet (car x)
							     /:comment-field))
						  (fput frame 
							slot
							facet
							(car x)
							/:not-comment-field
							y))
						r)))))
			       s))))))))

(defun rdelete-facet (frame slot facet)
  (let ((r (get-restricted-values frame)))
       (cond ((null r)
	      (fdelete-facet frame slot facet))
	     (t (let ((s (*rdata frame slot facet))
		      (good-values (*fdata-only frame slot $facet-field)))
		     (cond ((is-same good-values r)
			    (fdelete-facet frame slot facet))
			   (t (mapc 
			       '(lambda 
				 (x)
				 (let ((l (cdr (assq /:comment-field 
						     (cdr x)))))
				      (cond ((is-same l r)
					     (fdelete
					      frame slot 
					      facet (car x)))
					    (t (mapc 
						'(lambda 
						  (y)
						  (delete y (flistget
							     frame slot
							     facet (car x)
							     /:comment-field))
						  (fput frame 
							slot
							facet
							(car x)
							/:not-comment-field
							y))
						r)))))
			       s))))))))

(defun rdelete-datum (frame slot facet datum)
  (let ((r (get-restriced-values frame)))
       (cond ((null r)
	      (fdelete frame slot facet datum))
	     (t (let ((s (assoc datum (*rdata frame slot facet))))
		     (cond (s
			    (let ((g (cdr (assq /:comment-field 
						(cdr s)))))
				 (cond ((is-same g r)
					(fdelete frame slot
						 facet datum))
				       (t (mapc '(lambda (x)
							 (delete x (flistget
								    frame slot
								    facet datum
								    /:comment-field))
							 (fput frame slot 
							       facet datum 
							       /:not-comment-field
							       x))
						r)))))))))))

(defun rremove-datum (frame slot facet datum)
  (let ((r (get-restriced-values frame)))
       (cond ((null r)
	      (fremove-datum frame slot facet datum))
	     (t (let ((s (assoc datum (*rdata frame slot facet))))
		     (cond (s
			    (let ((g (cdr (assq /:comment-field 
						(cdr s)))))
				 (cond ((is-same g r)
					(fremove-datum frame slot
						       facet datum))
				       (t (cond ((eq 'facet '$value)
						 (run-if-removed-methods
						  datum
						  frame
						  slot)))
					  (mapc '(lambda (x)
							 (delete x (flistget
								    frame slot
								    facet datum
								    /:comment-field))
							 (fput frame slot 
							       facet datum 
							       /:not-comment-field
							       x))
						r)))))))))))



;;
;;	functions not yet written/:
;;		rput-structure
;;		rheritage
;;		rheritage-slots
;;


