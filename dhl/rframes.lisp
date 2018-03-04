(include declar)

;;
;;	Basic frames for rules, domains, rule-domains, etc.
;;

(declare (special domain ruledomain))

;;
;;	Add a demon to thing which if ever anything with a domain
;;		slot gets removed, have its reference in its domain
;;		removed.
;;

(fassert thing
  (domain ($if-removed (remove-domain-instance))
	  ($if-added (add-domain-instance))))

(defun add-domain-instance ()
  (fput :value 
	(fvalue-only :value 'domain-slot)
	'$value
	(fname :frame)))

(defun remove-domain-instance ()
  (fremove :value 
	   (fvalue-only :value 'domain-slot)
	   '$value
	   (fname :frame)))

(fassert rule
  (ako ($value (thing)))
  (domain)
  (condition)
  (action))	;; (etc...)

(fassert domain
  (ako ($value (thing)))
  (domain)
  (domain-slot ($value (instance-in-domain)))
  (gather ($value (get-all-things)))
  (put ($if-added (add-if-added-instance)))
  (remove ($if-added (add-if-deleted-instance)))
  (put)
  (remove))

(defun get-all-things (domain /&rest ignor)
  (fvalues-only domain (fvalue-only domain 'domain-slot)))

(defun add-if-added-instance ()
  (fput-datum (frame+ :frame) 
	      (fvalue-only (frame+ :frame)
			   'domain-slot)
	      '$if-added 
	      (fname :value)))

(defun add-if-deleted-instance ()
  (fput-datum (frame+ :frame) 
	      (fvalue-only (frame+ :frame)
			   'domain-slot)
	      '$if-removed 
	      (fname :value)))

(fassert rule-domain
  (ako ($value (domain)))
  (domain-slot ($value (rule)))
  (gather)
  (put)
  (remove)
  (domain)
  (rule))


