(include declar)

(declare (macros t))

;;
;;	Knowledge Domains.
;;
;;	see file dhl//rule.l for function domain
;;	which allows the user to declare a domain.
;;

(defmacro gather (domain args)
  `(apply (rvalue-only ,domain 'gather) (cons ,domain ,args)))

(defun dput macro (arg)
  `(let ((/:comment-field 'domain/:)
	 (/:not-comment-field 'not-domain/:)
	 (slot-field 'domain)
	 (inherit-slot-field nil)
	 ($facet-field '$domain)
	 ($not-facet-field '$not-domain))
	(rput ,@(cdr arg))))

(defun dget macro (arg)
  `(let ((/:comment-field 'domain/:)
	 (/:not-comment-field 'not-domain/:)
	 (slot-field 'domain)
	 (inherit-slot-field nil)
	 ($facet-field '$domain)
	 ($not-facet-field '$not-domain))
	(rget ,@(cdr arg))))

(defun dremove macro (arg)
  `(let ((/:comment-field 'domain/:)
	 (/:not-comment-field 'not-domain/:)
	 (slot-field 'domain)
	 (inherit-slot-field nil)
	 ($facet-field '$domain)
	 ($not-facet-field '$not-domain))
	(rremove ,@(cdr arg))))

(defun dreplace macro (arg)
  `(let ((/:comment-field 'domain/:)
	 (/:not-comment-field 'not-domain/:)
	 (slot-field 'domain)
	 (inherit-slot-field nil)
	 ($facet-field '$domain)
	 ($not-facet-field '$not-domain))
	(rreplace ,@(cdr arg))))


