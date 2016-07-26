From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 13:05:02 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA07800; Thu, 2 Jun 88 13:05:00 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA03689; Thu, 2 Jun 88 12:43:44 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA07941; Thu, 2 Jun 88 12:44:07+0900
Date: Thu, 2 Jun 88 12:44:07+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083@tansei.cc.u-tokyo.junet>
Message-Id: <8806020344.AA07941@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: domain.l.frl
Status: RO


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


