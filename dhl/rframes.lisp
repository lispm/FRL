From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:03:13 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08040; Thu, 2 Jun 88 14:03:12 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04243; Thu, 2 Jun 88 13:29:59 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08156; Thu, 2 Jun 88 12:46:18+0900
Date: Thu, 2 Jun 88 12:46:18+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020346.AA08156@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: rframes.l.frl
Status: RO


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


