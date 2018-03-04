;;; -*- Mode: LISP; Lowercase: Yes -*-
;;
;;
;;   file to create frl on the lisp machine.
;;        this file is only for the lisp machine.
;;

(package-declare frl global 20000. ()
		 (shadow select if firstn  listp logand for push pop union
			 intersection readline) 
		 ())

(defun compile-frl ()
    (make-system 'frl ':compile ':noconfirm))

(defun load-frl ()
  (make-system 'frl))

(load frllib//fauxfns)

(eval `(defsystem frl
		  (:name 
		   "Frame Representation Language")
		  (:short-name 
		   "frl")
		  (:package frl)
		  (:pathname-default 
		   ,(frl-file-name ))
		  (:module declarations (,(frl-file-name ldeclar)))
		  (:module frllib (,(frl-file-name frllib fauxfns) 
				   ,(frl-file-name frllib ftop)))
		  (:module frl-utility-macro (,(frl-file-name util cntrl) 
					      ,(frl-file-name util set) 
					      ,(frl-file-name util util)))
		  (:module frl-fmacro (,(frl-file-name frl fmacro)))
		  (:module frl-basic-macro (,(frl-file-name frl faccess) 
					    ,(frl-file-name frl fassert) 
					    ,(frl-file-name frl raccess)))
		  (:module dhl-frames (,(frl-file-name dhl rframes)))
		  (:module dhl-domain-macro (,(frl-file-name dhl domain)))
		  (:module dhl-rule (,(frl-file-name dhl rule)))
		  (:module frl-utility (,(frl-file-name util sutil) 
					,(frl-file-name util ftrace)))
		  (:module frl-basic (,(frl-file-name frl futil) 
				      ,(frl-file-name frl flist) 
				      ,(frl-file-name frl fherit) 
				      ,(frl-file-name frl freq) 
				      ,(frl-file-name frl ttyio)
				      ,(frl-file-name frl fdump) 
				      ,(frl-file-name frl fask) 
				      ,(frl-file-name frl thing) 
				      ,(frl-file-name frl thing1)))
		  (:module dhl-match (,(frl-file-name dhl match)))
		  (:module oil (,(frl-file-name oil foil) 
				,(frl-file-name oil oil) 
				,(frl-file-name oil demo)))
		  (:module pidgin (,(frl-file-name talk pidgin)))
		  (:module talk (,(frl-file-name talk ftalk0) 
				 ,(frl-file-name talk frmish) 
				 ,(frl-file-name talk rulish)))
		  (:module rule (,(frl-file-name rule sentin) 
				 ,(frl-file-name rule rule) 
				 ,(frl-file-name rule rtemp)))
		  (:module demo (,(frl-file-name demo atn)
				 ,(frl-file-name demo oatn)
				 ,(frl-file-name demo tower)
				 ,(frl-file-name demo btower)))
		  (:compile-load declarations)
		  (:compile-load frllib (:fasload declarations)
				 (:fasload declarations))
		  (:compile-load frl-utility-macro (:fasload frllib)
				 (:fasload frllib))
		  (:compile-load frl-fmacro (:fasload frl-utility-macro)
				 (:fasload frl-utility-macro))
		  (:compile-load frl-basic-macro (:fasload frl-fmacro)
				 (:fasload  frl-fmacro))
		  (:compile-load frl-utility
				 (:fasload frl-basic-macro)
				 (:fasload frl-basic-macro))
		  (:compile-load frl-basic
				 (:fasload frl-utility)
				 (:fasload frl-utility))
		  (:compile-load rule
				 (:fasload frl-basic)
				 (:fasload frl-basic))
		  (:compile-load dhl-frames
				 (:fasload rule)
				 (:fasload rule))
		  (:compile-load dhl-domain-macro
				 (:fasload dhl-frames)
				 (:fasload dhl-frames))
		  (:compile-load dhl-rule
				 (:fasload dhl-domain-macro)
				 (:fasload dhl-domain-macro))))

(comment
  (:compile-load dhl-match
		 (:fasload dhl-rule)
		 (:fasload dhl-rule)))

(comment
  (:compile-load oil
		 (:fasload rule)
		 (:fasload rule))
  (:compile-load pidgin
		 (:fasload rule)
		 (:fasload rule))
  (:compile-load talk
		 (:fasload pidgin)
		 (:fasload pidgin))
  (:compile demo
	    (:fasload dhl-match)))



