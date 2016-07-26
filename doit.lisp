From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 13:05:04 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA07806; Thu, 2 Jun 88 13:05:03 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA03678; Thu, 2 Jun 88 12:43:39 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA07932; Thu, 2 Jun 88 12:44:02+0900
Date: Thu, 2 Jun 88 12:44:02+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083@tansei.cc.u-tokyo.junet>
Message-Id: <8806020344.AA07932@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: doit.l.frl
Status: RO


;;
;;	this file if served as input into lisp will create
;;		a new FRL. (the dumplisp or savelisp is
;;		done by the user or the makefile).
;;
;;	set interpret-mode to t here if you wish to load an
;;		interpretive version of frl.
;;
;;	see file ftoplevel.l to set the main frl directory 
;;		of sources and objects (variable frl-main-dir).
;;

#+franz
(includef 'init.l)

#+franz
(sstatus ignoreeof nil)	;; to undo something in ~douglas/.lisprc
			;; since I usually create FRL - douglas
#+franz
(setq $gcprint nil)
#+franz
(gc)			;; garbage collect before dumping lisp

#+lispm
(load "hulk:ps:<frl>frlpkg")
#+lispm
(make-system 'frl ':compile)


