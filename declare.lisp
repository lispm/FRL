From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 13:03:33 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA07786; Thu, 2 Jun 88 13:03:32 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA03653; Thu, 2 Jun 88 12:43:28 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA07913; Thu, 2 Jun 88 12:43:54+0900
Date: Thu, 2 Jun 88 12:43:54+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083@tansei.cc.u-tokyo.junet>
Message-Id: <8806020343.AA07913@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: declare.lsp
Status: RO


;;;***********************************************************************
;;;          Compiler Initilization for FRL files (execpt util and lisp).
;;;***********************************************************************

(include ldeclar)

(eval-when (compile)
  (setq interpret-mode nil)
  (frl-basic-macro-load))


