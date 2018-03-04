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


