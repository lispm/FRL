;;;***********************************************************************
;;;          Compiler Initilization for FRL files (execpt util and lisp).
;;;***********************************************************************

(include ldeclar)

(eval-when (compile)
  (setq interpret-mode nil)
  (frl-basic-macro-load))


