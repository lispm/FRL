From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:04:57 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08132; Thu, 2 Jun 88 14:04:56 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04342; Thu, 2 Jun 88 13:30:57 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08126; Thu, 2 Jun 88 12:45:54+0900
Date: Thu, 2 Jun 88 12:45:54+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020345.AA08126@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: oatn.l.frl
Status: RO


(eval-when (compile)
  (load (frl-file-name oatn demo)))


;;
;;	A rule and domain system for doing 
;;		augmented transition networks (ATN).
;;
;;	Note: this atn can not have left-recursive rules.
;;	for the same reason LL parsers can not have left-recursive rules.
;;

;; some simple macros/:

(defmacro lhs (rule)
  `(*fvalue-only ,rule 'lhs))

(defmacro rhs (rule)
  `(*fvalue-only ,rule 'rhs))

(defmacro terminalp (part-of-speech)
  `(ako/? ,part-of-speech 'terminal))

(defmacro non-terminalp (part-of-speech)
  `(ako/? ,part-of-speech 'non-terminal))

(defmacro is-word (word part-of-speech)
  `(memq ,part-of-speech 
	 (*fvalues-only ,word 'part-of-speech)))

;;
;; functions to add the rules to the domains for cross-reference lookup.
;;	by the non-terminal and the first symbol they expand.
;;

(eval-when (compile load eval)
  (defun add-to-non-terminal nil
	 (fput (lhs :value) 'rules '$value (fname :value)))

  (defun remove-from-non-terminal nil
	 (fremove (lhs :value) 'rules '$value (fname :value))))

;;
;;	some initial frames
;;

(fassert token
  (ako ($value (thing))))

(fassert non-terminal
  (ako ($value (token)))
  (rules)
  (first-symbol-of-rule))

(fassert terminal
  (ako ($value (token))))

(fassert word
  (ako ($value (thing)))
  (part-of-speech))

;;
;;	domain for atn.
;;

(rule-domain atn rule-domain nil (sentence)
  (putrule pushrule)
  (getrule poprule)
  (gather get-sentence-rules)
  (scope close)
  (put add-to-non-terminal)		;; use this to sort rules
					;; by their lhs non-terminal
  (remove remove-from-non-terminal)
  (applyrule check-sentence))

;;
;;	a sub domain of atn to handle partial sentences.
;;
;;	Note/: atn assumes you are giving it a sentence, 
;;		sub-atn does not assume this, you tell it
;;		the non-terminal you are looking for.
;;

(rule-domain sub-atn atn nil (sentence non-terminal)
  ;; all other slots are inherited from the atn frame.
  (gather get-non-terminal-rules)
  (applyrule check-partial-sentence))

;;
;;	gather the possible rules via the 
;;	type of non-terminal that is wished to be parsed.
;;

;;
;; old simple definitions
;;
(defun get-sentence-rules (domain sentence)
  (*fvalues-only 'sentence 'rules))

(defun get-non-terminal-rules (domain sentence non-terminal)
  (*fvalues-only non-terminal 'rules))

(defun match-first-to-rules (non-terminal part-of-speech)
  (mapcan '(lambda (rule)
		   (let ((first (car (rhs rule))))
			(cond ((terminalp first)
			       (and (eq first part-of-speech)
				    (list rule)))
			      ((match-first-to-rules first part-of-speech)
			       (list rule)))))
	  (*fvalues-only non-terminal 'rules)))

;;
;;	checks if the sentence give is a sentence according to
;;	the given rule, all of the sentence must be used.
;;
;;	if it succeeds it returns the sentence structure,
;;	if it fails, it returns nil.
;;

(defun check-sentence (rule sentence)
  (check-partial-sentence rule sentence 'sentence))

;;
;;	takes a rule lhs /:/:= rhs where lhs = non-terminal,
;;		and tries to match it to sentence.
;;	if it succeeds, it returns 
;;	(sentence-structure . remainder-of-sentence-not-used)
;;
;;	if it fails it returns nil.
;;
;;	in the case that the non-terminal is a sentence,
;;	the entire sentence must be used by the rule.
;;

(defun check-partial-sentence (rule sentence lhs)
  (cond (sentence
	 ;
	 ; note/: bindings in do are done in parallel.
	 ;
	 (do ((r (rhs rule) (cdr r))
	      (s sentence (cdr s))
	      (word (car sentence) (cadr s))
	      (part-of-speech (car (rhs rule)) (cadr r))
	      (tree nil))
	     ((null r)
	      (cond ((eq lhs 'sentence)
		     (and (null s)
			  (cons lhs tree)))
		    (t (cons (cons lhs tree) s))))
	     (cond ((and (terminalp part-of-speech)
			 (is-word word part-of-speech))
		    (setq tree (append tree (list (list part-of-speech
							word)))))
		   ((and (non-terminalp part-of-speech)
			 (let ((x (sub-atn s part-of-speech)))
			      (cond (x (setq tree (append tree (list (car x))))
				       (setq s x))))))
		   (t (return nil)))))))

;;
;;	some needed frames for parts-of-speech.
;;

(defmacro non-terminal (p)
  `(fassert ,p 
	    (ako ($value (non-terminal)))))

(defmacro terminal (p)
  `(fassert ,p 
	    (ako ($value (terminal)))))

(non-terminal sentence)
(non-terminal nounphrase)
(non-terminal verbphrase)
(non-terminal adverblist)
(non-terminal prepositionalphrase)
(non-terminal adjectivephrase)
(non-terminal adjectivelist)
(non-terminal object)
(terminal noun)
(terminal verb)
(terminal adjective)
(terminal article)
(terminal adverb)
(terminal preposition)

;;
;;	Sentence structure rules.
;;
(declare (special rulenumber))
(defvar rulenumber 0)

(eval-when (compile)
  (setq rulenumber 0))

;;
;; (atn-rule non-terminal /:/:= handle) =>
;;
;;	(rule new-rule-name rule domain
;;	      (lhs non-terminal)
;;	      (rhs handle))
;;

(defmacro atn-rule (non-terminal /:/:= . handle)
  `(rule ,(intern (concat non-terminal (implode (explode (setq rulenumber (1+ rulenumber))))))
	 rule
	 ,(cond ((eq non-terminal 'sentence)
		 'atn)
		('sub-atn))
	 (lhs ,non-terminal)
	 (rhs ,handle)))

;;
;;	some simple sentence rules.
;;

(atn-rule sentence /:/:= nounphrase verbphrase)
(atn-rule nounphrase /:/:= noun)
(atn-rule nounphrase /:/:= article nounphrase)
(atn-rule nounphrase /:/:= adjectivelist nounphrase)
(atn-rule nounphrase /:/:= noun prepositionalphrase)
(atn-rule adjectivelist /:/:= adjectivephrase)
(atn-rule adjectivelist /:/:= adjectivephrase adjectivelist)
(atn-rule adjectivephrase /:/:= adjective)
(atn-rule adjectivephrase /:/:= adverblist adjective)
(atn-rule verbphrase /:/:= verb)
(atn-rule verbphrase /:/:= verb object adverblist)
(atn-rule verbphrase /:/:= verb object)
(atn-rule verbphrase /:/:= verb prepositionalphrase)
(atn-rule object /:/:= nounphrase)
(atn-rule verbphrase /:/:= verb object)
(atn-rule verbphrase /:/:= verb adverblist)
(atn-rule adverblist /:/:= adverb)
(atn-rule adverblist /:/:= adverb adverblist)
(atn-rule prepositionalphrase /:/:= preposition nounphrase)

;;
;;	(word newword its-part-of-speech) =>
;;		(fassert newword
;;			 (ako ($value (word)))
;;			 (part-of-speech ($value (its-part-of-speech))))
;;

(defmacro word (word part-of-speech)
  `(fassert ,word
	    (ako ($value (word)))
	    (part-of-speech ($value (,part-of-speech)))))

;;
;;	a small beginners dictionary.
;;

(word the article)
(word big adjective)
(word dog noun)
(word ate verb)
(word ran verb)
(word quickly adverb)
(word bad adjective)
(word in preposition)
(word box noun)
(word dish noun)
(word food noun)
(word went verb)
(word on preposition)
(word bone noun)

;;
;; display - display the sentence in its syntax form.
;;
(defun display fexpr (s)
  ($prpr (atn s)))


