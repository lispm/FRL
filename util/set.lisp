From z30083@tansei.cc.u-tokyo.junet Thu Jun  2 14:03:38 1988
Received: by aoyama.cc.aoyama.junet (3.2/6.3Junet-1.0)
	id AA08106; Thu, 2 Jun 88 14:03:38 JST
Received: by ccut.cc.u-tokyo.junet (5.51/6.3Junet-1.0/CSNET-JUNET)
	id AA04316; Thu, 2 Jun 88 13:30:41 JST
Received: by tansei.cc.u-tokyo.junet (4.12/6.3Junet-1.0)
	id AA08190; Thu, 2 Jun 88 12:46:45+0900
Date: Thu, 2 Jun 88 12:46:45+0900
From: z30083@tansei.cc.u-tokyo.junet (Masayuki Ida)
Return-Path: <z30083>
Message-Id: <8806020346.AA08190@tansei.cc.u-tokyo.junet>
To: ida@aoyama.junet
Subject: set.l.frl
Status: RO


(include declar)
(comment Set Operations)	;-*-Lisp-*-

;;; >>>>> May want to use LIBLSP; SET FASL instead.

(defun SETIFY (l)
        ;; returns L with all duplicates (compared using
        ;;  EQUAL) removed.  The order is unchanged.
        (cond((null l) nil)
             (t
              (do ((set (list (car l)))
                   (remainder (cdr l) (cdr remainder)))
                  ((null remainder) (nreverse set))
                  (cond((not (member (car remainder) set))
                        (setq set (cons (car remainder) set))))))))

 (defun SETIFYQ (l)
        ;; returns L with all duplicates (compared using
        ;;  EQ) removed.  The order is unchanged.
        (cond((null l) nil)
             (t
              (do ((set (list (car l)))
                   (remainder (cdr l) (cdr remainder)))
                  ((null remainder) (nreverse set))
                  (cond((not (memq (car remainder) set))
                        (setq set (cons (car remainder) set))))))))

;; (SETMINUS a b) returns all elements of a not in b.

(defmacro SETMINUSQ2 (A B) 
      `(do ((x ,a (cdr x)) (result))
           ((null x) result)
           (or (memq (car x) ,b)
               (setq result (cons (car x) result)))))

(defmacro SETMINUS2 (A B) 
      `(do ((x ,a (cdr x)) (result))
           ((null x) result)
           (or (member (car x) ,b)
               (setq result (cons (car x) result)))))

;; *** Rewrite setminus to be a LEXPR ***

(defun SETMINUS (a b) (nreverse (setminus2 a b)))

(defun SETMINUSQ (a b) (nreverse (setminusq2 a b)))


;; UNION takes the union of any number of sets.

(DEFUN UNION N
       (COND ((= N 2) (UNION2 (ARG 1) (ARG 2)))
	     ((DO ((I 2 (1+ I)) (RESULT (ARG 1)))
		  ((> I N) RESULT)
                  (DECLARE (FIXNUM I))
		  (SETQ RESULT (UNION2 (arg i) RESULT))))))


(DEFUN UNIONQ N
       (COND ((= N 2) (UNIONQ2 (ARG 1) (ARG 2)))
	     ((DO ((I 2 (1+ I)) (RESULT (ARG 1)))
		  ((> I N) RESULT)
                  (DECLARE (FIXNUM I))
		  (SETQ RESULT (UNIONQ2 (arg i) RESULT))))))



;; UNION2 takes the union of exactly two sets. Most efficient of the first is
;; the smallest.

(defun UNIONQ2 (a b)
       (nconc (nreverse (setminusq2 a b)) b))

(defun UNION2 (a b)
       (nconc (nreverse (setminus2 a b)) b))

;; INTERSECT takes the intersection of n sets of atoms.

(DEFUN INTERSECT N 
       (COND ((= N 2) (INTERSECT2 (ARG 1) (ARG 2)))
	     ((DO ((I 2 (1+ I)) (RESULT (ARG 1)))
		  ((> I N) RESULT)
                  (DECLARE (FIXNUM I))
		  (SETQ RESULT (INTERSECT2 RESULT (ARG I)))))))

(DEFUN INTERSECTQ N 
       (COND ((= N 2) (INTERSECTQ2 (ARG 1) (ARG 2)))
	     ((DO ((I 2 (1+ I)) (RESULT (ARG 1)))
		  ((> I N) RESULT)
                  (DECLARE (FIXNUM I))
		  (SETQ RESULT (INTERSECTQ2 RESULT (ARG I)))))))

(defun INTERSECT? (a b)
       ;; returns T iff A and B have at least one element in common using
       ;; an EQUAL test.
       ;;  (If A is smaller than B, it is more efficient.)
       (cond((null a) nil)
            ((null b) nil)
            ((do ((x a (cdr x)))
                 ((null x) nil)
                 (if (member (car x) b)
                     (return t))))))

(defun INTERSECTQ? (a b)
       ;; returns T iff A and B have at least one element in common using
       ;; an EQ test.
       ;;  (If A is smaller than B, it is more efficient.)
       (cond((null a) nil)
            ((null b) nil)
            ((do ((x a (cdr x)))
                 ((null x) nil)
                 (if (memq (car x) b)
                     (return t))))))


;; These take the intersection of exactly two sets.  The first should be the smaller,
;; for maximum efficiency.

(defun INTERSECTQ2 (a b)
       (do ((x a (cdr x)) (result))
           ((null x) (nreverse result))
           (and (memq (car x) b)
                (setq result (cons (car x) result)))))


(defun INTERSECT2 (a b)
       (do ((x a (cdr x)) (result))
           ((null x) (nreverse result))
           (and (member (car x) b)
                (setq result (cons (car x) result)))))

;;; Adding and Removing elements of a SET.

(defun SADDQ (item set)
       ;; cons item onto set iff it is not already in the set.
       (cond((null item) set)
            ((memq item set) set)
            ((cons item set))))

(defun SADD (item set)
       ;; cons item onto set iff it is not already in the set (using EQUAL).
       (cond((null item) set)
            ((member item set) set)
            ((cons item set))))

(defun SREMOVEQ (item set)
       ;; like DELQ but copies the set if deletion is necessary.
       (cond((memq item set)
             (delq item (append set nil)))
            (set)))

(defun SREMOVE (item set)
       ;; like DELETE but copies the set if deletion is necessary.
       (cond((member item set)
             (delete item (append set nil)))
            (set)))


