(setf A '((red 1.0)(green 0.9)(blue 1.0)(yellow 0.2)(white 0.4)(black 0.0)))
(setf B '((red 0.8)(green 0.7)(blue 0.8)(yellow 0.1)(white 0.5)(black 0.0)))

;linguistic variables
(setf intelligent '((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
(setf rich '((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))

#|Constructors and Selectors|#

;Fuzzy Set on Members
(defun mem(x) (caar x))

;Fuzzy Set on Degrees
(defun deg(x) (cadar x))

;Fuzzy Set on Make-pair
(defun makepair (m d) (cons m (cons d '())))

#|Functions|#
;Fuzzy Set on Maximum
(defun fmax (x m)
	(if (null x) m
		(if (> (deg x) m)
			(fmax (cdr x)(deg x))
			(fmax (cdr x) m)
                )
	)
)


;Fuzzy Set on Minimum 
(defun fmin (x m)
	(if (null x) m
		(if (< (deg x) m)
			(fmin (cdr x)(deg x))
			(fmin (cdr x) m)
		)
	)
)

#|Fuzzy Set Operations|#
;Fuzzy Set Union AUB = {max(da(x),db(x)) / x | x is an element U}
(defun funion (x y)
	(if (null x) '()
		(if (> (deg x) (deg y))
			(cons (makepair (mem x) (deg x)) (funion (cdr x) (cdr y)))
		    (cons (makepair (mem y) (deg y)) (funion (cdr x) (cdr y)))
		)
	)
)

;Fuzzy Set Intersection A(U’)B = {min(da(x),db(x)) / x | x is an element U}
(defun finter (x y)
	(if (null x) '()
		(if (< (deg x)(deg y))
			(cons (makepair (mem x)(deg x))(finter (cdr x)(cdr y)))
			(cons (makepair (mem y)(deg y))(finter (cdr x)(cdr y)))
		)
	)
)
;Fuzzy Set Complement A’ = {(1 - da(x)) / x | x is an element of U}
(defun fcomp (x)
	(if (null x) '()
		(cons (makepair (mem x) (- 1 (deg x))) (fcomp (cdr x)))
	)
)

;Fuzzy Set Normalization NORM(A) = {[da(x)/(max{da(y)})] / x | x,y are elements in U}
(defun fnorm (x)
	(defun help(y m)
		(if (null y) '()
			(cons (makepair (mem y)(/ (deg y) m)) (help (cdr y) m))
		)
	)
	(help x (fmax (cdr x)(cadar x)))
)

;Fuzzy Set Dilation DILA(A) = {sqrt(da(x)) / x | x is in U}
(defun fdil (x)
	(if (null x) '()
		(cons (makepair (mem x) (sqrt (deg x)))
		(fdil (cdr x)))
	)
)

;Fuzzy Set Concentration CON(A) = {(da(x))^2 / x | x is in U}
(defun fcon (x)
	(if (null x) '()
		(cons (makepair (mem x) (expt (deg x) 2))
		(fcon (cdr x)))
	)
)

;Fuzzy Set Intesification 
;INT(A) = {2(da(x)^2 for 0 < da(x) < 0.5
;	   1 -2(1 - da(x))^2 for 0.5 < da(x) < 1 | x is in U}
(defun fint (x)
	(if (null x) '()
		(if (> (deg x) 0.5)
			(cons (makepair(mem x)(- 1(* 2(expt (- 1 (deg x)) 2)))) (fint (cdr x)))
			(cons (makepair(mem x)(* 2(expt (deg x) 2))) (fint (cdr x)))
		)
	)
)

#|Linguistic Hedges|#
(defun sortOf (x)
	(fnorm (fint (finter (fdil x)(fint (fdil (fcomp x))))))
)

(defun inAsense (x)
	(fnorm (finter (fint x)(fcomp x)))
)

(defun someWhat (x)
	(fnorm (fint (fdil x)))
)

(defun anythingBut (x)
	(fnorm (fint (fcomp x)))
)

(defun very (x)
	(fcon x)
)

(defun reasonably (x)
	(fcon (fnorm (finter (fdil (fcon x))(fnorm (fint (fcomp x))))))
)

(defun moreOrless (x)
	(fnorm (finter (fint (fdil x))(fcomp x)))
)

(setf hedges
      '(sortOf inAsense someWhat anythingBut very reasonably moreOrless)
)

(setf hedgesList 
      '((intelligent (sortOf ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
                     (inAsense ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
                     (somewhat ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
                     (anythingBut ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
                     (very ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
                     (reasonably ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
                     (moreOrless ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0))))
        (rich (sortOf ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
              (inAsense ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
              (someWhat ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
              (anythingBut ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
              (very ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
              (reasonably ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))
              (moreOrles ((1 .01)(2 .1)(3 .4)(4 .6)(5 .8)(6 1.0)))))
)

(setf persons
      '((Ben (intelligent '((1 .2)(2 .4)(3 1.0)(4 .7)(5 .3)(6 .3)))
             (rich '((1 .8)(2 .5)(3 1.0)(4 .8)(5 .5)(6 .8))))
       (Mary (intelligent '((1 .01)(2 .1)(3 .4)(4 .6)(5 .7)(6 .8)))
              (rich '((1 .2)(2 .5)(3 .5)(4 .8)(5 .4)(6 .5)))))
)

(defun Hamming (x)
	(defun help(y m)
		(if (null y) '()
			(cons (makepair (mem y)(- (deg y) m)) (help (cdr y) m))
		)
	)
	(help x (fmax (cdr x)(cadar x)))
)

;(load "//Users//nathanael//Desktop//FuzzySet.lsp")
;This is how to load the file on CLISP on Macintosh