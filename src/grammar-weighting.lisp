(defun addWeightsToRule (rule initialWeight grammar)
  (if (null rule) '()
    (let ( (next (car rule))
	   (therest (addWeightsToRule (cdr rule) initialWeight grammar)))
      (if (symbolp next)
	  (cons next
		(cons 
		 (mapcar (lambda (x) (cons initialWeight x)) 
			 (range 0 
				(length (getGrammarCategory grammar next))))
		 therest))
	(cons next therest)))))

(defun initWeights (refgrammar initialWeight &optional (grammar refgrammar))
  (if (null grammar) '()
    (let ( (category (car grammar)))
      (cons 
       (list (car category) 
	     (mapcar (lambda (x) (addWeightsToRule x initialWeight refgrammar))
		     (cadr category)))
       (initWeights refgrammar initialWeight (cdr grammar))))))

(defun getGrammarWeights (grammar category rule position)
  (getWeightsFromRulePosition 
   (nth rule (getGrammarCategory grammar category)) position))

(defun getWeightsFromRulePosition (rule position)
  (if (< position 0) '()
    (let ( (next (car rule)))
      (if (listp next) 
	  (if (eq position 0) next
	    (getWeightsFromRulePosition (cdr rule) (- position 1)))
	(getWeightsFromRulePosition (cdr rule) position)))))