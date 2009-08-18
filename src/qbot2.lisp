(load "grammar.lisp")
(load "utilities.lisp")
(load "output.lisp")
(load "grammar-weighting.lisp")

(defun getGrammarCategory (grammar category)
  (if (null grammar) '()
    (if (eq (caar grammar) category) (cadar grammar)
      (getGrammarCategory (cdr grammar) category))))

(defun generateProg (grammar &optional (type 'start) (rulenum 0))
  (let ((rule (nth rulenum (getGrammarCategory grammar type))))
    (buildRule rule grammar)))

(defun buildRule (rule grammar)
  (if (null rule) ""
    (let ( (next (car rule)))
      (cond ( (stringp next) 
	      (concat next (buildRule (cdr rule) grammar)))
	    ( (symbolp next)
	      (concat (buildRule 
		       (randElem 
			(getGrammarCategory grammar next)) grammar)
		      (buildRule (cdr rule) grammar)))
	    (T "")))))

(defun genProgTree (grammar &optional (type 'start) (rulenum 0))
  (let* ((rule (nth rulenum (getGrammarCategory grammar type))))
    (list type rulenum (buildRuleTree rule grammar))))


(defmacro parseTree (tree baseReturn symbol string list)
  `(if (null ,tree) ,baseReturn
     (let ( (next (car ,tree)))
       (cond ( (symbolp next) ,symbol)
	     ( (stringp next) ,string)
	     ( (listp next) ,list)
	     (T ,baseReturn)))))

(defun buildRuleTree (rule grammar &optional (position 0))
  (parseTree 
   rule '()
   (let* ( (weights (sumAndSort (cadr rule)))
	   (num (cdr (selectItem (cadr weights) (random (car weights))))))
     (cons (genProgTree grammar next num) (buildRuleTree (cddr rule) grammar)))
   (cons next (buildRuleTree (cdr rule) grammar))
   NIL))

(defun extractProgString (progTree)
  (parseTree 
   progTree ""
   (extractProgString (cddr progTree))
   (concat next (extractProgString (cdr progTree)))
   (concat (extractProgString next) 
	   (extractProgString (cdr progTree )))))

(defun numNodes (progTree)
  (parseTree progTree 0 
	     (+ 1 (numNodes (cddr progTree)))
	     (numNodes (cdr progTree))
	     (+ (numNodes next) (numNodes (cdr progTree)))))

(defun getNode (progTree nodeNum)
  (if (<= nodeNum 0) progTree
    (parseTree 
     progTree '()
     (getNode (caddr progTree) nodeNum)
     (getNode (cdr progTree) nodeNum)
     (let ( (nodes (numNodes next)))
       (if (< nodes nodeNum) (getNode (cdr progTree) (- nodeNum nodes))
	 (getNode next (- nodeNum 1)))))))

(defun getNodeNumsOfType (progTree type &optional (num 0))
  (parseTree 
   progTree '()
   (getNodeNumsOfType (caddr progTree) type num)
   (getNodeNumsOfType (cdr progTree) type num)
   (let ( (subType (car next))
	  (newNum (+ num 1))
	  (nodes (numNodes next)))
     (append 
      (if (eq subType type) 
	  (acons (cadr next) newNum (getNodeNumsOfType next type newNum))
	(getNodeNumsOfType next type newNum))
      (getNodeNumsOfType (cdr progTree) type (+ num nodes))))))

(defun replaceBranch (progTree branch nodeNum)
  (parseTree 
   progTree '()
   (if (<= nodeNum 0) branch
     (cons next 
	   (cons (cadr progTree) 
		 (list (replaceBranch (caddr progTree) branch nodeNum)))))
   (cons next (replaceBranch (cdr progTree) branch nodeNum))
   (let ( (nodes (numNodes next)))
     (if (< nodes nodeNum) 
	 (cons next (replaceBranch (cdr progTree) branch (- nodeNum nodes)))
       (cons (replaceBranch next branch (- nodeNum 1)) (cdr progTree))))))

(defun nodeInParent (parent nodeNum)
  (if (< nodeNum 1) NIL
    (parseTree parent NIL NIL 
	       (nodeInParent (cdr parent) nodeNum)
	       (if (= nodeNum 1) T
		 (let ( (nodes (numNodes next)))
		   (if (> nodes nodeNum) NIL
		     (nodeInParent (cdr parent) (- nodeNum nodes))))))))

(defun getNthRule (progTree n)
  (parseTree progTree '()
	     (getNthRule (caddr progTree) n)
	     (getNthRule (cdr progTree) n)
	     (if (<= n 0) next
	       (getNthRule (cdr progTree) (- n 1)))))

(defun getNodePosition (rule nodeNum)
  (if (<= nodeNum 1) 0
    (parseTree 
     rule 0 0
     (getNodePosition (cdr rule) nodeNum)
     (+ 1 (getNodePosition (cdr rule) (- nodeNum (numNodes next)))))))

(defun getChangingRule (progTree nodeNum)
  (parseTree progTree '()
	     (let ( (rule (caddr progTree)))
	       (if (nodeInParent rule nodeNum)
		   (list next (cadr progTree) (getNodePosition rule nodeNum))
		 (getChangingRule rule (- nodeNum 1))))
	     (getChangingRule (cdr progTree) nodeNum)
	     (let ( (nodes (numNodes next)))
	       (if (> nodes nodeNum) (getChangingRule next nodeNum)
		 (getChangingRule (cdr progTree) (- nodeNum nodes))))))


(defun createOffspring (prog1 prog2 grammar)
  (let* ( (nodes1 (numNodes prog1))
	  (nodes2 (numNodes prog2))
	  (branch1num (random>0 nodes1))
	  (changingRule1 (getChangingRule prog1 branch1num))
	  (branch1 (getNode prog1 branch1num))
	  (possibleBranches (getNodeNumsOfType prog2 (car branch1)))
	  (weights (apply 'getGrammarWeights grammar changingRule1))
	  (weightedBranches 
	   (sumAndSort (mapcar (lambda (x) 
				 (cons (car (nth (car x) weights)) (cdr x))) 
			       possibleBranches)))
	  (branch2num (cdr (selectItem (cadr weightedBranches) 
				       (random (car weightedBranches)))))
	  (branch2 (getNode prog2 branch2num))
	  (changingRule2 (getChangingRule prog2 branch2num))
	  (child1 (replaceBranch prog1 branch2 branch1num))
	  (child2 (replaceBranch prog2 branch1 branch2num)))
    (list child1 changingRule1 (list (car branch2) (cadr branch2))
	  child2 changingRule2 (list (car branch1) (cadr branch1)))))


(defun initialPop (size grammar runname)
  (generateNames (genPop size grammar) 0 runname))

(defun genPop (size grammar)
  (if (<= size 0) '()
    (cons (genProgTree grammar)
	  (genPop (- size 1) grammar))))

(defun sumAndSort (data)
  (list (reduce '+ (mapcar 'car data)) (sort data '> :key 'car)))

(defun selectIndividual (pop fitnessData tierWeights)
  (let* ( (tierIndex (random (car tierWeights)))
	  (tierNum (cdr (selectItem (cadr tierWeights) tierIndex)))
	  (tier (nth tierNum fitnessData))
	  (indIndex (random (car tier))))
    (assoc (cdr (selectItem (cadr tier) indIndex)) pop :test 'string=)))

(defun createNextGen (grammar pop fitnessData tierWeights 
			      &optional (popSize (length pop)))
  (if (<= popSize 0) '()
    (let* ( (parent1 (selectIndividual pop fitnessData tierWeights))
	    (parent2 (selectIndividual pop fitnessData tierWeights))
	    (childData (createOffspring (cdr parent1) (cdr parent2) grammar)))
      (cons (append (butlast childData 3) (list (car parent1)))
	    (cons (append (last childData 3) (list (car parent2)))
		  (createNextGen grammar pop fitnessData tierWeights 
				 (- popSize 2)))))))

(defun generateNames (items genNum runName &optional (indNum 0))
  (if (null items) '()
    (cons (cons (format nil "~A-g~2,'0Db~3,'0D" runName genNum indNum)
		(car items))
	  (generateNames (cdr items) genNum runName (+ indNum 1)))))

(defun flatFitnessData (fitnessData)
  (if (null fitnessData) '()
    (append (cadar fitnessData) (flatFitnessData (cdr fitnessData)))))

(defun findBotFitness (botName fitnessData)
  (if (null fitnessData) 0
    (car (find botName fitnessData :test 'string= :key 'cdr))))

(defun updateType (grammar type typenum pos num func &optional (val 1))
  (if (null grammar) '()
    (let ( (nextType (car grammar)))
      (cons (if (eq (car nextType) type) 
		(list type (updateSection (cadr nextType) typenum pos num func val))
	      nextType)
	    (updateType (cdr grammar) type typenum pos num func val)))))

(defun updateSection (type typenum pos num func val)
  (if (null type) '()
    (let ( (next (car type)))
      (cons (if (eq typenum 0) (updateRule next pos num func val)
	      next)
	    (updateSection (cdr type) (- typenum 1) pos num func val)))))

(defun updateRule (section pos num func val)
  (if (null section) '()
    (let* ( (next (car section))
	    (newPos (if (listp next) (- pos 1) pos))
	    (newNext (if (and (listp next) (eq pos 0)) (updateWeight next num func val) next)))
      (cons newNext (updateRule (cdr section) newPos num func val)))))


(defun updateWeight (weights num func val)
  (if (null weights) '()
    (cons (if (eq num 0) (cons (>0 (apply func (caar weights) (list val))) (cdar weights)) (car weights))
	  (updateWeight (cdr weights) (- num 1) func val))))

(defun updateGrammar (oldGrammar changeData oldFitness newFitness)
  (if (null changeData) oldGrammar
    (let* ( (next (car changeData))
	    (childFitness (findBotFitness (car next) newFitness))
	    (parentFitness (findBotFitness (fourth next) oldFitness))
	    (args (cons oldGrammar (append (second next) (cdr (third next))))))
      (updateGrammar (apply 'updateType 
			    (append args (list (if (< parentFitness childFitness) '+ '-))))
		     (cdr changeData) oldFitness newFitness))))


(defun evalPopExternal (pop genDir groupSize numFromGroup)
  (let* ( (extension ".c"))
    (progn
      (if (not (probe-directory genDir)) (make-dir genDir))
      (outputPop genDir pop extension)
      (print (format nil "python2.5 QBotServer.py ~A \"*~A\" ~A ~A" 
	      genDir extension groupSize numFromGroup))
      (with-open-stream (str (make-pipe-input-stream 
			    (format nil "python2.5 QBotServer.py ~A \"*~A\" ~A ~A" 
				    genDir extension groupSize numFromGroup)))
		      (read str)))))


(defun gpQuakeBots 
  (&optional (runName "TheRun")
	     (tierWeights '(100 ((40 . 0) (30 . 1) (20 . 2) (10 . 3))))
	     (genNum 0)
	     (grammar (initWeights (getGrammar) 10))
	     (pop (initialPop 512 grammar runName))
	     (groupSize 8)
	     (numFromGroup 2)
	     (baseDir "bots/")
	     (fitnessData '()))
  (progn
    (outputData baseDir runName genNum grammar pop)
    (if (< genNum 50)
	(let* ( (oldFitnessData (if (null fitnessData) 
				    (evalPopExternal pop (getGenDir baseDir runName genNum) 
					     groupSize numFromGroup) 
				  fitnessData))
		(newGenNum (+ genNum 1))
		(newPopData (createNextGen 
			     grammar pop oldFitnessData tierWeights))
		(newPop (generateNames 
			 (mapcar 'car newPopData) newGenNum runName))
		(changeData (generateNames 
			     (mapcar 'cdr newPopData) newGenNum runName))
		(newFitnessData (evalPopExternal newPop (getGenDir baseDir runName newGenNum) 
						 groupSize numFromGroup))
		(newGrammar (updateGrammar grammar changeData 
					   (sort (flatFitnessData oldFitnessData) 'string< :key 'cdr) 
					   (sort (flatFitnessData newFitnessData) 'string< :key 'cdr))))
	  (gpQuakeBots runName 
		       tierWeights 
		       newGenNum 
		       newGrammar 
		       newPop 
		       groupSize
		       numFromGroup
		       baseDir
		       newFitnessData)))
    'finished))
