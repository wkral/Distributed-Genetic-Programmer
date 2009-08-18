(defun processAttributes (attributes)
  (parseTree attributes ""
	     (concat (format nil " ~(~A~)=" next) 
		     (processAttributes (cdr attributes)))
	     (concat (format nil "\"~A\"" next)
		     (processAttributes (cdr attributes)))
	     NIL))

(defun writeXML (data &optional (depth 0))
  (parseTree 
   data ""
   (let ( (indent (make-string depth :initial-element #\Tab))
	  (attributes (remove-if 'listp (cdr data)))
	  (rest (remove-if-not 'listp (cdr data))))
     (concat indent
	     (format nil "<~(~A~)" next) 
	     (processAttributes attributes)
	     (if (cdr data) ">~%" "")
	     (writeXML rest (+ depth 1))
	     (if (endp (cdr data)) " />~%" 
	       (format nil "~A</~(~A~)>~%" indent next))))
   (concat (make-string depth :initial-element #\Tab) next 
	   (format nil "~%"))
   (concat (writeXML next depth) (writeXML (cdr data) depth))))

(defun htmlFile (title body)
  `(html (head (title (,title))) ,body))

(defun tableRow (items)
  (cons 'tr (mapcar (lambda (x) (list 'td (list x))) items)))

(defun writeHtmlFile (fileName htmlData)
  (with-open-file 
   (str fileName :direction :output) 
   (format str (writeXML htmlData))))

(defun headingCommentList (heading comment items)
  (htmlFile 
   heading 
   `(body (h1 (,heading)) (,comment)
	  (ul ,(mapcar (lambda (x) 
			 (list 'li (list 'a 'href (car x) 
					 (cdr x)))) items)))))



(defun outputPop (dir pop ext)
  (dolist (individual pop)
    (with-open-file (str (concat dir (car individual) ext)
			 :direction :output)
		    (format str (extractProgString (cdr individual))))
    ;save as lisp code to resume runs later
    (with-open-file (str (concat dir (car individual) ".dat")
			 :direction :output)
		    (write individual :stream str))))

(defun outputGrammar (dir grammar)
  (with-open-file (str (concat dir "grammar.txt") :direction :output)
		  (pprint grammar str)))

;to be removed will be the responsiblity of the python code
(defun fitnessPage (fitnessData genNum)
  (let* ( (title (format nil "Generation ~A: Fitness Data" genNum))
	  (tableHeader (tableRow '((b ("Bot Name")) (b ("Kills/minute")))))
	  (convertIndData 
	   (lambda (x) (tableRow 
			`((a href 
			     ,(concat (cdr x) ".txt")
			     (,(cdr x)))
			  ,(format nil "~A" (car x))))))
	  (tierTables 
	   (mapcar (lambda (y) 
		     `(table border "1" 
			     ,tableHeader 
			     ,(mapcar convertIndData (cadr y))))
		   (car fitnessData))))
    (htmlFile title
	      `(body (h1 (,title)) 
		     (a href "grammar.txt" ("Grammar"))
		     (h2 ("Ranking")) 
		     ,tierTables
		     (h2 ("Game Data"))
		     ))))

(defun getGenDir (baseDir runName genNum)
  (concat baseDir runName (format nil "/gen~A/" genNum))) 

(defun outputData (dir runName genNum grammar pop)
  (let* ( (runsFile (concat dir "runs.dat"))
	  (runsHtml (concat dir "index.html"))
	  (previousRuns (with-open-file (str runsFile :if-does-not-exist NIL) 
					(if str (read str) ())))
	  (runs (if (member runName previousRuns :test 'string=) 
		    previousRuns (cons runName previousRuns)))
	  (runDir (concat dir runName "/"))
	  (runHtml (concat dir runName "/index.html"))
	  (genDir (getGenDir dir runName genNum)))
    (progn (if (not (probe-directory runDir)) (make-dir runDir))
	   (with-open-file (str runsFile :direction :output) 
			   (write runs :stream str))
	   (writeHtmlFile 
	    runHtml 
	    (headingCommentList 
	     runName
	     "Check the statistics for each of the generations for this run"
	     (mapcar (lambda (x) (list
				  (format nil "gen~A/index.html" x)
				  (format nil "Generation ~A" x)))
		     (reverse (range 0 genNum)))))
	   (if (not (probe-directory genDir)) (make-dir genDir))
	   (outputGrammar genDir grammar)
	   (writeHtmlFile 
	    runsHtml 
	    (headingCommentList 
	     "QuakeBot Evolution" 
	     "Check the status of the evolution runs below"
	     (mapcar (lambda (x) 
		       (list (concat x "/index.html") x))
		     runs))))))
