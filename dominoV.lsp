; The ever-popular data set
(setq data '( ((0 0 b) (1 0 b))  ((0 1 3) (0 2 2))  ((0 3 b) (0 4 4))  ((0 5 4) (0 6 5))  
	      ((1 1 5) (2 1 2))  ((1 2 3) (1 3 5))  ((1 4 b) (2 4 6))  ((1 5 6) (1 6 2))  
	      ((2 0 4) (3 0 2))  ((2 2 1) (2 3 2))  ((2 5 6) (3 5 3))  ((2 6 4) (3 6 6))  
	      ((3 1 4) (3 2 4))  ((3 3 1) (3 4 1))  ((4 0 b) (4 1 3))  ((4 2 1) (5 2 b))  
	      ((4 3 4) (5 3 3))  ((4 4 5) (4 5 1))  ((4 6 6) (5 6 1))  ((5 0 2) (5 1 2)) 
	      ((5 4 4) (6 4 1))  ((5 5 5) (6 5 5))  ((6 0 5) (7 0 6))  ((6 1 1) (6 2 3)) 
	      ((6 3 3) (7 3 3))  ((6 6 6) (7 6 6))  ((7 1 b) (7 2 5))  ((7 4 2) (7 5 b)) ) )


(defun get-value (R C Data)
  	(cond ( (null Data)
	 		(format t "Seems to be a mistake in get-value...~s ~s ~% " R C)
	 		nil
	 	  )
		  (T
	 	  	(let* ( (dom (first Data))  (left (first dom))  (right (second dom)) )
	   			 	(cond ((and (= R (first left)) (= C (second left)))
		  					; (format t "get-value ~s ~s answer ~s~%" R C (third left))
		  					(third left)
		  				  )
		 				  ((and (= R (first right)) (= C (second right)))
		  					; (format t "get-value ~s ~s answer ~s~%" R C (third right))
		  					(third right)
		  				  )
		 				  (T
		  					(get-value R C (rest Data))
		  				  )
		 			)			
		 	)
		   )
	)
 )

(defun get-newlist (Data)
  	(cond ( (null data)
	 			 		nil
	 	  )
  		(t
		  (let* ( (dom (first Data))  (left (first dom))  (right (second dom)) )
	   			  (append (list (list (first left) (second left) (third left)) (list (first right) (second right) (third right)) ) (get-newlist (rest Data)))	))						
		 )
)

(defun same-row (R C Data)
	(setf mysamevalue-row '())
	(setf Lists (get-newlist Data))
	(setf val (get-value R C Data))
	(dolist (element Lists)
		(if (and (= (first  element) R) (equal (third element) val) (not (= (second element) C)))
			(push (list (first element) (second element)) mysamevalue-row))
		)
	(setf mysamevalue-row (reverse mysamevalue-row))
	(return-from same-row mysamevalue-row)
)

(defun same-col (R C Data)
	(setf mysamevalue-col '())
	(setf Lists (get-newlist Data))
	(setf val (get-value R C Data))
	(dolist (element Lists)
		(if (and (= (second  element) C) (equal (third element) val) (not (= (first element) R)))
			(push (list (first element) (second element)) mysamevalue-col))
		)
	(setf mysamevalue-col (reverse mysamevalue-col))
	(return-from same-col mysamevalue-col)
)


(defun opposite (R C Data)
	(cond 	( 	(null Data)
				nil
			)
			(T 
				(let* ( (dom (first Data))
						(left (first dom))
						(right (second dom))
					  )
					(cond 	((and (= R (first left)) (= C (second left))) 
								(list (list (first right) (second right)))
							)
							((and (= R (first right)) (= C (second right)))
								(list (list (first left) (second left)))
							)
							(T (opposite  R C (rest Data)))

					)
				)
			)
	)
)

; (format t "The opposite of ~a is ~a ~%" (list 3 6) (opposite 3 6 data))

(defun all-moves (R C Data)
  (delete-duplicates 
   (append (same-row R C Data)
	   (same-col R C Data)
	   (opposite R C Data))
   :test 'equal))

; (format t "The value is ~a ~%" (all-moves 3 6 data))

(defun mymember (ele li)
      (cond
       ((null li) nil)
       ((equal ele (first li)) t)
       (t (mymember ele (rest li)))))
		
; (format t " Member is ~a~%" (mymember (list 0 3) (all-moves 0 0 data)))

(setf mypath '())
; (defun look (R C Data)
; 	(setf PosPath (all-moves R C Data))
; 	(cond ( (equal T (mymember (list 7 6) PosPath))
; 			(push (list 7 6) mypath)
; 			(push (list R C) mypath)  
; 		  )
; 		  ( (equal nil PosPath )
; 		  		nil
; 		  	)
		  		
; 		))



(defun look (R C Potetialist)
	(if (null Potetialist)
		nil
		(cond ((equal t (mymember (list 7 6) Potetialist))
			   (append (list (list 7 6)) (list (list R C)))
			  )
			  ((equal T (mymember (list 7 6) (look (first (first Potetialist)) (second (first Potetialist)) (all-moves (first (first Potetialist)) (second (first Potetialist)) Data))))
			  	(append (list (list 7 6)) (look (first (first Potetialist)) (second (first Potetialist)) (all-moves (first (first Potetialist)) (second (first Potetialist)) Data)) (list (list R C)))
			  )
			  (t (look (first (rest Potetialist)) (second (first Potetialist)) (all-moves (first (rest Potetialist)) (second (first Potetialist)) Data)))
		)
	)
		  
		; (look )
	
)
	

; ((cell (first Potetialist)) (row (first cell) ) (col (second cell)) )
; 		(cond 	((null Potetialist) nil)
; 				((equal T (mymember (list 7 6) (all-moves row col  Data)))
; 			; (push (list 7 6) mypath)
; 			; (push (list row col) mypath)
; 			; (push (list R C) mypath)
; 					(append (list (list 7 6)) (list (list row col)) (list (list R C)))

; 		  		)
; 		  		(append ())
; 				(t (append (look row col (rest Potetialist) )))
; 		  		; (t  nil )
; 		 )




; (look 6 0 (all-moves 6 0 data))
; (format t "All moves: ~a~%" (all-moves 5 6 data) )


; (format t "my path is ~A~%" mypath)
(format t "Path is ~A~%" (look 3 0 (all-moves 3 0 data)) )
; (format t "Path is ~A~%" (look 2 6 (all-moves 2 6 data)) )

; (exit)