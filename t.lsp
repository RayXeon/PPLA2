(setq data '( ((0 0 b) (1 0 b))  ((0 1 3) (0 2 2))  ((0 3 b) (0 4 4))  ((0 5 4) (0 6 5))
			  ((1 1 5) (2 1 2))  ((1 2 3) (1 3 5))  ((1 4 b) (2 4 6))  ((1 5 6) (1 6 2))
			  ((2 0 4) (3 0 2))  ((2 2 1) (2 3 2))  ((2 5 6) (3 5 3))  ((2 6 4) (3 6 6))
			  ((3 1 4) (3 2 4))  ((3 3 1) (3 4 1))  ((4 0 b) (4 1 3))  ((4 2 1) (5 2 b))
			  ((4 3 4) (5 3 3))  ((4 4 5) (4 5 1))  ((4 6 6) (5 6 1))  ((5 0 2) (5 1 2))
			  ((5 4 4) (6 4 1))  ((5 5 5) (6 5 5))  ((6 0 5) (7 0 6))  ((6 1 1) (6 2 3))
			  ((6 3 3) (7 3 3))  ((6 6 6) (7 6 6))  ((7 1 b) (7 2 5))  ((7 4 2) (7 5 b)) ))

(defun get-value (R C Data)
  (cond ((null data)
	 ; (format t "Seems to be a mistake in get-value...~s ~s ~% " R C)
	 nil)
	(T
	 (let* ((dom (first data)) (left (first dom)) (right (second dom)))
	   (cond ((and (= R (first left)) (= C (second left)))
		  ; (format t "get-value ~s ~s answer ~s~%" R C (third left))
		  (third left))
		     ((and (= R (first right)) (= C (second right)))
		  ; (format t "get-value ~s ~s answer ~s~%" R C (third right))
		  (third right))
		 (T
		  (get-value R C (rest data)))
		 )))))

(defun dataToArray (Data)
	(cond ((null Data) nil)
		  (T(let* ((dom (first Data)) (left (first dom)) (right (second dom)))
				(append  (list left right) (dataToArray (rest Data)))))))

(defun remove-self (R C Array)
	(let* (notcontainself)
		(dolist (element Array)
			(if (not (and (equal R (first element)) (equal C (second element))))
				(setq notcontainself (append notcontainself (list element)))
			)
		)
		(return-from remove-self notcontainself)
	)
)

(defun all-moves (R C Data)
  (delete-duplicates 
   (append (same-row R C Data)
	   (same-col R C Data)
	   (opposite R C Data))
   :test 'equal))

(defun same-row (R C Data)
	(let* ((value (get-value R C Data)) (cells (dataToArray Data)) (desireList))
		 (dolist (cell cells)
			(if  (and (equal R (first cell))  (equal value (third cell)) )
				 (setq desireList (append desireList (list (list (first cell) (second cell)))))
			)
		 )
		 (return-from same-row (remove-self R C desireList))	
	)
)

(defun same-col (R C Data)
	(let* ((value (get-value R C Data)) (cells (dataToArray Data)) (desireList))
		(dolist (cell cells)
			(if (and (equal C (second cell))  (equal value (third cell)))
				(setq desireList (append desireList (list (list (first cell) (second cell)))))
			)
		)
		(return-from same-col (remove-self R C desireList))
	)
)

(defun opposite (R C Data)
	(let* ((dom (first Data)) (left (first dom)) (right (second dom)) )  
		(cond ((null Data) nil)
			  ((and (= R (first left)) (= C (second left)))
			  	(list (list (first right) (second right)))
			  )
			  ((and (= R (first right)) (= C (second right)))
			  	(list (list (first left) (second left)))
			  )
			  (T (opposite R C (rest Data)))
		)     
	)
)


; (defun path (Lists Data)
; 	(let* ((path) (cell (first Lists)) (R (first cell)) (C (second cell)) (possibleMove (all-moves R C Data)) )
; 		(cond ( (equal nil Lists)
; 				nil
; 			  )
; 			  ( (member (list 7 6) Lists :test 'equal)
; 			  	(setq path (append   (list (list 7 6)) path))
; 			  	; (append path (list (list 7 6)))
; 			  	; (format T "True")
; 			  )
; 			  ( (member (list 7 6) possibleMove :test 'equal)
; 			  	(setq path (append (list (list 7 6)) (list (list R C)) path))
; 			  	; (append path (path possibleMove Data))
; 			  )	
; 			  (T
; 			  	(append (path (rest Lists) Data))
			  		
; 			  )
; 		)
; 		; (return-from path path)
; 	)
; )


(defun finder (R C Data)
	(let* ((path) (lst (all-moves R C Data)) )
		(cond ( (null lst) nil)
			  (	(equal (list R C) (list 7 6))
			  	(setq path (list (list 7 6)))  
			  )
			  ( (member (list 7 6) lst :test 'equal)
			  		(setq path (append (list (list 7 6))  (list (list R C)) ))
			  		(print path)
			  )
			  (T (dolist (ele lst)
			  		(let* ((row (first ele)) (col (second ele)) )
			  			
			  				(setq path (append (list (list row col)) (finder row col Data)))
			  			
			  		)
			  	 ) 
			  )
			  
    	)
    	(format t "the path is ~A~%" path)
	)
)
(defun sum1 (N)
	(cond ((= N 0) 0)
		  ((= N 1) 1)
		  (T (+ N (sum1 (- N 1))))
	)
)
; (format t "This is ~A~%" (same-row 3 1 data))
; (format T "This is the opposite ~A~%" (opposite 6 6 data))
; (format t "The lists are ~A~%" (same-col 3 6 data))

(format T "All possible moves ~A~%" (all-moves 0 0 data))
; (format T "This is the path ~A~%" (path (all-moves 2 0 data) data))
; (finder (all-moves 0 0 data) data)
(finder 2 6 data)
(format T "Try this path ~A~%" (finder (all-moves 0 0 data) data))

