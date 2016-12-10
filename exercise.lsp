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


(defun getwhatIwant (Lst)
	
)



; (setq path (look (list (list 0 0)) Data))
; (setq steps (reverse path))

; (format t "Movelist = ~A" path)

; (defun printf (lst)
; 	(cond ( (not (equal (rest lst) nil))
; 			(format t "~%Moving from ~A to ~A" (first lst) (second lst))
; 			(printf (rest lst)))
; 	)
; )
; (printf steps)
; (exit)


(print "Please enter row")
(defvar *row* (read))
(print "Please enter col")
(defvar *col*(read))


; (format T "~%This is the all possible moves of (~A ~A):~% ~A~%~%" *row* *col* (all-moves *row* *col* data))
(format T "~%This is the output from getwhatIwant function:~%~A~%~%" (getwhatIwant (list (list *row* *col*))))

