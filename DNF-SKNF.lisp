; Власюк Олеся Сергеевна, группа 324, 2017
; Построение по ДНФ равносильной ей КНФ

; удаление символов дизъюнкции
(defun del_d (L)
	(cond ((null L) nil)
		  ((eq (car L) 'V) (del_d (cdr L)))
		  (T (cons (car L)(del_d (cdr L)))) ) )
		  
; заключение в отдельные подсписки переменных с отрицанием		  
(defun comb_not (L)
	(cond ((null L) nil)
	      ((eq (car L) '!) (cons (list '! (cadr L))(comb_not (cddr L))))
		  (T (cons (car L)(comb_not (cdr L)))) ) )

; заключение отдельных конъюнкций, образовывающих данную дизъюнкцию, в подсписки, удаление знака конъюнкции		  
(defun comb_k (L)
	(cond ((null L) nil)
	      ((eq (cadr L) '&) (cond ((or (atom (car L))(member '! L)) (comb_k (cons (list (caddr L)(car L))(cdddr L))))
								  (T (comb_k (cons (cons (caddr L)(car L))(cdddr L)))) ) )
		  (T (cond ((atom (car L)) (cons (cons (car L)())(comb_k (cdr L))))
				   (T (cons (car L)(comb_k (cdr L)))) ) ) ) )

; перемножение одной дизъюнкции с конъюнкцией (если элемент второй дизъюнкции - список)				   
(defun mul1 (a L)
	(cond ((member '!  L) (cons (cons L a)()))
		  ((null (cdr L)) (cons (cons (car L) a)()))
		  (T (append (cons (cons (car L)a)())(mul1 a (cdr L)))) ) )

; перемножение одной дизъюнкции и одного элемента второй дизъюнкции 
(defun mul_1 (a L)
	(cond ((and (not (atom a))(not (member '!  a))) (mul1 a L))
		  ((member '!  L) (cons (list a L)()))
		  ((null (cdr L)) (cons (list a (car L))()))
		  (T (append (cons (list a (car L))())(mul_1 a (cdr L)))) ) )

; произведение двух дизъюнкций		  
(defun mult_2 (L1 L2)
	(cond ((null (cdr L1)) (mul_1 (car L1) L2))
		  (T (append (mul_1 (car L1)L2)(mult_2 (cdr L1)L2))) ) )

; произведение всех дизъюнкций		  
(defun mult_s (L)
	(cond ((null (cddr L)) (mult_2 (car L)(cadr L)))
		  (T (mult_s (cons (mult_2 (car L)(cadr L))(cddr L)))) ) )			  

; удаление повторяющихся атомов		  
(defun makeset (L)
	(cond ((null L) nil)
		  ((member (car L) (cdr L)) (makeset (cdr L)))
		  (T (cons (car L) (makeset (cdr L))) ) ) )

; определение, содержится ли переменная с отрицанием из одной дизъюнкции в другой	  
(defun l_l (a L)
	(cond ((null L) nil)
		  ((atom (car L)) (l_l a (cdr L)))
		  ((eq (cadr a) (cadar L)) T)
		  (T (l_l a (cdr L))) ) )

; определение, есть ли одинаковые элементы в списке	(сравнение с учётом переменных с отрицанием)	  
(defun member_t (L1 L2)
	(cond ((null L1) T)
		  ((atom (car L1)) (cond ((member (car L1) L2) (member_t (cdr L1) L2))
								 (T nil)) )
		  ((l_l (car L1) L2) (member_t (cdr L1) L2))
		  (T nil) ) )

; определение, есть ли одинаковые атомы в списке		  
(defun makeset_pr (L)
	(cond ((null L) nil)
		  ((AND (not (eq (car L) '!))(member (car L) (cdr L))) T)
		  (T (makeset_pr (cdr L))) ) )

; снятие всех скобок в списке		  
(defun flatten_f (S)
	(cond ((null S) nil)
		  ((atom S) (cons S ()))	  
		  (T (append (flatten_f (car S))(flatten_f(cdr S)))) ) )		  

; обращение в ноль конъюнкций, содержащих логический ноль		  
(defun log_op_not (L)
	(cond ((makeset_pr (flatten_f L)) nil)
		  (T L) ) )

; преобразовывание конъюнкций: обращение необходимых в ноль и удаление лишних повторяющихся переменных		  
(defun log_op (L)
	(cond ((null L) nil)
		  (T (cons (log_op_not (makeset (car L)))(log_op (cdr L)))) ) )

; удаление нулевых конъюнкций		  
(defun del_null (L)
	(cond ((null L) nil)
		  ((null (car L)) (del_null (cdr L)))
		  (T (cons (car L)(del_null (cdr L)))) ) )	  

; определение, есть ли совпадающие диъюнкции		  
(defun sovp (L)
	(cond ((null (cdr L)) nil)
		  ((member_t (car L) (cadr L)) T)
		  (T (sovp (cons (car L) (cddr L)))) ) )

; вычитание из второго списка первого		  
(defun l2_l1_ost (L1 L2)
	(cond ((null L2) nil)
		  ((AND (atom (car L2)) (not (member (car L2) L1))) (cons (car L2) (l2_l1_ost L1 (cdr L2))))
		  ((atom (car L2)) (l2_l1_ost L1 (cdr L2)))
		  ((not (sovp (list (car L2) L1))) (cons (cadr L2) (l2_l1_ost L1 (cdr L2))))
		  (T (l2_l1_ost L1 (cdr L2))) ) )

; вычитание из второго списка переменных, содержащихся в первом (считается, что !a и a - одна и та же переменная)		  
(defun l2_l1_ost_1 (L1 L2)
	(cond ((null L2) nil)
		  ((not (makeset_pr (flatten_f (cons (car L2) L1)))) (cond ((not(atom (car L2))) (cons (cadr L2) (l2_l1_ost_1 L1 (cdr L2))))
																   (T (cons (car L2) (l2_l1_ost_1 L1 (cdr L2)))) ) )
		  (T (l2_l1_ost_1 L1 (cdr L2))) ) )

; проверка, содержит ли вторая дизъюнкция первую (если да - удаляем её) 		  
(defun part_l2_l1 (L1 L2)
	(cond ((null L2) L1)
		  ((not(l2_l1_ost L1 (car L2))) nil)
		  (T (part_l2_l1 L1 (cdr L2))) ) )

; проверка, содержит ли первая дизъюнкция вторую (если да - удаляем её)	  
(defun l1_part_l2 (L1 L2)
	(cond ((null L2) nil)
		  ((not(l2_l1_ost (car L2) L1)) (l1_part_l2 L1 (cdr L2)))
		  (T (cons (car L2) (l1_part_l2 L1 (cdr L2))) ) ) )

; правила поглощения (если в дизюнкции целиком содержится другая - удаляем данную)	  
(defun log_op_lst (L)
	(cond ((null L) nil)
		  ((null(part_l2_l1 (car L) (cdr L))) (log_op_lst (cdr L)))
		  (T (cons (car L) (log_op_lst (l1_part_l2 (car L) (cdr L)))) ) ) )

; добавление знаков дизъюнкции в один элемент конъюнкции		  
(defun ins_d (L)
	(cond ((null L) nil)
		  ((null (cdr L)) L)
		  ((null (cddr L)) (cons (car L) (cons 'V (cdr L))) )
		  (T (cons (car L) (cons 'V (ins_d (cdr L)))) ) ) )

; добавление соотвествующих знаков конъюнкции и дизъюнкции в образовавшейся КНФ		  
(defun ins_k (L)
	(cond ((AND (null (car L)) (not(null (cdr L)))) (ins_k (cdr L)))
		  ((null L) '(1))
		  ((null (cdr L)) (cons (ins_d (car L)) nil))
		  (T (cons (ins_d (car L)) (cons '& (ins_k (cdr L))) ) ) ) )

; преобразование исходной формулы в КНФ без символов		  
(defun enter_d (L)
	(del_null (log_op_lst (del_null(log_op (mult_s (comb_k (comb_not (del_d L))))))) ) )		

; преобразование КНФ без символов '&', 'V' в конечный результат	
(defun print_sok (L)	
	(ins_k (enter_d L) ))

; добавление в одну дизъюнкцию одной недостающей переменной (преобразование её в две)	
(defun make_not (a L)
	(cond ((null a) (cons L nil))
		  ( T (list (cons a L) (cons (list '! a) L))) ) )

; добавление в одну дизъюнкцию всех недостающих переменных		  
(defun make_s (L1 L2)
	(cond ((eq (car L2) '!) (make_s L1 (cdr L2)))
		  ((null (cdr L2)) (make_not (car L2) L1))
		  ((not (eq (car L2) '!)) (append (make_s (car (make_not (car L2) L1)) (cdr L2)) (make_s (cadr (make_not (car L2) L1)) (cdr L2))) )
		  (T (make_s L1 (cdr L2))) ) )

; преобразование КНФ в СКНФ посредством добавления элементов конъюнкции и переменных в них		  
(defun make_sov (L L3)
	(cond ((null L3) L)
		  ((null L) nil)
		  ((null (l2_l1_ost_1 (car L) L3) ) (cons (car L) (make_sov (cdr L) L3)) )
		  (T (append (make_s (car L) (l2_l1_ost_1 (car L) L3)) (make_sov (cdr L) L3))) ) )  

; удаление одинаковых дизъюнкций в результате образованной СКНФ		  
(defun min_k (L)
	(cond ((null L) nil)
		  ((null (sovp L)) (cons (car L) (min_k (cdr L))))
		  (T (min_k (cdr L))) ) )

; преобразование КНФ в СКНФ		  
(defun print_sov (L)
	(ins_k (min_k (make_sov (enter_d L) (makeset (flatten_f (enter_d L)))))) )

; печать примеров	
	
(print (print_sok '(A V b & ! c)))
(print (print_sov '(A V b & ! c)))
(print nil)
	
(print (print_sok '(A & ! B V B & ! c V c & ! d)))
(print (print_sov '(A & ! B V B & ! c V c & ! d)))
(print nil)

(print (print_sok '(a & ! b V b & ! c)))
(print (print_sov '(a & ! b V b & ! c)))
(print nil)

(print (print_sok '(a & b & ! c V a & b & d V b & c & d)))
(print (print_sov '(a & b & ! c V a & b & d V b & c & d)))
(print nil)

(print (print_sok '(a & ! b V b & ! c V ! d)))
(print (print_sov '(a & ! b V b & ! c V ! d)))
