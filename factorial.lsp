(defun factorial(x)
  (if (= x 0)
	1  
	(* x (factorial(- x 1)))
	))
(setq a '0)
