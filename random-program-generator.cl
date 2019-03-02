(defconstant register-number 5)
(defvar registers)
(defconstant operations (make-array '(3) :initial-contents
				    (list #'+ #'- #'*)))

(defun reset-registers ()
  (setf registers (make-array `(,register-number)
			      :initial-contents '(1 2 3 5 7))))

(reset-registers)

;; () -> Instruction
(defun random-instruction ()
  (list (random (length operations)) ; operation
	(random register-number) ; output register
	(random register-number) ; input register 1
	(random register-number))) ; input register 2

;; Instruction -> ()
(defun execute-instruction (inst)
  (let ((op (first inst))
	(r1 (second inst))
	(r2 (third inst))
	(r3 (fourth inst)))
    (setf (aref registers r1)
	  (funcall (aref operations op)
		   (aref registers r2)
		   (aref registers r3)))
    registers))

;; Int -> Program
(defun random-program (max-length)
  (labels ((random-program-with-length (len)
	     (if (= len 0) nil
		 (cons (random-instruction)
		       (random-program-with-length (1- len))))))
    (random-program-with-length (1+ (random max-length)))))

;; Program -> ()
(defun execute-program (program)
  (reset-registers)
  (mapcar #'(lambda (inst)
	      (execute-instruction inst))
	  program))

(defun try-random-program (expected-result max-program-length)
  (let ((program (random-program max-program-length)))
    (execute-program program)
    (if (= (aref registers 0) expected-result) program nil)))

(defun find-program (expected-result number-of-tries max-program-length)
  "Finds a program that produces expected-result starting from an
initially defined set of registers. Randomly generates number-of-tries
programs of random length with maximum max-program-length."
  (if (= number-of-tries 0) 'No-program-found
      (let ((program (try-random-program expected-result max-program-length)))
	(if program program
	    (find-program expected-result
			  (1- number-of-tries)
			  max-program-length))))) 

(defun find-shorter-program (expected-result
			     number-of-tries
			     max-program-length)
  "Finds the shortest program amoung number-of-tries randomly
generated proagams that produces expected-result starting from an
initially defined set of registers."
  (labels
      ((helper (number-of-tries curr-prog)
	 (if (= number-of-tries 0) curr-prog
	     (let* ((new-prog (try-random-program expected-result
						  max-program-length))
		    (new-len (length new-prog))
		    (curr-len (length curr-prog)))
	       (if (or (= curr-len 0)
		       (and (> new-len 0) (< new-len curr-len)))
		   (helper (1- number-of-tries) new-prog)
		   (helper (1- number-of-tries) curr-prog))))))
    (helper number-of-tries nil)))

