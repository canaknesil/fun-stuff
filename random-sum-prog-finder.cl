;;;; STORAGE

(defvar storage)

(defun reset-storage ()
  (setf storage (make-array '(5) :initial-contents
			    '(1 2 4 5 7)))) ; list to be summed

(reset-storage)

;;;; REGISTERS

(defconstant number-of-registers 5)
(defvar registers)

(defun reset-registers ()
  (setf registers (make-array `(,number-of-registers)
			      :initial-contents '(1 0 1 1 1))))

(reset-registers)

;;;; PROGRAM COUNTER

(defvar pc 0)

;;;; INSTRUCTIONS

(defclass instruction () ())

(defclass binary-instruction (instruction)
  ((op :initarg :op :accessor op)
   (r1 :initarg :r1 :accessor r1)
   (r2 :initarg :r2 :accessor r2)
   (r3 :initarg :r3 :accessor r3)))

(defclass unary-instruction (instruction)
  ((op :initarg :op :accessor op)
   (r1 :initarg :r1 :accessor r1)
   (r2 :initarg :r2 :accessor r2)))

(defclass load-instruction (instruction)
  ((dst-reg :initarg :dst-reg :accessor dst-reg)
   (loc-reg :initarg :loc-reg :accessor loc-reg)))

(defclass cjump-instruction (instruction)
  ((op :initarg :op :accessor op)
   (r1 :initarg :r1 :accessor r1)
   (r2 :initarg :r2 :accessor r2)
   (pc :initarg :pc :accessor pc)))


;;; Instruction Execute

(defgeneric execute (instruction))

(defmethod execute ((inst binary-instruction))
  (if (= (aref registers (r3 inst)) 0) 'do-nothing ; division by zero
      (setf (aref registers (r1 inst))
	    (funcall (op inst)
		     (aref registers (r2 inst))
		     (aref registers (r3 inst)))))
  (incf pc)
  registers)

(defmethod execute ((inst unary-instruction))
  (setf (aref registers (r1 inst))
	(funcall (op inst)
		 (aref registers (r2 inst))))
  (incf pc)
  registers)

(defmethod execute ((inst load-instruction))
  (let ((loc (aref registers (loc-reg inst))))
    (setf (aref registers (dst-reg inst))
	  (if (and (>= loc 0) (< loc (array-total-size storage)))
	      (aref storage loc) 0)))
  (incf pc)
  registers)

(defmethod execute ((inst cjump-instruction))
  (if (funcall (op inst) (r1 inst) (r2 inst))
      (setf pc (pc inst))
      (incf pc)))


;;; Instruction Show

(defgeneric show (instruction))

(defmethod show ((inst binary-instruction))
  (print (list (op inst) (r1 inst) (r2 inst) (r3 inst))))

(defmethod show ((inst unary-instruction))
  (print (list (op inst) (r1 inst) (r2 inst))))

(defmethod show ((inst load-instruction))
  (print (list 'load (dst-reg inst) (loc-reg inst))))

(defmethod show ((inst cjump-instruction))
  (print (list 'cjmp (op inst) (r1 inst) (r2 inst) (pc inst))))


;;; Available operations

(defconstant binary-operations
  (make-array '(5) :initial-contents
	      (list #'+ #'- #'* #'(lambda (a b) (floor (/ a b))) #'rem)))

(defconstant unary-operations
  (make-array '(2) :initial-contents
	      (list #'1+ #'1-)))

(defconstant comparison-operations
  (make-array '(5) :initial-contents
	      (list #'= #'< #'> #'<= #'>=)))


;;;; RANDOMIZATION

(defun random-binary-instruction ()
  (let ((op (aref binary-operations
		  (random (array-total-size binary-operations))))
	(r1 (random number-of-registers))
	(r2 (random number-of-registers))
	(r3 (random number-of-registers)))
    (make-instance 'binary-instruction :op op :r1 r1 :r2 r2 :r3 r3)))

(defun random-unary-instruction ()
  (let ((op (aref unary-operations
		  (random (array-total-size unary-operations))))
	(r1 (random number-of-registers))
	(r2 (random number-of-registers)))
    (make-instance 'unary-instruction :op op :r1 r1 :r2 r2)))

(defun random-load-instruction ()
  (let ((dst-reg (random number-of-registers))
	(loc-reg (random number-of-registers)))
    (make-instance 'load-instruction :dst-reg dst-reg :loc-reg loc-reg)))

(defun random-cjump-instruction (program-size)
  (let ((op (aref comparison-operations
		  (random (array-total-size comparison-operations))))
	(r1 (random number-of-registers))
	(r2 (random number-of-registers))
	(pc (random program-size)))
    (make-instance 'cjump-instruction :op op :r1 r1 :r2 r2 :pc pc)))

(defun random-instruction (program-size)
  (let ((op-type (random 4)))
    (cond ((= op-type 0) (random-binary-instruction))
	  ((= op-type 1) (random-unary-instruction))
	  ((= op-type 2) (random-load-instruction))
	  (t (random-cjump-instruction program-size)))))

;; Int -> Program
(defun random-program (max-length)
  (let* ((program-size (1+ (random max-length)))
	 (program (make-array `(,program-size))))
    (loop for i from 0 to (1- program-size) do
	 (setf (aref program i) (random-instruction program-size)))
    program))

;; Program -> ()
(defun execute-program (program max-cycle)
  (reset-registers)
  (reset-storage)
  (labels ((helper (count)
	     (cond ((>= pc (array-total-size program)) 'program-ended)
		   ((>= count max-cycle) 'max-cycle-reached)
		   (t (progn
			(execute (aref program pc))
			(helper (1+ count)))))))
    (helper 0)))

(defun show-program (program)
  (if program
      (loop for i from 0 to (1- (array-total-size program)) do
	   (show (aref program i)))
      (print nil)))

(defun try-random-program (list max-program-length max-cycle)
  (let ((program (random-program max-program-length)))
    (execute-program program max-cycle)
    (if (= (aref registers 0)
	   (reduce #'(lambda (a b) (+ a b)) list :initial-value 0))
	program nil)))

(defun find-program (list number-of-tries max-program-length max-cycle)
  "Finds a program that calculates the sum of the elements of the list
stored in the storage. Randomly generates number-of-tries programs of
random length with maximum max-program-length."
  (if (= number-of-tries 0) 'No-program-found
      (let ((program (try-random-program list max-program-length max-cycle)))
	(if program program
	    (find-program list
			  (1- number-of-tries)
			  max-program-length
			  max-cycle))))) 

#|
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

|#
