;;;; microsim-assembler.lisp

(in-package #:microsim-assembler)


(defparameter *valid-op-codes* '("LDA_I" "LDA_M" "JMP"))

(defparameter *line-cnt* 0)
(defparameter *address* #x100)
(defparameter *labels* ())
(defparameter *output* ())

(defun initialise ()
  (setq *line-cnt* 0)
  (setq *address* #x100)
  (setq *labels* ())
  (setq  *output* ()))

(defun parse-hex (s)
  (if (string= (str:substring 0 2 s) "0x")
      (return-from parse-hex (parse-integer (str:substring 2 t s) :radix 16)))
  (if (string= (str:substring 0 1 s) "$")
      (return-from parse-hex (parse-integer (str:substring 1 t s) :radix 16))))

(defun prepend-and-inc-address(n)
  (let ((old-address *address*))
	(setq *address* (+ *address* 1))
	(return-from prepend-and-inc-address (list old-address n))))

(defun process-label (s)
  (format t "~a -> LABEL~%" s)
  (setq s (str:remove-punctuation s))
  (setq *labels* (cons (cons s *address*) *labels*)))

(defun process-op-code (s)
  (format t "~a -> OP-CODE~%" s)
  (let ((tokens (str:words s)))
    (cond
      ((str:starts-with-p "LDA_I" s)
       (process-op-code-lda_i tokens))
      ((str:starts-with-p "LDA_M" s)
       (process-op-code-lda_m tokens))
      ((str:starts-with-p "JMP" s)
       (process-op-code-jmp tokens))))
  )

(defun process-op-code-lda_i (tokens)
  (format t "LDA_I ================ ~a~%" (second tokens))
  (setq *output* (append *output* (prepend-and-inc-address '1)))
  (setq *output* (append *output* (prepend-and-inc-address (parse-integer (second tokens))))))

(defun process-op-code-lda_m (tokens)
  (format t "LDA_M ================ ~a~%" (second tokens))
  (setq *output* (append *output* (prepend-and-inc-address '2)))
  (let ((addr (cdr (assoc (second tokens) *labels* :test #'equalp))))
    (setq *output* (append *output* (prepend-and-inc-address (nth-value 0 (truncate addr 256)))))
    (setq *output* (append *output* (prepend-and-inc-address (nth-value 1 (truncate addr 256)))))))

(defun process-op-code-jmp (tokens)
  (format t "JMP ================ ~a~%" (second tokens))
  (setq *output* (append *output* (prepend-and-inc-address '10)))
  (let ((addr (cdr (assoc (second tokens) *labels* :test #'equalp))))
    (setq *output* (append *output* (prepend-and-inc-address (nth-value 0 (truncate addr 256)))))
    (setq *output* (append *output* (prepend-and-inc-address (nth-value 1 (truncate addr 256)))))))

(defun process-org (s)
  (format t "~a -> ORG ~%" s)
  (let ((target_addr (second(str:words s))))
    (format t "     ----> ~a ~%" target_addr)
    (setq *address* (parse-hex target_addr))
    (format t "  Address is ~x  /  ~d ~%" *address* *address*)))

(defun parse-asm-file ()
  (initialise)
  (let ((in (open "/home/barry/software/projects/microsim-assembler/test.asm" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
          while line do
	    (progn
	      (incf *line-cnt*)
	      (setq line (str:trim line))
	      (format t "~a~%" line)
	      (cond
		((str:blankp line)
		 (format t "-> BLANK~%"))
		((str:starts-with-p "#" line)
		 (format t "-> COMMENT~%"))
		((str:starts-with-p "ORG" line)
		 (process-org line))
		((str:ends-with-p ":" line)
		 (process-label line))
		((if (member (car (str:words line)) *valid-op-codes* :test #'string=) t nil)
		 (process-op-code line))
		(t
		 (format t "-> ERROR~%")))
	      ))
    (close in))))
