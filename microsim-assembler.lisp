;;;; microsim-assembler.lisp

(in-package #:microsim-assembler)


(defparameter *valid-op-codes* '("LDA_I" "JMP"))

(defparameter *line-cnt* 0)
(defparameter *address* #x100)
(defparameter *labels* ())

(defun initialise ())

(defun parse-hex (s)
  (if (string= (str:substring 0 2 s) "0x")
      (return-from parse-hex (parse-integer (str:substring 2 t s) :radix 16)))
  (if (string= (str:substring 0 1 s) "$")
      (return-from parse-hex (parse-integer (str:substring 1 t s) :radix 16))))

(defun process-label (s)
  (format t "~a -> LABEL~%" s)
  (setq s (str:remove-punctuation s))
  (setq *labels* (cons (list s *address*) *labels*)))

(defun process-op-code (s)
  (format t "~a -> OP-CODE~%" s))

(defun process-org (s)
  (format t "~a -> ORG ~%" s)
  (let ((target_addr (second(str:words s))))
    (format t "     ----> ~a ~%" target_addr)
    (setq *address* (parse-hex target_addr))
    (format t "  Address is ~x  /  ~d ~%" *address* *address*)))

(defun parse-asm-file ()
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
