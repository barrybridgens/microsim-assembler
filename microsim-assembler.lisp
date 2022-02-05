;;;; microsim-assembler.lisp

(in-package #:microsim-assembler)


(defparameter *valid-op-codes* '("LDA_I" "JMP"))

(defparameter *line-cnt* 0)
(defparameter *address* #x100)


(defun initialise ())

(defun process-label (s)
  (format t "~a -> LABEL~%" s))

(defun process-op-code (s)
  (format t "~a -> OP-CODE~%" s))

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
		((str:ends-with-p ":" line)
		 (process-label line))
		((if (member (car (str:words line)) *valid-op-codes* :test #'string=) t nil)
		 (process-op-code line))
		(t
		 (format t "-> ERROR~%")))
	      ))
    (close in))))
