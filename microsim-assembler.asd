;;;; microsim-assembler.asd

(asdf:defsystem #:microsim-assembler
  :description "An assembler program for my microsim toy microprocesor simulator"
  :author "Barry Bridgens <barry@bridgens.me.uk>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:str)
  :components ((:file "package")
               (:file "microsim-assembler")))
