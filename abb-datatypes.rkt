#lang eopl
(define-datatype bintree bintree?
  (leaf-node (datum number?))
  (interior-node (key symbol?)
                 (left bintree?)
                 (right bintree?)
                 )
  )

(define binTree '(8 (3 (1 () ()) (6 (4 () ()) (7 () ()))) (10 () (14 (13 () ()) ()))))
