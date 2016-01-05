#lang racket
(require "tree.rkt")
;The function takes a single argument funciton f and binarytreeand returns a new tree, where every node is transformed by f`
( define ( tree-map f tree )
   ( cond
      [ ( empty-tree? tree ) ( list ) ]
      [ else ( make-tree ( f ( root tree ) ) ( tree-map f ( left tree ) ) ( tree-map f ( right tree ) ) ) ] ) )
