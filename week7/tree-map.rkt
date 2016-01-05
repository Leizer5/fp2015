#lang racket
(require "tree.rkt")

( define ( tree-map f tree )
   ( cond
      [ ( empty-tree? tree ) ( list ) ]
      [ else ( make-tree ( f ( root tree ) ) ( tree-map f ( left tree ) ) ( tree-map f ( right tree ) ) ) ] ) )
