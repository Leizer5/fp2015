#lang racket
(require "tree.rkt")

; Inserts x in the tree, returning a new BST with the proper structure
( define ( bst-insert x tree )
   ( cond
      [ ( empty-tree? tree ) ( make-leaf x ) ]
      [ ( > x ( root tree ) ) ( make-tree ( root tree ) ( left tree ) ( bst-insert x ( right tree ) ) ) ]
      [ else ( make-tree ( root tree ) ( bst-insert x ( left tree ) ) ( right tree ) ) ] ) )


; Checks if x is an element of tree
( define ( bst-element? x tree )
   ( cond
      [ ( empty-tree? tree ) #f ]
      [ ( = x ( root tree ) ) #t ]
      [ ( > x ( root tree ) ) ( bst-element? x ( right tree ) ) ]
      [ else ( bst-element? x ( left tree ) ) ] ) )


; Traverse the tree in a such way that the list should contain sorted elements
( define ( bst->list tree )
    ( if ( empty-tree? tree )
         ( list ) 
      ( append ( bst->list ( left tree ) ) ( cons ( root tree ) ( bst->list ( right tree ) ) ) ) ) )


; Checks if the given binary tree is a binary search tree
( define ( bst? tree )
   ( define ( iter? tree min max )
      ( cond
         [ ( empty-tree? tree ) #t ]
         [ ( or ( < ( root tree ) min ) ( > ( root tree ) max ) ) #f ]
         [ else ( and ( iter? ( left tree ) min ( root tree ) ) ( iter? ( right tree ) ( root tree ) max ) ) ] ) )
   ( iter? tree ( - ( root tree ) 100 ) ( + ( root tree ) 100 ) ) )
      
