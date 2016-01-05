#lang racket
(require "tree.rkt")
;The first function takes a level, a binary tree and returns a list of all emenents that are located on that same level of the tree.
;Start counting levels from 1.
( define ( tree-level level tree )
  ( cond
     [ ( empty-tree? tree ) ( list ) ]
     [ ( = level 1 ) ( list ( root tree ) ) ]
     [ else ( append ( tree-level ( - level 1 ) ( left tree ) ) ( tree-level ( - level 1 ) ( right tree ) ) ) ] ) )
;The second function takes a binary tree and returns a list of lists of all elemenets on every level of the tree.
;The ith element of the returned list should be the elements on the ith level of the tree
( define ( tree-levels tree )
   ( define ( iter tree result height )
      ( cond
         [ ( = height 0 ) result ]
         [ else ( iter tree ( cons ( tree-level height tree ) result ) ( - height 1 ) ) ] ) )
   ( iter tree ( list ) ( height tree ) ) )
