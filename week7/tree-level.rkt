#lang racket
(require "tree.rkt")

( define ( tree-level level tree )
  ( cond
     [ ( empty-tree? tree ) ( list ) ]
     [ ( = level 1 ) ( list ( root tree ) ) ]
     [ else ( append ( tree-level ( - level 1 ) ( left tree ) ) ( tree-level ( - level 1 ) ( right tree ) ) ) ] ) )

( define ( tree-levels tree )
   ( define ( iter tree result height )
      ( cond
         [ ( = height 0 ) result ]
         [ else ( iter tree ( cons ( tree-level height tree ) result ) ( - height 1 ) ) ] ) )
   ( iter tree ( list ) ( height tree ) ) )
