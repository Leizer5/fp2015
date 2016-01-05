#lang racket
(provide make-tree make-leaf empty-tree? root left right t bst bst-fake height)

( define ( make-tree n l r )
   ( list n l r ) )

( define ( make-leaf x )
   ( make-tree x '() '() ) )

( define ( empty-tree? tree )
   ( empty? tree ) )

( define ( root tree )
   ( first tree ) )

( define ( left tree )
   ( first ( rest tree ) ) )

( define ( right tree )
   ( first ( rest ( rest tree ) ) ) )

( define t ; a tree
  ( make-tree 1
    ( make-tree 2
      ( make-leaf 5)
       ( make-leaf 6 ) )
    (make-leaf 3 ) ) )

( define bst ; binary-sorted tree
   ( make-tree 8
      ( make-tree 3
         ( make-leaf 1 )
         ( make-tree 6
            ( make-leaf 4 )
            ( make-leaf 7 ) ) )
      ( make-tree 10
         ( list )
         ( make-tree 14
            ( make-leaf 13 )
            ( list ) ) ) ) )

( define bst-fake ; fake binary-sorted tree
   ( make-tree 20
      ( make-leaf 10 )
      ( make-tree 30
         ( make-leaf 5 )
         ( make-leaf 40 ) ) ) )

( define ( height tree )
   ( cond
      [ ( empty-tree? tree ) 0 ]
      [ else ( + 1 ( max ( height ( left tree ) ) ( height ( right tree ) ) ) ) ] ) )
