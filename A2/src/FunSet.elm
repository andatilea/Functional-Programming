-----------------------
-- Anda-Corina Tilea
-- 28.10.2020
-----------------------
module FunSet exposing (..)
import List exposing (..)

type alias FunSet = Int -> Bool

contains : FunSet -> Int -> Bool
contains set elem = set elem

singletonSet : Int -> FunSet
singletonSet elem = \inputElem -> elem == inputElem

{-
Conveniece function to create a set of elements.
```elm
setOf [1, 2, 3] == union (union (singletonSet 1) (singletonSet 2)) (singletonSet 3))
setOf [1, 2, 3] == fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]
```
-}

--2.2.6

setOf : List Int -> FunSet
setOf elems = \x -> List.any (\y -> y == x) elems
{-
Returns the union of 2 sets.
```elm
(union (singletonSet 1) (singletonSet 2)) 1 == True
(union (singletonSet 1) (singletonSet 2)) 2 == True
(union (setOf [1, 3, 4]) (setOf [1, 2])) 2 == True
(union (setOf [1, 3, 4]) (setOf [1, 2])) 5 == False
```
-}
union : FunSet -> FunSet -> FunSet
union a b = \inputElem -> (contains a inputElem) || (contains b inputElem)      -- is enough for the element to be contained only by one of the sets;

{-
Returns the intersection of 2 sets.
```elm
(intersect (setOf [1, 2]) (setOf [1, 3])) 1 == True
(intersect (setOf [1, 2]) (setOf [1, 3])) 2 == False
```
-}
intersect : FunSet -> FunSet -> FunSet
intersect a b = \inputElem -> (contains a inputElem) && (contains b inputElem)      -- the element must be contained by both sets;

{-
Returns the difference of 2 sets.
```elm
(diff (setOf [1, 2]) (setOf [1, 3])) 1 == False
(diff (setOf [1, 2]) (setOf [1, 3])) 2 == True
```
-}
diff : FunSet -> FunSet -> FunSet
diff a b = \inputElem -> ((contains a inputElem) && (not (contains b inputElem))) ||
                         ((not (contains a inputElem)) && (contains b inputElem))    -- the element must be contained by one set and it must not be contained by the other;

{-
Returns a new set, with `function` applied to each of element.
```elm
(map (\x -> x + 1) (setOf [1, 2])) 1 == False
(map (\x -> x + 1) (setOf [1, 2])) 2 == True
(map (\x -> x + 1) (setOf [1, 2])) 3 == True
```
-}
-- helper function to check if a bounded element, which satisfies the given function, exists;
check : FunSet -> (Int -> Bool) -> Bool
check s f =
    let
         iteration a =
          if (a < -1000)                        -- the bound was taken for [-1000,1000];
          then False
          else if (contains s a) && f(a)
          then True
          else iteration (a - 1)
    in
        iteration (1000)

-- apply function to each element of the set;
map: ( Int -> Int ) -> FunSet ->  FunSet
map function set = \inputElem -> (check set (\x -> inputElem == function(x)))

{-
Takes a list of sets and returns a new set, which is build by applying a fold using `operation` function.
```elm
(fold union [(singletonSet 1), (singletonSet 2), (singletonSet 3)]) 1 == True
(fold intersect [setOf [1], setOf [2]]) 1 == False
(fold intersect [setOf [1], setOf [2]]) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 1 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 2 == False
(fold diff [setOf [1, 2, 3], setOf [1], setOf [2]] ) 3 == True
```
-}
fold: ( FunSet -> FunSet -> FunSet ) -> List FunSet -> FunSet
fold operation sets =
           let
                       foldLeft op1 s1 =                                                 -- create a helper function to represent folding from the left;
                          case s1 of
                          x :: xs -> foldl op1  x xs
                          others -> (\_ -> False)

                       foldRight : (FunSet -> FunSet -> FunSet) -> List FunSet -> FunSet    -- create a helper function to represent folding from the right;
                       foldRight op2 s2 =
                           case s2 of
                           x :: xs -> foldr op2 x xs
                           others -> (\_ -> False)

                       combined op3 s3 =                                                 -- since the two types of folding usually return different results, we will check them both;
                           intersect  (foldRight op3 s3) (foldLeft op3 s3)               -- the intersection of their results will give the correct answer;
           in
                  combined operation sets                                                   -- apply the third helper on the problem's data;


