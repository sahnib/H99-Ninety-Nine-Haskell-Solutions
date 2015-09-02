module Prob1To10
(
  lastElem,
  lastButOne,
  elemAt,
  totalElem,
  reverseList,
  isAPalindrome,
  NestedList,
  flatten,
  eliminate,
  pack,
  encode
) where

-- Problem 1: Find the last element of a list.

lastElem :: [a] -> a
lastElem [] = error "empty list"
lastElem [x] = x
lastElem (_:xs) = last xs

-- Problem 2: Find the last but one element of a list.

lastButOne :: [a] -> a
lastButOne [] = error "empty list"
lastButOne [_] = error "list too short"
lastButOne (x:[_]) = x
lastButOne (_:xs) = lastButOne xs


-- Problem 3: Find the K'th element of a list. The first element in the list is number 1.

elemAt :: Int -> [a] -> a
elemAt _ [] = error "list too short"
elemAt 1 (x:_) = x
elemAt n (_:xs) = elemAt (n-1) xs

-- Problem 4: Find the number of elements of a list.

totalElem :: [a] -> Int
totalElem = foldr (\x -> (+)1) 0

-- Problem 5: Reverse a list.

reverseList :: [a] -> [a] 
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

isAPalindrome :: Eq a => [a] -> Bool
isAPalindrome [] = True
isAPalindrome [x] = True
isAPalindrome ([x,y]) = x == y
isAPalindrome (x:xs) =
  (x == last xs) && isAPalindrome (init xs)

-- Problem 7: Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
flatten (List x) = foldr ((++).flatten) [] x

-- Problem 8: Eliminate consecutive duplicates of list elements.

eliminate :: Eq a => [a] -> [a]
eliminate [] = []
eliminate [x] = [x]
eliminate (x:y:xs) =
  if x == y
  then eliminate (x:xs)
  else x:eliminate (y:xs)

-- Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

packHelper :: Eq a => a -> [[a]] -> [[a]]
packHelper a [] = [[a]]
packHelper a (x:xs) =
  if a == head x
  then (a:x):xs
  else [a]:x:xs

pack :: Eq a => [a] -> [[a]]
pack = foldr packHelper []

-- Problem 10: Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

encode :: Eq a => [a] -> [(Int, a)]
encode = map (\xs -> (length xs, head xs)).pack
