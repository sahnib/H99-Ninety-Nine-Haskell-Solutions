import Prob1To10 (pack)
-- Problem 11: Modified run-length encoding.

-- Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

data Encoded a = Single a | Multiple Int a deriving (Show)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map (\xs -> if length xs == 1 then Single (head xs) else Multiple (length xs) (head xs)).pack

-- Problem 12: Decode a run-length encoded list.

-- Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

decodeOne :: Encoded a -> [a]
decodeOne (Single a) = [a]
decodeOne (Multiple n a) = replicate n a

decodeModified :: [Encoded a] -> [a]
decodeModified = concatMap decodeOne

-- Problem 13: Run-length encoding of a list (direct solution).

-- Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

encodeDirectOne :: Eq a => a -> [Encoded a] -> [Encoded a]
encodeDirectOne x [] = [Single x]
encodeDirectOne x l@(Single y:xs) =
  if x == y
  then Multiple 2 y:xs
  else Single x:l
encodeDirectOne x l@(Multiple n y:xs) =
  if x == y
  then Multiple (n+1) y:xs
  else Single x:l

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect = foldr encodeDirectOne []

-- Problem 14: Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x , x]
dupli (x:xs) = dupli [x] ++ dupli xs

-- Problem 15: Replicate the elements of a list a given number of times.

repli :: [a] -> Int -> [a]
repli _ 0 = []
repli [x] 1 = [x]
repli [x] n
  | n < 0 = []
  | otherwise = x:repli [x] (n-1)
repli (x:xs) n
  | n < 0 = []
  | otherwise = repli [x] n ++ repli xs n

-- An alternative
repliAlternative :: [a] -> Int -> [a]
repliAlternative x n = concatMap (replicate n) x

-- Problem 16: Drop every N'th element from a list.
dropEveryHelper :: [a] -> Int -> Int -> [a]
dropEveryHelper [] _ _ = []
dropEveryHelper l@(x:xs) m n
  | n < 0 = l
  | otherwise =
    if m == n
    then dropEveryHelper xs 1 n
    else x:dropEveryHelper xs (m+1) n

dropEvery :: [a] -> Int -> [a]
dropEvery x = dropEveryHelper x 1

-- Problem 17: Split a list into two parts; the length of the first part is given.

split :: [a] -> Int -> [[a]]
split [] _ = []
split l@(x:xs) n
  | n < 0 = [[], l]
  | otherwise =
    let y = head ys
        z = tail ys
    in (x:y):z
  where
    ys = split xs (n-1)
    
-- Problem 18: Extract a slice from a list.

-- Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.

