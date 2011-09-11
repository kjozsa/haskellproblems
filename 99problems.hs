{- problem 1 -}
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs


{- problem 2 -}
lastBut' :: [a] -> a
lastBut' [x,_] = x
lastBut' (x:xs) = lastBut' xs

lastBut'' = last . init


{- problem 3 -}
elementAt :: (Ord a) => [a] -> Int -> a
elementAt xs 1 = head xs
elementAt (_:xs) x = elementAt xs (x - 1)


{- problem 4 -}
length' :: [a] -> Int
length' xs = foldl (\acc _ -> acc + 1) 0 xs


{- problem 5 -} 
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []


{- problem 6 -}
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == (reverse xs)


{- problem 8 -}
compress :: Eq a => [a] -> [a]
compress xs = foldr (\acc x -> if acc == (head x) then x else acc:x) [last xs] xs


