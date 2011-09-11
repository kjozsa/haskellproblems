{- euler 1 -}
e1 = foldl (+) 0 [x | x <- [0..999], x `mod` 3 == 0 || x `mod` 5 == 0]

{- euler 2 -}
fibo :: (Integral a) => a -> a -> [a]
fibo a b = [a] ++ fibo b (a+b) 
e2 = sum $ takeWhile (< 4000000) $ filter even $ fibo 1 1

{- euler 3 -}
primes :: (Integral a) => a -> a -> [a]
primes num d 
  | num `mod` d == 0 = [d] ++ (primes rest d) 
  | d > num-1        = []
  | otherwise          = primes num (d+1)
  where rest = num `div` d

e3 = last $ primes 600851475143 2

{- euler 4 -}
e4 = maximum $ filter (\x -> show x == reverse(show(x))) [x*y | x <- [1..999], y <- [1..999]]

{- euler 48 -}
lots :: [Char]
lots = reverse $ take 10 $ reverse $ show $ sum $ map (\x -> x^x) [1..1000]

{- euler 5 -}
good :: Int -> Bool
good num = foldl1 (&&) $ map (\x -> num `mod` x == 0) [1..20]
e5 = take 1 [x | x <- [13*17, 2*13*17 ..], good x]

{- euler 7 -}
prime :: Int -> Bool
prime num = foldl1 (&&) $ map (\x -> mod num x /= 0) [2..(round $ sqrt $ fromIntegral num)]
prime' num = all (\x -> mod num x /= 0) $ takeWhile (\n -> n^2 <= num) (2:[3,5..])
e7 = last $ take 10001 $ filter prime' (2:[3,5..])


