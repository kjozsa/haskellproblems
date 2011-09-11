factors :: (Integral a) => a -> a
factors num = foldl (\acc x -> if num `mod` x == 0 then acc+1 else acc) 0 [1..num]

e11 = head $ filter (\z -> factors z > 500) $ map (foldl (+) 0) $ map (\x -> [1..x]) [1..]

e11' = head $ filter (fact500) $ triangles
     where triangles = map (foldl (+) 0) $ map (\x -> [1..x]) [1..]
           fact500 z = factors z > 500
           factors num = foldl (\acc x -> if num `mod` x == 0 then acc+1 else acc) 0 [1..num]


