nattoint :: Int -> Int
nattoint n
    | mod n 2 == 0 = 1 * (n `div` 2)
    | otherwise = -(n+1) `div` 2

nattoint' :: Int -> Int
nattoint' n = flip n * (n + oddoreven n) `div` 2
			  where flip x = truncate $ (-1)**(fromIntegral x);
			  		oddoreven x =  (`div` 2) $ (+1) $ flip(x-1)

check :: Int-> Bool
check n = map nattoint [0..n] == map nattoint [0..n]