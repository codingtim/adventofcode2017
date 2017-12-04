checksum :: [[Int]] -> Int
checksum xs = checksumAcc xs 0

checksumAcc :: [[Int]] -> Int -> Int
checksumAcc [] a = a
checksumAcc (x:xs) a = checksumAcc xs (divisible x + a)

divisible :: [Int] -> Int
divisible xs = filterDivisible (pairs xs xs) 

filterDivisible :: [(Int, Int)] -> Int
filterDivisible [] = 0
filterDivisible ((a,b):xs) | a `mod` b == 0 && a /= b = a `div` b
                           | otherwise = filterDivisible xs 

pairs :: [Int] -> [Int] -> [(Int, Int)]
pairs [] _ = []
pairs (x:xs) ys = pair x ys ++ pairs xs ys

pair :: Int -> [Int] -> [(Int, Int)] 
pair x [] = []
pair x (y:ys) = (x,y) : pair x ys
