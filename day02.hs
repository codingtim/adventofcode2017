checksum :: [[Int]] -> Int
checksum xs = checksumAcc xs 0

checksumAcc ::[[Int]] -> Int -> Int
checksumAcc [] a = a
checksumAcc (x:xs) a = checksumAcc xs ((maxA x (head x)) - (minA x (head x)) + a)

maxA :: [Int] -> Int -> Int
maxA [] m = m
maxA (x:xs) m | x > m = maxA xs x
              | otherwise = maxA xs m

minA :: [Int] -> Int -> Int 
minA [] m = m 
minA (x:xs) m | x < m = minA xs x
              | otherwise = minA xs m
