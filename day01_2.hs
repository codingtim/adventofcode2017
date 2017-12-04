captcha :: String -> Int
captcha xs = captchaInt(map (read . (:"")) xs :: [Int])

captchaInt :: [Int] -> Int
captchaInt xs = captchaAcc xs xs ((length xs) `div` 2) 0

captchaAcc :: [Int] -> [Int] -> Int -> Int -> Int
captchaAcc [] _ _ a = a
captchaAcc (x:xs) ys s a | x == (jump s (x:xs) ys) = captchaAcc xs ys s (a+x)
                         | otherwise = captchaAcc xs ys s a

jump :: Int -> [Int] -> [Int] -> Int
jump 0 (x:xs) _ = x
jump 0 [] (y:ys) = y
jump a [] ys = jump a ys ys
jump a (x:xs) ys = jump (a-1) xs ys
