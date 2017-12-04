captcha :: String -> Int
captcha xs = captchaInt(map (read . (:"")) xs :: [Int])

captchaInt :: [Int] -> Int
captchaInt xs = captchaAcc xs (head xs) 0

captchaAcc :: [Int] -> Int -> Int -> Int
captchaAcc [] _ a = a
captchaAcc (x:[]) h a | x == h = x + a
                      | otherwise = a
captchaAcc (x:y:xs) h a | x == y = captchaAcc (y:xs) h (a+x)
                        | otherwise = captchaAcc (y:xs) h a
