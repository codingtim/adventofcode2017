valid :: [[String]] -> Int
valid xs = foldl (+) 0 (map validP xs)

validP :: [String] -> Int
validP (x:[]) = 1
validP (x:xs) | notContain x xs = validP xs
              | otherwise = 0

notContain :: String -> [String] -> Bool
notContain _ [] = True
notContain a (x:xs) | a == x = False
                    | otherwise = notContain a xs
