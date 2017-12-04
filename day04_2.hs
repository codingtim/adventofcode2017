import Data.List
valid :: [[String]] -> Int
valid xs = foldl (+) 0 (map validP xs)

validP :: [String] -> Int
validP (x:[]) = 1
validP (x:xs) | notContainP x xs = validP xs
              | otherwise = 0

notContainP :: String -> [String] -> Bool
notContainP _ [] = True
notContainP a (x:xs) | notContain a (permutations x) = notContainP a xs
                     | otherwise = False

notContain :: String -> [String] -> Bool
notContain _ [] = True
notContain a (x:xs) | a == x = False
                    | otherwise = notContain a xs
