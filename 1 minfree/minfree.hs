import Data.Array

--O(n^2)
minfree :: [Int] -> Int
minfree xs = let truthList = map (\x -> if (x `elem` [0 .. length xs]) then True else False) xs
             in length $ takeWhile id truthList

--O(n)
fastMinFree :: [Int] -> Int
fastMinFree xs = length $ takeWhile id $ elems $ 
                 accumArray (||) False (0, n)
                            (zip (filter (<= n) xs) (repeat True))
                    where n = length xs