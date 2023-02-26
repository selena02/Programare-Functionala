sumaImpare :: [Int]-> Int
sumaImpare = foldr (+) 0 . map(^2) . filter odd 

ex2 :: [Bool] -> Bool
ex2  = foldr (&&) True 

allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies f xs = foldr (&&) True (map f xs)
-- foldr (\ir->fi&&r) True l

anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies f l = foldr ( || ) False (map f l)

mapFoldr :: (a->b) -> [a] -> [b]
mapFoldr f [] = []
mapfoldr f y = foldr(\x xs -> (f x):xs) [ ] y

mapFilter :: (a->Bool)->[a]->[a]
mapFilter f = foldr(\ x xs -> if f x then x:xs else xs)[] 

listToInt :: [Integer] -> Integer
listToInt  = foldl (\i r -> 10 * i + r) 0 

rmChar :: Char -> String -> String
rmChar c s = filter ( /= c ) s 

rmCharRec :: String -> String -> String
rmCharRec "" s = s
rmCharRec (h:t) s = rmCharRec t (rmChar h s) 

rmCharsFold :: String -> String -> String
rmCharsFold l s = foldr rmChar s l





