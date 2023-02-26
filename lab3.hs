-- nrVocale :: [String] -> Int
-- nrVocale [] = 0
-- nrVocale (h:t)
--     |elem h ['a','e','i','o'] = t'+ 1
--     |otherwise = t'
--     where t' = nrVocale t  

import Data.List
import Data.Char 

nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t)
    | h `elem`  ["a","e","i","o"] = t'+ 1
    |otherwise = t'
    where t' = nrVocale t 

countVowels :: String -> Int
countVowels string = length [x | x <- string, x `elem` "aeiouAEIOU"]    

f :: Int -> [Int] -> [Int]
f _ [] = []
f a (x:xs) 
    |even x = x : a : t'
    |otherwise = x:t'
    where t' = f a xs

divizori :: Int -> [Int]
divizori x = [i | i <- [1..x], x`mod`i==0] 

listadiv :: [Int] -> [[Int]]
listadiv l = [ divizori s | s<-l]

inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (h:t)
     |h>=a && h<=b = h : t'
     |otherwise = t'
     where t' = inIntervalRec a b t


inInterval :: Integer -> Integer -> [Integer] -> [Integer]
inInterval x y v = [i | i<-v , i>=x && i<=y]

pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    | h > 0 = 1 + t'
    | otherwise = t'
    where t' = pozitiveRec t

pozitiveComp :: [Int] -> Int
pozitiveComp l = length[x | x <- l, x>0]

pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp v = [y| (x,y) <-zip v[0..], odd x]

pozitiiImpare0 :: [Int] -> Int -> [Int]
pozitiiImpare0 [] _ = []
pozitiiImpare0 (h:t) x
    | odd h = x : pozitiiImpare0 t(x+1)
    | otherwise = pozitiiImpare0 t(x+1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec v = pozitiiImpare0 v 0 

multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (h:t)
    | isDigit h = digitToInt h * multDigitsRec t
    |otherwise = multDigitsRec t

multDigitsComp :: String -> Int
multDigitsComp s = product [digitToInt e |e <-s, isDigit e]