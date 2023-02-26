factori::Int->[Int]
factori n= [x | x <- [1..n], mod n x == 0]

prim :: Int -> Bool
prim n = length(factori(n)) == 2

numerePrime::Int-> [Int]
numerePrime n = [x | x <- [2..n], prim x]

myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 ls_1 ls_2 ls_3 =[(a,b,c)|((a,b),c) <- zip (zip ls_1 ls_2)ls_3]

firstEl :: [(a,b)] -> [a]
firstEl ls = map fst ls

sumList :: [[Int]] ->[Int]
sumList ls = map sum ls

prel2 :: [Int] -> [Int]
prel2 ls = [if a`mod`2 ==0 then a `div` 2 else a*2| a <- ls]

paritate :: Int -> Int
paritate x 
    |even x = x `div` 2
    |otherwise = x + x 

functie :: Char -> [String] -> [String]
functie  litera  = filter (litera `elem`) 

func9 :: [Int] -> [Int]
func9 ls = map (\x->x*x)(filter odd ls)

pozitiiImpare:: [Int] -> [Int]
pozitiiImpare ls = map(\x -> x*x) [i |(i,j)<- filter(\(i,j)->odd j)(zip ls [0..])]

numaiVocale ::[String]->[String]
numaiVocale = map (filter(`elem`"aeiouAEIOU"))

myMap :: ( a -> b) -> [a]->[b]
myMap _  []=[]
myMap f(x:xs)=f x :myMap f xs

myFilter::(a->Bool)->[a]->[a]
myFilter _[]=[]
myFilter f (x:xs) 
        |f x = x : myFilter f xs
        |otherwise = myFilter f xs