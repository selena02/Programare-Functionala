import Distribution.Simple.Setup (falseArg)

data Fruct = Mar String Bool | Portocala String Int

ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia(Mar _ _) = False
ePortocalaDeSicilia (Portocala s b ) = s`elem` [ "Tarocco", "Moro" ,"Sanguinello"];

listaFructe = [Mar "Ionatan" False, Portocala "Sanguinello" 10, Portocala "Valencia" 22, Mar "Golden Delicious" True, Portocala "Sanguinello" 15, Portocala "Moro" 12, Portocala "Tarocco" 3, Portocala "Moro" 12, Portocala "Valencia" 2, Mar "Golden Delicious" False, Mar "Golden" False, Mar "Golden" True]

nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia  ls = sum [b | Portocala s b <- ls, ePortocalaDeSicilia (Portocala s b) ] 
    
nrMereViermi :: [Fruct] -> Int
nrMereViermi ls = sum[1 | Mar a b <- ls, b==True] 

type NumeA = String
type Rasa = String

data Animal = Pisica NumeA | Caine NumeA Rasa deriving Show

vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow"
vorbeste (Caine _ _ ) = "Woof" 

rasa :: Animal -> Maybe String
rasa (Pisica _) = Nothing
rasa (Caine a b) = Just b

data Linie = L [Int] deriving Show
data Matrice = M [Linie] deriving Show

verifica :: Matrice -> Int -> Bool
verifica ( M l ) nr = foldr (&&) True [sum x == nr | L x <- l] 

--doarPozN :: Matrice -> Int -> Bool
--doarPozN (M l) nr = foldr (&&) True [ x > 0 | M l<-x]  

doarPozN :: Matrice -> Int -> Bool
doarPozN  (M linii) n = foldr (&&) True [all (>0) l | L l <- linii, length l == n]

corect :: Matrice -> Bool
corect (M []) = True
corect (M [x]) = True
corect (M (L x:L y:t))
    |length x == length y = corect (M (L y:t))
    |otherwise = False