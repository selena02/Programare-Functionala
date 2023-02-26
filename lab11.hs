{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
     -}
--class Functor f => Applicative f where
  --  pure :: a -> f a
   -- (<*>) :: f (a -> b) -> f a -> f b

--Just length <*> Just "world"

--Just (++" world") <*> Just "hello,"
--pure (+) <*> Just 3 <*> Just 5
--pure (+) <*> Just 3 <*> Nothing
--(++) <$> ["ha","heh"] <*> ["?","!"]

data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

instance Functor List where
    fmap f (Nil) = Nil
    fmap f (Cons a b) = Cons (f a) (fmap f b)

app :: List a -> List a -> List a 
app Nil l2 = l2
app (Cons h t) l2 = Cons h (app t l2)    
    
instance Applicative List where
   pure x = Cons x Nil
   Nil <*> l2 = Nil
   (Cons h t) <*> l2 = fmap h l2 `app` (t <*> l2)


f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Cow = Cow {
name :: String
, age :: Int
, weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty [] = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative nr
    |nr<0 = Nothing
    |nr>=0 = Just nr

cowFromString1 :: String -> Int -> Int -> Maybe Cow
cowFromString1 a b c
    |(noEmpty a == Just a) && (noNegative b == Just b) && (noNegative c == Just c) = Just (Cow a b c)
    |otherwise = Nothing     

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString a b c =  fmap Cow(noEmpty a) <*> noNegative b <*> noNegative c

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address deriving (Eq, Show)

validateLength :: Int -> String -> Maybe String
validateLength a b
    |length b < a = Just b
    |otherwise = Nothing

mkName :: String -> Maybe Name
mkName "" = Nothing
mkName nume = fmap Name (validateLength 26 nume) 

mkAddress :: String -> Maybe Address
mkAddress "" = Nothing 
mkAddress adresa = fmap Address (validateLength 101 adresa)

mkPerson1 :: String -> String -> Maybe Person
mkPerson1 a b 
    |(mkName a == Just (Name a)) && (mkAddress b == Just (Address b)) = Just(Person (Name a) (Address b))
    |otherwise = Nothing 

mkPerson :: String -> String -> Maybe Person
mkPerson a b = fmap Person (mkName a) <*> (mkAddress b) 