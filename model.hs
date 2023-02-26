---------------------------------------------------------------------
data Point = Pt [Int] deriving Show

data Arb = Empty | Node Int Arb Arb deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a

instance ToFromArb Point where
    toArb (Pt []) = Empty 
    toArb (Pt (x:xs)) = Node x (toArb (Pt (filter (< x) xs)))  (toArb (Pt (filter (>= x) xs)))
    -- toArb (Pt []) = Empty
    -- toArb (Pt (x : xs)) = insert x $ toArb (Pt xs) where
    --   insert val Empty = Node val Empty Empty
    --   insert val (Node root left right)
    --     | val <= root = Node root (insert val left) right
    --     | otherwise = Node root left (insert val right)
    fromArb :: Arb -> Point
    fromArb Empty = (Pt[])
    fromArb (Node nod Empty Empty) = (Pt[nod]) 
    fromArb (Node n left right) = let
                         Pt l1 = fromArb left
                         Pt l2 = fromArb right
                        in Pt(l1++l2)

a=toArb (Pt[1,2,3,4,5,6])
----------------------------------------------------------------------------------------
getFromInterval1 :: Int->Int->[Int]->[Int]
getFromInterval1 a b ls = filter (\x -> x>=a && x<=b) ls 

getFromInterval a b list = do 
    x <-list 
    if a <= x && x <= b then return x else []

------------------------------------------------------------------------------------------
newtype ReaderWriter env a = RW {getRW :: env-> (a,String)}

instance Monad (ReaderWriter env) where
  return va = RW (\_ -> (va,""))
  ma >>= k = RW f 
      where f env = let (va, str1) = getRW ma env
                        (vb, str2)  = getRW (k va) env
                    in (vb, str1 ++ str2)

instance Applicative (ReaderWriter env) where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor (ReaderWriter env) where              
  fmap f ma = pure f <*> ma  

 

 

