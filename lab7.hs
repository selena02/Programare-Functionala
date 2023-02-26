import Distribution.SPDX (LicenseId(AFL_1_2))
import Distribution.PackageDescription (emptyFlag)

data Expr = Const Int | Expr :+: Expr | Expr :*: Expr deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int | Node Operation Tree Tree deriving (Eq, Show)

instance Show Expr where
    show::Expr->String
    show (Const i) = show i
    show (a :+: b) = "(" ++ show a ++ "+" ++ show b ++ ")"
    show (a :*: b) = "(" ++ show a ++ "*" ++ show b ++ ")"

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)

evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (a :+: b) = evalExp a + evalExp b
evalExp (a :*: b) = evalExp a * evalExp b 


evalArb :: Tree -> Int
evalArb (Lf a)=a
evalArb (Node Add a b) = evalArb a + evalArb b
evalArb (Node Mult a b) = evalArb a * evalArb b

arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))

exp2 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))

expToArb :: Expr -> Tree
expToArb (Const x) = (Lf x)
expToArb (a :+: b) = Node Add (expToArb a) (expToArb b)
expToArb (a :*: b) = Node Mult (expToArb a)  (expToArb b)

class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert:: Ord key=> key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  keys collection = [fst x | x<- toList collection]
  values :: c key value -> [value]
  values collection =[snd x| x<- toList collection]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
  fromList ((k,v):t) = insert k v (fromList t)

newtype PairList k v = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
    empty = PairList[]
    singleton k v = PairList[(k,v)]
    delete k (PairList l) = PairList $ filter (\(k1,v1)-> k1/=k) l   
    insert k v (PairList l)= PairList $ (k,v) : filter (\(k1,v1)-> k1/=k) l  
    clookup k= lookup k .getPairList
    toList=getPairList

data SearchTree key value
  = Empty | BNode
      (SearchTree key value) -- elemente cu cheia mai mica
      key                    -- cheia elementului
      (Maybe value)          -- valoarea elementului
      (SearchTree key value) -- elemente cu cheia mai mare    

instance Collection SearchTree where
    empty = Empty
    singleton k v = BNode Empty k (Just v) Empty
     insert k v (BNode t1 k1 v1 t2) 
        |k == k1 = BNode t1 k1 (Just v) t2
        |k<k1 = BNode (insert k v t1) k1 v1 t2
        |otherwise = BNode t1 k1 v1 (insert k v t2)