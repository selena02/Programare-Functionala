{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
newtype Identity a = Identity a

instance Functor Identity where
    fmap::(a->b)->Identity a->Identity b
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a 

instance Functor Pair where
    fmap f (Pair a1 a2) = Pair (f a1) (f a2) 

data Constant a b = Constant b

instance Functor (Constant a) where
    fmap f (Constant b) = Constant (f b) 

data Two a b = Two a b
    
instance Functor (Two a )   where
    fmap f (Two a y) = Two a (f y)

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)
    
data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d) 

data Four'' a b = Four'' a a a b
instance Functor (Four'' a) where
    fmap f (Four'' a b c d) = Four'' a b c (f d)

data Quant a b = Finance | Desk a | Bloor b
instance Functor (Quant a) where
    fmap f (Finance) = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f1 (LiftItOut fa) = LiftItOut (fmap f1 fa)

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f,Functor g) => Functor (Parappa f g) where
     fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance (Functor f,Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

data Notorious g o a t = Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
    fmap g(Notorious go ga gt) = Notorious go ga (fmap g gt)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor GoatLord where
    fmap f (NoGoat) = NoGoat
    fmap f (OneGoat a) = OneGoat(f a)
    fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor TalkToMe where
    fmap f (Halt)= Halt
    fmap f (Print s a) = Print s (f a)
    fmap f (Read g) = Read (fmap f g)

