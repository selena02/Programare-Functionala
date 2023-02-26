import Distribution.SPDX (LicenseId(AFL_1_2))
import Distribution.PackageDescription (emptyFlag)

data Punct = Pt [Int]

instance Show Punct where
    show (Pt l) = "(" ++ parse l ++")" where
        parse [] = " "
        parse [x] = show x
        parse( x:xs) = show x ++ ", " ++ parse xs

data Arb = Vid | F Int | N Arb Arb deriving Show

class ToFromArb a where
    toArb :: a -> Arb
    fromArb :: Arb -> a        

instance ToFromArb Punct where
    toArb :: Punct -> Arb
    toArb (Pt[]) = Vid
    toArb (Pt(x:xs)) = N(F(x)) (toArb (Pt(xs)))
    fromArb ::Arb -> Punct
    fromArb Vid = (Pt[])
    fromArb (F a) = (Pt [a])
    fromArb (N a b) = let
                         Pt l1 = fromArb a
                         Pt l2 = fromArb b
                        in Pt(l1++l2)

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

instance GeoOps Geo where
    perimeter (Square l) = 4*l
    perimeter (Rectangle l1 l2) = 2*l1 + 2*l2
    perimeter (Circle r)= 2*pi*r 
    area (Square l) = l*l
    area (Rectangle l1 l2) = l1*l2
    area (Circle r) = pi*r*r 

instance (Eq x, Floating x) => Eq(Geo x) where 
    a==b = perimeter a == perimeter b   




