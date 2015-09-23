module Lista5 where

import Data.Monoid
-- Andrei Martins Silva
-- Carlos Augusto Silva

data Coisa a = Nada 
               | UmaCoisa a
               | DuasCoisas a a 
               | TresCoisas a a a 
               deriving Show


instance (Monoid a) => Monoid (Coisa a) where
        mempty = Nada
        mappend (UmaCoisa x) ( DuasCoisas y z ) = TresCoisas x y z
        mappend (DuasCoisas x y) (UmaCoisa z) = TresCoisas x y z
        mappend (DuasCoisas k l) (DuasCoisas m n) =  DuasCoisas (k <> m) (l <> n)
        mappend (TresCoisas p q r) (TresCoisas s t u) = TresCoisas (p <> s) (q <> t) (r <> u)
        mappend (UmaCoisa a) (UmaCoisa b) = DuasCoisas a b
        mappend (UmaCoisa a) (TresCoisas b c d) = TresCoisas (a <> b) c d
        mappend (TresCoisas a b c) (UmaCoisa d) = TresCoisas (a <> d) b c
        mappend (DuasCoisas a b) (TresCoisas c d e) = TresCoisas (a <> c) (b <> d) e
        mappend (TresCoisas a b c) (DuasCoisas d e) = TresCoisas (a <> d) (b <> e) c
        mappend (UmaCoisa a) Nada = UmaCoisa a
        mappend (DuasCoisas a b) Nada = DuasCoisas a b
        mappend (TresCoisas a b c) Nada = TresCoisas a b c
        mappend Nada (UmaCoisa a) = UmaCoisa a
        mappend Nada (DuasCoisas a b) = DuasCoisas a b
        mappend Nada (TresCoisas a b c) = TresCoisas a b c
        mappend _ _ = Nada

instance Monoid Int where
        mempty = 0
        mappend = (+)


-- Exerc�cio 2
{-
let t1 = mappend (TresCoisas 2 3 4) (TresCoisas 5 6 7) :: Coisa Int
t1
OUTPUT> TresCoisas 7 9 11

let t2 = mappend (UmaCoisa 1) (DuasCoisas 1 2) :: Coisa Int
t2
OUTPUT> TresCoisas 1 1 2

let t3 = mappend (UmaCoisa 1) (TresCoisas 1 2 3) :: Coisa Int
t3
OUTPUT> TresCoisas 2 2 3

let t4 = mappend (UmaCoisa 2) (UmaCoisa 9) :: Coisa Int
t4
OUTPUT> DuasCoisas 2 9

let t5 = mappend (DuasCoisas 3 5) (DuasCoisas 4 9) :: Coisa Int
t5
OUTPUT> DuasCoisas 7 14

let t6 = mappend (UmaCoisa "Maria ") (DuasCoisas "Do " "Bairro") :: Coisa String
t6
OUTPUT> TresCoisas "Maria " "Do " "Bairro"

let t7 = mappend (DuasCoisas "Maria " "Bairro") (UmaCoisa "Do ") :: Coisa String
t7
OUTPUT> TresCoisas "Maria " "Bairro" "Do "

let t8 = mappend (TresCoisas "Fafa " "Fefe " "Fifi ") (TresCoisas "Dada " "Dede " "Didi ") :: Coisa String
t8
OUTPUT> TresCoisas "Fafa Dada " "Fefe Dede " "Fifi Didi "

let t9 = mappend (DuasCoisas  3 9) (TresCoisas 9 12 56) :: Coisa Int
t9
OUTPUT> TresCoisas 12 21 56

let t10 = mappend (TresCoisas 5 9 3) (DuasCoisas 12 45) :: Coisa Int
t10
OUTPUT> TresCoisas 17 54 3

let t11 = mappend (TresCoisas 4 76 1) (UmaCoisa 14) :: Coisa Int
t11
OUTPUT> TresCoisas 18 76 1

let t12 = mappend (UmaCoisa "Maria ") (UmaCoisa "Callas ") :: Coisa String
t12
OUTPUT> DuasCoisas "Maria " "Callas "

let t13 = mappend (UmaCoisa 7) Nada :: Coisa Int
t13
OUTPUT> UmaCoisa 7
 
let t14 = mappend (DuasCoisas "Joao " "Maria ") Nada :: Coisa String
t14
OUTPUT> DuasCoisas "Joao " "Maria "

let t15 = mappend Nada (TresCoisas "G" "H" "C") :: Coisa String
t15
OUTPUT> TresCoisas "G" "H" "C" 
-}

-- Exerc�cio 3
combinarTudo :: (Monoid a) => [Coisa a] -> Coisa a
combinarTudo [Nada] = Nada
combinarTudo xs = foldl mappend Nada xs






