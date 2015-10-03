module Lista3 where

{-
Exercício 3.1 Faça uma função que retorne a média de um [Double] usando
foldl.
-}

mean :: [Double] -> Double
mean ds = foldl (+) 0 ds / fromIntegral (length ds)

{-
Exercício 3.2 Faça uma função que retorne o desvio padrão de um [Double]
usando foldl. O desvio padrão de um vetor é dado por σ =
é a média deste vetor.
n
(x −x) 2
i=1 i
n−1
, onde x
-}

-- Exercício 3.3 Refaça o exercício 1.15 usando map.
{-
Exercício 3.4 Faça uma função que receba um [String] e retorne todos ele-
mentos palíndromos. Ver exercício 1.9.
-}
-- Exercício 3.5 Refaça o exercício 2.8 usando map.

{-
Exercício 3.6 Usando o exercício 2.7 como base, faça uma função que retorna
todas as moedas com o campo val valendo Euro.
-}

