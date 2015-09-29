module Lista2 where

-- Andrei Martins Silva - 1210045-7 Noturno

{-
Exercício 2.1 Faça um novo tipo chamado Metros, que possui um value cons-
tructor de mesmo nome cujos parâmetros são um Int que representa a dimensão
e um Double que representa o valor da medida. 
Implemente as funções
1. areaQuadrado :: Metros → Metros: Calcula a área de um quadrado
2. areaRet :: Metros → Metros → Metros: Calcula a área de um retangulo
3. areaCubo :: Metros → Metros: Calcula a área de um cubo
Exemplo: areaQuadrado(Metros 1 2.0) = Metros 2 4.0
Use o pattern matching para ignorar as metragens erradas (cáclular a área de
um quadrado com um lado de dimensão 4 não é válido).
-}

data Metros = Metros Int Double deriving Show

areaQuadrado :: Metros -> Metros
areaQuadrado (Metros 1 l) = Metros 2 (l * l)
areaQuadrado (Metros _ _) = error "Nao foi possivel calcular a area"

areaRet :: Metros -> Metros -> Metros
areaRet (Metros 1 b) (Metros 1 h) = Metros 2 (b*h)
areaRet (Metros _ _) (Metros _ _) = error "Nao foi possivel calcular a area"

areaCubo :: Metros -> Metros
areaCubo (Metros 1 a) = Metros 2 (6 * a * a)
areaCubo (Metros 2 m) = Metros 2 (6 * m)    -- representa uma entrada onde a área de uma das faces do cubo é fornecida.
areaCubo (Metros _ _) = error "Nao foi possivel calcular a area"

{-
Exercício 2.2 (Validação de nomes) Faça o novo tipo Valido que possui dois
value constructor Sim e Nao. O value constructor Sim possui um parâmetro
(campo) String. Implemente uma função isNomeValido que recebe um nome
e retorna Nao caso a String seja vazia e Sim caso contrário.
-}

data Valido = Sim String | Nao deriving Show

isNomeValido :: String -> Valido
isNomeValido [] = Nao
isNomeValido s  = Sim s

{-
Exercício 2.3 Refaça o exercício 3 do capítulo anterior usando record syntax e
tipos com parâmetro (siga o exemplo da conversão de medidas SI para imperial).
-}

data Valido' = Sim' {nome :: String} | Nao' deriving Show

isNomeValido' :: String -> Valido'
isNomeValido' [] = Nao'
isNomeValido' s = Sim' {nome = s}

{-
Exercício 2.4 Faça o tipo Numero, que possui um value constructor Ok com
um campo double e outro value constructor Erro com um campo String. Faça a
função dividir que divida dois números e caso o segundo número seja 0 emita um
erro (use o pattern matching). Exemplo,
    
    dividir(Numero 6) (Numero 5) = N umero1.2.
-}

data Numero = OK Double |
              Erro String
              deriving Show

dividir :: Numero -> Numero -> Numero
dividir _ (OK 0.0) = Erro "Division by 0!"
dividir (OK a) (OK b) = OK $ a/b
dividir _ _ = Erro "Please provide two Numeros"

{-
Exercício 2.5 Faça o tipo Cripto que possua dois value constructors Mensagem
e Cifrado ambos com um campo String e um value constructor Erro. Faça as
funções encriptar e decriptar seguindo cada exemplo a seguir

    encriptar(Mensagem ”FATEC”) = Cifrado ”GBUFD”
    decriptar(Cifrado ”DBTB”) = Mensagem ”CASA”.
    
OBS: a encriptação deve empurrar cada letra a frente e a decriptação, faz o in-
verso, empurrando uma letra para trás. Use as funções succ e pred e também list
compreeshions. Não é possível encriptar mensagens cifradas e decriptar mensa-
gens.
-}
data Cripto = Mensagem String |
              Cifrado String |
              Falha             -- Erro would conflict with the value constructor Erro declared on the previous exercise
              deriving Show

encriptar :: Cripto -> Cripto
encriptar (Mensagem m) = Cifrado [succ c | c <- m]
encriptar (Cifrado _) = Falha
encriptar _ = Falha

decriptar :: Cripto -> Cripto
decriptar (Cifrado m) = Mensagem [pred c | c <- m]
decriptar (Mensagem _) = Falha
decriptar _ = Falha
  



{-
Exercício 2.6 Faça uma função encriptarTodos que encripta (ou dá erro) todos
os elementos de um vetor de Cripto.
-}

encriptarTodos :: [Cripto] -> [Cripto]
encriptarTodos = map encriptar 

{-
Exercício 2.7 Tendo como base o exercício de conversão de medidas dado em
aula, crie uma função que faça conversão de câmbio. Você deve criar o tipo Cam-
bio contendo os value constructors Euro, Real e Dollar. Crie também o tipo Moeda
que possui os campos (val :: Double) e (cur :: Cambio). Use re-
cord syntax e as taxas de conversão do dia ao qual você fez o exercício (especifique
o dia por comentário).
-}

{-
Exercício 2.8 Crie a função converterTodosReal que recebe uma lista de Moedas
e retorna outra lista de Moedas com todos os seus elementos convertidos para Real.
Use list compreenshion.
-}

{-
Exercício 2.9 Crie a função maxMoeda que recebe uma lista de Moedas e re-
torna o valor máximo absoluto(sem conversão alguma) dentre os campos val
desta lista. Exemplo,
maxM oeda [Moeda 3 Real , Moeda 7 Dollar , Moeda 2 Euro] = 7.
OBS: Use a função maximum.
-}















