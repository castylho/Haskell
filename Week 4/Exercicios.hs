{-4.1) Faça uma função que retorne a média de um [Double] ,
usando foldl .
1- recebe lista
2- length lista
3- lista / length lista

-}
soma x = foldl (+) 0 x

media :: [Double] -> Double
media x = foldl (+) 0 x / fromIntegral(length x)

{-4.2) Faça uma função que receba uma [String] e retorne
todos os elementos palíndromos. Ver exercício 3.7.
-}

retornaPalin :: [String] -> [String]
retornaPalin frase = filter (\f -> f == reverse f) frase

{-4.3) Implemente uma função que filtre os números pares e
outra que filtre os ímpares de uma lista recebida via parâmetro.
-}
apenasPar :: [Int] -> [Int]
apenasPar lista = filter (even) lista

apenasImpar :: [Int] -> [Int]
apenasImpar lista = filter (odd) lista

{-4.4) Filtre os números primos de uma lista recebida por
parâmetro.
-}

divisivel n = [x | x<-[1..n], mod n x == 0]

ehPrimo x = length (divisivel x) == 2

todosPrimos :: [Int] -> [Int]
todosPrimos lista = filter (ehPrimo) lista

{-4.5) Implemente uma função que receba uma lista de inteiros e
retorne o dobro de todos, eliminando os múltiplos de 4.

1-recebe lista
2- multiplica por 2
3- retira multiplos de 4

-}

tira4 :: [Int] -> [Int]
tira4 lista = filter (\x -> mod x 4 /= 0) $ map (*2) lista

{-4.6) Faça uma função func que receba uma função f de tipo
(String -> String) , e uma String S que retorna o reverso
de S concatenado com aplicação da função f em s .-} 

func46 :: (String -> String)-> String ->  String
func46 func frase = reverse frase ++ func frase 

{-4.7) Crie um tipo Dia contendo os dias da semana. Faça uma
função que receba uma lista de Dias e filtre as Terças .

1- recebe lista de dias
2- Terça esta presente -> Bool usar elem?
3- retorno lista de dias
-}

data DiaSemana = Segunda | Terça | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show,Eq,Enum)

elimTer :: [DiaSemana] -> [DiaSemana]
elimTer dias = filter (\x -> x/= Terça) dias

{-4.8) Implemente o tipo Dinheiro que contenha os campos
valor e correncia ( Real ou Dolar ), e uma função que
converta todos os "dinheiros" de uma lista para dólar (e outra para
real). Com isso, implemente funções para:

-Filtrar todos os Dolares de uma lista de Dinheiro .
-Somar todos os Dolares de uma lista.
-Contar a quantidade de Dolares de uma lista
-}
data Correncia = Euro | Dollar | Real deriving (Show,Eq)


data Dinheiro = Dinheiro {valor::Double,
                          curr :: Correncia
                          } deriving Show



converteReal :: Dinheiro -> Dinheiro
converteReal (Dinheiro x Dollar) = Dinheiro (x*3.16) Real
converteReal x = x



filtraDolar :: [Dinheiro]-> [Dinheiro]
filtraDolar list = filter (\x -> curr(x) == Dollar) list

somaDolar :: [Dinheiro] -> Double
somaDolar lista = sum $ map (valor) (filtraDolar lista)

qtdeDolares :: [Dinheiro] -> Int
qtdeDolares lista = length (filtraDolar lista)

{-4.9) Usando a função foldl , crie lambdas para:
Contar números negativos de uma lista de Int .

-Contar letras 'P' de uma String .

-Para contar Sabados em uma lista de um [DiaSemana] .

-Para, a partir de uma lista de [DiaSemana] , retornar a soma dos dias. 
Exemplo: [Segunda, Segunda, Quarta] deve retornar 5 . 

Use uma função auxiliar para converter DiaSemana para Int .

-}
diaInt :: DiaSemana -> Int
diaInt Segunda = 1
diaInt Terça = 2
diaInt Quarta = 3
diaInt Quinta = 4
diaInt Sexta = 5
diaInt Sabado = 6
diaInt _ = 7

contaP :: String -> Int
contaP frase = length $ filter (\x -> x == 'p') frase

somaDias :: [DiaSemana] -> Int
somaDias dias = foldl (+) 0 $ map diaInt dias

contSabado :: [DiaSemana] -> Int
contSabado lista = length $ filter (\x -> x == Sabado) lista

