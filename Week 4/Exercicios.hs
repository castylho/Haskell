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
dobro :: [Int] -> [Int]
dobro x = map (*2) x

tira4 :: [Int] -> [Int]
tira4 lista = [y | y<- dobro lista, mod y 4 /= 0]

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

data Dia = Segunda | Terça | Quarta | Quinta | Sexta | Sabado | Domingo deriving (Show,Eq,Enum)

elimTer :: Dia -> Dia -> Bool
elimTer Terça d = d /= Terça

{-4.8) Implemente o tipo Dinheiro que contenha os campos
valor e correncia ( Real ou Dolar ), e uma função que
converta todos os "dinheiros" de uma lista para dólar (e outra para
real). Com isso, implemente funções para:

-Filtrar todos os Dolares de uma lista de Dinheiro .
-Somar todos os Dolares de uma lista.
-Contar a quantidade de Dolares de uma lista
-}

{-4.9) Usando a função foldl , crie lambdas para:
Contar números negativos de uma lista de Int .

-Contar letras 'P' de uma String .
-Para contar Sabados em uma lista de um [DiaSemana] .

-Para, a partir de uma lista de [DiaSemana] , retornar a soma dos dias. 
Exemplo: [Segunda, Segunda, Quarta] deve retornar 5 . 

Use uma função auxiliar para converter DiaSemana para Int .

-}

