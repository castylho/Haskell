module Exercicios_pt1 where


{-3.1) Crie o tipo Pergunta com os values constructors Sim
ou Nao . Faça as funções seguintes, determinando seus tipos
explicitamente.

pergNum: recebe via parâmetro uma Pergunta .
Retorna 0 para Nao e 1 para Sim .

listPergs : recebe via parâmetro uma lista de
Perguntas , e retorna 0 s e 1 s correspondentes aos
constructores contidos na lista.

and' : recebe duas Perguntas como parâmetro e
retorna a tabela verdade do and lógico, usando
Sim como verdadeiro e Nao como falso.

or' : idem ao anterior, porém deve ser usado o ou
lógico.

not' : idem aos anteriores, porém usando o not
lógico.
-}
data Pergunta = Sim | Nao

pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0

listPergs :: [Pergunta] -> [Int]
listPergs lista = [pergNum x| x<-lista]

and' :: Pergunta -> Pergunta -> Pergunta
and' Sim Sim = Sim
and' _ _ = Nao

or' :: Pergunta -> Pergunta -> Pergunta
or' Nao Nao = Nao
or' _ _ = Sim

not' :: Pergunta -> Pergunta -> Pergunta
not' Nao Nao = Nao
not' Nao Sim = Sim
not' Sim Nao = Sim
not' Sim Sim = Nao

{-3.2) Faça o tipo Temperatura que pode ter valores Celsius ,
Farenheit ou Kelvin . Implemente as funções:

converterCelsius: recebe um valor double e uma
temperatura, e faz a conversão para Celsius.

converterKelvin : recebe um valor double e uma
temperatura, e faz a conversão para Kelvin.

converterFarenheit : recebe um valor double e
uma temperatura, e faz a conversão para Farenheit.

Celsius to Fahrenheit:   °C × 1,8 + 32 = °F
Fahrenheit to Celsius:   (°F − 32) / 1,8 = °C
-}

data Temperatura = Celsius Double | Farenheit Double | Kelvin Double

converterCelsius :: Temperatura -> Double
converterCelsius (Farenheit x) = (x-32)/1.8
converterCelsius (Kelvin x) = x - 273.15

converterFarenheit :: Temperatura -> Double
converterFarenheit (Celsius x) = (x*1.8) + 32
converterFarenheit (Kelvin x) = (x*1.8) - 459.67

converterKelvin :: Temperatura -> Double
converterKelvin (Celsius x) = x + 273.15
converterKelvin (Farenheit x) = (x+459.67) *(5/9)

{-3.3) Implemente uma função que simule o vencedor de uma
partida de pedra, papel e tesoura usando tipos criados. Casos de
empate devem ser considerados em seu tipo.
-}

data Jogada = Pedra | Papel | Tesoura
partida :: (Jogada,Jogada) -> String
partida(Pedra,Pedra) = "Empate! 2 Pedras"
partida(Pedra,Papel) = "Vitoria do papel"
partida(Pedra, Tesoura) = "Vitoria da Pedra"
partida(Papel, Tesoura) = "Vitoria da tesoura"
partida(Papel, Papel) = "Empate! 2 Papel"
partida(Tesoura, Tesoura) = "Empate! 2 Tesouras"

{-
3.4) Faça uma função que retorne uma string, com todas as
vogais maiúsculas e minúsculas eliminadas de uma string passada
por parâmetro usando list compreenshion.
-}

--funcao34 :: String -> String
--funcao34 ls = [x | x<-ls, x=/'a','e','i']

{-3.5) Sabe-se que as unidades imperiais de comprimento podem
ser Inch , Yard ou Foot (há outras ignoradas aqui). Sabe-se
que 1in=0.0254m , 1yd=0.9144m , 1ft=0.3048 . Faça a função
converterMetros que recebe a unidade imperial e o valor
correspondente nesta unidade. Esta função deve retornar o valor
em metros.
Implemente também a função converterImperial , que
recebe um valor em metros e a unidade de conversão. Esta função
deve retornar o valor convertido para a unidade desejada.
-}
data Medida = Inch | Yard | Foot deriving Show

converterMetros :: Double -> Medida -> Double
converterMetros unit Inch = unit * 0.0254
converterMetros unit Yard = unit * 0.9144
converterMetros unit Foot = unit * 0.3048

converterImperial :: Double -> Medida -> Double
converterImperial unit Inch = unit * 39.37
converterImperial unit Yard = unit * 1.09
converterImperial unit Foot = unit * 3.38

{-3.6) Faça um novo tipo chamado Mes , que possui como
valores todos os meses do ano. Implemente:
A função checaFim , que retorna o número de dias
que cada mês possui (considere fevereiro tendo 28
dias).
A função prox , que recebe um mês atual e retorna o
próximo mês.
A função estacao , que retorna a estação do ano de
acordo com o mês e com o hemisfério.
Use apenas tipos criados pela palavra data aqui.
	norte
Primavera Março a Maio
Verao Junho a Agosto
Outono Setembro a Novembro
Inverno Dezembro a eFevereiro
-}

data Mes = Janeiro | Fevereiro | Marco | Abril | Maio | Junho | Julho
 | Agosto | Setembro | Outubro | Novembro | Dezembro deriving (Enum,Show)

checaFim :: Mes -> Int
checaFim Janeiro = 30
checaFim Fevereiro = 28
checaFim Marco = 30
checaFim Abril = 31
checaFim Maio = 30
checaFim Junho = 31
checaFim Julho = 30
checaFim Agosto = 31
checaFim Setembro = 30
checaFim Outubro = 31
checaFim Novembro = 30
checaFim Dezembro = 31

prox :: Mes -> Mes
prox x = succ x

data Hemisferio = Norte | Sul

estacao :: Hemisferio -> Mes -> String
estacao Norte Junho = "Verao"
estacao Norte Julho = "Verao"
estacao Norte Agosto = "Verao"
estacao Norte Dezembro = "Inverno"
estacao Norte Janeiro = "Inverno"
estacao Norte Fevereiro = "Inverno"
estacao Norte Setembro = "Outono"
estacao Norte Outubro = "Outono"
estacao Norte Novembro = "Outono"
estacao Norte Marco = "Primavera"
estacao Norte Abril = "Primavera"
estacao Norte Maio = "Primavera"

estacao Sul Junho = "Inverno"
estacao Sul Julho = "Inverno"
estacao Sul Agosto = "Inverno"
estacao Sul Dezembro = "Verao"
estacao Sul Janeiro = "Verao"
estacao Sul Fevereiro = "Verao"
estacao Sul Setembro = "Primavera"
estacao Sul Outubro = "Primavera"
estacao Sul Novembro = "Primavera"
estacao Sul Marco = "Outubro"
estacao Sul Abril = "Outubro"
estacao Sul Maio = "Outono"



{-3.7) Faça uma função que receba uma String e retorne
True se esta for um palíndromo; caso contrário, False .
-}
isPalindrome:: String -> Bool
isPalindrome string = string == reverse string


{-3.8) Faça uma função que elimine todos os números pares,
todos os ímpares múltiplos de 7 e negativos de uma lista de inteiros
passada via parâmetro. Você deve retornar esta lista em ordem
reversa em comparação a do parâmetro.
-}
funcao38 :: [Int] -> [Int]
funcao38 ls = [ x | x<-reverse ls, even x, mod x 7 /= 0, x>=0]


{-3.9) Faça uma função que recebe três Strings x , y e z como
parâmetro. A função retorna uma tupla com três coordenadas
contendo a ordem reversa em cada. A primeira coordenada deve
conter string reversa do primeiro parâmetro, e assim por diante.
teste2 x y z = [(x,y,z) | x<- reverse x, y<-reverse y, z<-reverse z]
-}

funcao39 :: [Char] -> [Char] -> [Char] -> ([Char], [Char], [Char])
funcao39 x y z = (reverse x, reverse y, reverse z)

{-3.10) Faça uma função chamada revNum , que receba uma
String s e um Int n . Esta deverá retornar as n primeiras letras
em ordem reversa e o restante em sua ordem normal. Exemplo:
revNum 4 "FATEC" = "ETAFC"

'a' : 'b' : [] = "ab"
reverse ('a' : 'b' : []) = "ba"

reverse [fatec !! x : [] | x<-[0..3] ]
["E","T","A","F"]


-}
--revNum :: Int -> String -> String


{-3.11) Crie o tipo de dado Binario que pode ser Zero ou
Um . Faça outro tipo de dado chamado Funcao que pode ser
Soma2 , Maior , Menor ou Mult2 . Implemente a função
aplicar que recebe uma Funcao e dois Binarios . Seu retorno
consiste em executar a operação desejada. Exemplo:
aplicar Soma2 Um Um = Zero
-}
data Binario = Zero | Um deriving Show

data Funcao = Soma2 | Maior | Menor | Mult2 deriving Show

aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Maior Zero Zero = Zero
aplicar Maior _ _ = Um

aplicar Menor Um Um = Zero
aplicar Menor _ _ = Zero

aplicar Mult2 Um Um = Um
aplicar Mult2 _ _ = Zero

aplicar Soma2 Zero Zero = Zero
aplicar Soma2 Um Um = Zero
aplicar Soma2 _ _ = Um

{-3.12) Faça uma função chamada binList , usando list
compreeshion, que recebe uma lista de Binarios (ver exercício
anterior) e retorna outra lista com elemento somado Um e
convertido para Int . 
Exemplo:
	binList [Um, Zero, Zero, Um, Zero] = [0,1,1,0,1]
-}

viraInt :: Binario -> Int
viraInt Zero = 0
viraInt _ = 1

binList :: [Binario] -> [Int]
binList bins = [viraInt(aplicar Soma2 Um x) | x<-bins]


