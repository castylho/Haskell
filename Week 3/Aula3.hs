--Exercicios de SALA
{-
Faça uma funçãp qie cpnverta os Dias para Ints.
Segunda vale 1, Terça 2, ..., DOmingo 7

-}
data Dias = Segunda | Terca | Quarta | Quinta | Sexta | Sabado | Domingo 

numDia :: Dias -> Int
numDia Segunda = 1
numDia Terca = 2
numDia Quarta = 3
numDia Quinta = 4
numDia Sexta = 5
numDia Sabado = 6
numDia _ = 7

{-
Crie um tipo Day que contenha os 7 dias da semana em ingles.
Implemente a função traduzirPI
que recebe os dias em Portugues e os traduza para o Ingles

Obs: Deixe explicito os tipos das funçoes sempre
-}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday
 | Saturday | Sunday deriving (Eq,Show)


traduzirPI :: Dias -> Day
traduzirPI Segunda = Monday
traduzirPI Terca = Tuesday
traduzirPI Quarta = Wednesday
traduzirPI Quinta = Thursday
traduzirPI Sexta = Friday
traduzirPI Sabado = Saturday
traduzirPI Domingo = Sunday


{-Faca uma funçao que a partir de uma carta retorna uma cor que a representa-}

data Naipe = Ouros | Paus | Espadas | Copas deriving(Enum,Show)

data Valor = As | Dois | Tres | Quatro |Cinco | Seis | Sete |
 Oito | Nove | Dez | Valete | Dama | Rei deriving (Enum, Show)

data Carta = Carta Valor Naipe deriving Show

cartas = [Carta x y | x<-[As .. Rei], y<-[Ouros .. Copas] ]

data Cor = Preto | Vermelho deriving Show

funcao :: Carta -> Cor
funcao (Carta _ Ouros) = Vermelho
funcao (Carta _ Copas) = Vermelho
funcao _ = Preto