module Exercicios where


--Gere Listas
--a) [1,11,121,1331,14641,161051,1771561] 11 ^ x
funcao21a::Int-> [Int]
funcao21a num = [11^x | x<-[0..num]]

--b)[1,2,3,5,6,7,9,10,11,13,14,15,17,18,19,21,22,23,25,26,27,29,30,31,33,34,35,37,38,39] falta multiplo 4

funcao21b :: [Int] -> [Int]
funcao21b lista = [x | x<-lista, mod x 4 /= 0]

--c) ["AaBB", "AbBB", "AcBB", "AdBB", "AeBB", "AfBB","AgBB"]

funcao21c :: Int -> [[Char]]
funcao21c letra = [ 'A' : ['a'..'z'] !! x: 'B' : 'B' : [] | x<-[0..letra] ]

--d) [5,8,11,17,20,26,29,32,38,41] tirou 14,23,35, posições 3,6,10
funcao21d :: Int -> [Int]
funcao21d qtde = [5 + (n-1)*3 | n<-[1..qtde], n/=4, n/=7, n/=11]

--e) [1.0,0.5,0.25,0.125,0.0625,0.03125]
funcao21e :: Int-> [Float]
funcao21e num = [1/2^x | x <-[0..num] ]
--f) [1,10,19,28,37,46,55,64] 1+9
funcao21f :: [Int]
funcao21f = [1,10..70]

--g) [2,4,8,10,12,16,18,22,24,28,30] 1,2,4,5,6,7,9,11,12,14,15 -> falta 3,8,10,13
funcao21g :: [Int] -> [Int]
funcao21g lista = [x*2 | x<-lista, x/=3, x/=7, x/=10, x/=13]

--h) ['@','A','C','D','E','G','J','L'] 
--falta B, F, H, I, K
--tirou posiçao 2, 6, 8, 9, 11 ?????

--Crie uma função que verifique se o tamanho de uma String é par ou não. Use Bool como retorno.

funcao22 :: [Char] -> Bool
funcao22 frase = even (length frase)


--Escreva uma função que receba um vetor de Strings e retorne uma lista com todos os 
--elementos em ordem reversa.

funcao23 :: [[Char]] -> [[Char]]
funcao23 lista = reverse lista

{-Escreva uma função que receba um vetor de Strings e
retorne uma lista com o tamanho de cada String. As palavras de
tamanho par devem ser excluídas da resposta

REFAZER
-}
funcao24 :: [[Char]] -> [Int]
funcao24 nomes = [ y |y<- length (nomes !! x) , x<-[0..(length nomes - 1) ], even y	 ]


--Escreva a função head como composição de duas outras.
funcao25 :: [Char] -> Char
funcao25 string = (last . reverse) string

--Faça uma função que receba uma String e retorne True se esta for um palíndromo; caso contrário, False
funcao26 :: [Char] -> Bool
funcao26 string = string == reverse string


{-Faça uma função que receba um inteiro e retorne uma
tupla, contendo: o dobro deste número na primeira coordenada, o
triplo na segunda, o quádruplo na terceira e o quíntuplo na quarta.-}

funcao27 :: Int -> [(Int, Int)]
funcao27 x = [(x,x*y) |  y <- [1..5] ]



















