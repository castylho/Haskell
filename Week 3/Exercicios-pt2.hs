module Exercicios_pt2 where
{-
3.13) Faça um novo tipo chamado Metros , que possui um
\textit{value constructor}
de mesmo nome, cujos
parâmetros são: um Int que representa a dimensão, e um
Double que representa o valor da medida e outro chamado
MetragemInvalida . Implemente as funções:
areaQuadrado :: Metros -> Metros : calcula a área de um quadrado.
areaRet :: Metros -> Metros -> Metros : calcula a área de um retângulo.
areaCubo :: Metros -> Metros : calcula a área de  um cubo.

Exemplo:
Prelude> areaQuadrado (Metros 1 2.0)
Metros 2 4.0

Use o pattern matching para ignorar as metragens erradas
(calcular a área de um quadrado com um lado de dimensão 4 não é
válido).
-}

data Metros = Metros Int Double | MetragemInvalida deriving Show

areaQuadrado :: Metros -> Metros 
areaQuadrado (Metros 1 lado) = Metros 2 (lado * lado)
areaQuadrado _ = MetragemInvalida

areaRet :: Metros -> Metros -> Metros
areaRet (Metros 1 lado) (Metros 1 altura) = Metros 2 (lado*altura)
areaRet _ _ = MetragemInvalida

areaCubo :: Metros -> Metros 
areaCubo (Metros 1 lado) = Metros 3 (lado * lado * 6)
areaCubo _ = MetragemInvalida

{-3.14) Faça o novo tipo Valido que possui dois value
constructors Sim e Nao . O value constructor Sim possui um
parâmetro (campo)
String . Implemente uma função
isNomeValido que recebe um nome e retorna Nao caso a
String seja vazia; caso contrário, Sim .
-}
data Valido = Sim String| Nao deriving Show
isNomeValido :: Valido -> Valido
isNomeValido (Sim x) = Sim "Sim" 

{-3.15) Refaça o exercício 3 do capítulo anterior usando record
syntax e tipos com parâmetro (siga o exemplo da conversão de
medidas SI para imperial).
-}
data Lista = List{xval :: [String]} deriving Show
reverter1 :: Lista -> [String]
reverter1 x = reverse (xval x)

{-3.16) Faça o tipo Numero , que possui um value constructor
Ok com um campo double e outro value constructor Erro com um campo String . 
Faça a função dividir que divida dois
números e, caso o segundo número seja 0, emita um erro 
(use o pattern matching). Exemplo:

Prelude> dividir (Numero 6) (Numero 5)
Numero 1.2.
-}

data Numero = Ok Double | Erro16 String deriving Show
dividir :: Numero -> Numero -> Numero
dividir _ (Ok 0) = Erro16 "Impossivel dividir por zero"
dividir (Ok x) (Ok y) = Ok (x/y)

{-3.17) Faça o tipo Cripto que possua dois values constructors
Mensagem e Cifrado , ambos com um campo String e um
value constructor Erro . Faça as funções encriptar e
decriptar , seguindo cada exemplo a seguir.
Prelude> encriptar (Mensagem "FATEC")
	Cifrado "GBUFD"

Prelude> decriptar (Cifrado "DBTB")
	Mensagem "CASA"

Veja que a encriptação deve empurrar cada letra a frente e a
decriptação faz o inverso, empurrando uma letra para trás. Use as
funções succ e pred , e também list compreeshions. Não é
possível encriptar mensagens cifradas e decriptar mensagens.
-}
data Cripto = Mensagem String | Cifrado String | Erro17 deriving Show

encriptar :: Cripto -> Cripto
encriptar(Mensagem xs) = (Cifrado [succ x | x<- xs])
encriptar _ = Erro17

descriptar :: Cripto -> Cripto
descriptar(Mensagem xs) = (Cifrado [pred x | x<- xs])
descriptar _ = Erro17

{-3.18) Faça uma função encriptarTodos que encripta (ou dá
erro) todos os elementos de um vetor de Cripto .
-}
encriptarTodos :: [Cripto] -> [Cripto]
encriptarTodos segredos = [ encriptar (x) | x<-segredos]

{-3.19) Tendo como base o exercício de conversão de medidas,
crie uma função que faça conversão de câmbio. Você deve criar o
tipo Cambio contendo os value constructors Euro , Real e
Dollar . Crie também o tipo Moeda que possui os campos val
:: Double e cur :: Cambio . Use record syntax e as taxas de
conversão do dia no qual você fez o exercício.
-}
data Cambio = Euro | Dollar | Real deriving Show
data Moeda = Moeda {valor::Double, curr :: Cambio} deriving Show

converterDollar :: Moeda -> Moeda
converterDollar (Moeda x Real) = Moeda (0.32*x) Dollar
converterDollar(Moeda x Euro) = Moeda (1.17*x) Dollar
converterDollar x = x

converterReal :: Moeda -> Moeda
converterReal (Moeda x Dollar) = Moeda (x*3.14) Real
converterReal (Moeda x Euro) = Moeda (x*3.72) Real
converterReal x = x

converteEuro :: Moeda -> Moeda
converteEuro (Moeda x Real) = Moeda (x*0.268) Euro
converteEuro (Moeda x Dollar) = Moeda (x * 0.843) Euro
converteEuro x = x
{-3.20) Crie a função converterTodosReal que recebe uma
lista de moedas e retorna outra lista de moedas com todos os seus
elementos convertidos para Real
Use list compreenshion.-}
converterTodosReal :: [Moeda] -> [Moeda]
converterTodosReal lista_moedas = [converterReal x | x<-lista_moedas]

{-3.21) Crie a função maxMoeda que recebe uma lista de moedas
e retorna o valor máximo absoluto (sem conversão alguma) dentre
os campos val desta lista. Exemplo:

Prelude> maxMoeda [Moeda 3 Real, Moeda 7 Dollar, Moeda 1 Euro]
7

Use a função maximum .

-}


maxMoeda :: [Moeda] -> Double
maxMoeda dim = maximum [valor x | x<-dim]
