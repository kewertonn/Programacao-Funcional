---Roteiro Trabalho 1 de PF <11811BCC049> <Kewerton Caetano Evangelista>

----1)analisa_raizes

analisa_raizes :: Double -> Double -> Double -> String
analisa_raizes a b c 
    |a == 0 = "4 - Equacao degenerada"
    |b^2 > 4*a*c = "1 - Possui duas raizes reais"
    |b^2 == 4*a*c = "2 - Possui uma raiz real"
    |b^2 < 4*a*c = "3 - Nenhuma raiz reais"

----2)equacao

equacao :: Double -> Double -> Double -> (Double, Double)
equacao a b c
    |a /= 0 = ((-b + sqrt(b^2-4*a*c))/2, (-b - sqrt(b^2-4*a*c))/2)
    |otherwise = (-(c/b), a)
--b*x+c => x = -(c/b)

----3)passagem

type Data = (Int, Int, Int)

idade :: Data -> Data -> Int
idade (da,ma,aa) (dn,mn,an) 
    |aa > an && ma < mn = (aa - an) - 1
    |aa > an && ma > mn = (aa - an)
    |aa > an && ma == mn && da > dn = (aa - an) - 1
    |aa > an && ma == mn && da <= dn = (aa - an)
    |otherwise = 0

passagem :: Float -> Data -> Data -> Float
passagem valor (da,ma,aa) (dn,mn,an) 
    |idade (da,ma,aa) (dn,mn,an) >= 70 = valor*0.50 
    |idade (da,ma,aa) (dn,mn,an) < 2 = valor + valor*0.15 
    |idade (da,ma,aa) (dn,mn,an) <= 10 = valor*0.40
    |otherwise = valor

----4)
--a) gera1: gerar a lista de inteiros, contendo o cubo de todos os pares entre 3 e 11.

gera1 :: [Int]
gera1 = [x^3 | x <- [3..13], even x]

--b) gera2: gerar a lista de duplas formadas tendo o primeiro elemento menor ou igual a 5 e o segundo elemento no intervalo fechado entre o valor do primeiro elemento e o seu triplo.

gera2 :: [(Int,Int)]
gera2 = [(x,y) | x<-[1..5],y<-[1..20], y >= x && y <= (3*x)]

--c) gera3: a partir de uma lista l1=[15,16], gerar a lista com todos os elementos dentro do intervalo fechado definido entre 1 e cada elemento de l1 (Obs.: pode ter elemento repetido na lista final)

l1 :: [Int]
l1 = [15,16]
gera3 :: [Int]
gera3 = [x | x <- [1..head l1]] ++ [y | y <- [1..last l1]]

--d) gera4: gerar uma lista de duplas, onde cada dupla são 2 números consecutivos de 1 a 10, sendo o primeiro elemento par (Ex: (2,3) e (4,5))

gera4 :: [(Int,Int)]
gera4 = [(x,x+1) | x<-[1..10], x < 10, even x]

--e) gera5: a partir da lista de duplas geradas no item d, gerar a lista onde cada elemento corresponde à soma dos elementos da dupla.

gera5 :: [Int]
gera5 = [fst x + snd x | x <- gera4]

----5)
--a)

contaNegM2 :: [Int] -> Int
contaNegM2 lista = length [x | x <- lista, (rem x 3) == 0, x > 0]

--b)

listaNegM2 :: [Int] -> [Int]
listaNegM2 lista = [x | x <- lista, (rem x 3) == 0, x > 0]

----6)

fatores :: Int -> [Int]
fatores x = [y | y <-[1..x], rem x y == 0]

primos :: Int -> Int -> [Int]
primos x y = [z | z <-[x..y], length (fatores z) == 2]

----7)

mdc2 :: Int -> Int -> Int
mdc2 x y 
    |x < y = mdc2 y x 
    |y == 0 = x 
    |otherwise = mdc2 y (mod x y)

mmc2 :: Int -> Int -> Int
mmc2 x y = div (x * y) (mdc2 x y)

mmc::Int -> Int -> Int -> Int
mmc x y z = mmc2 x (mmc2 y z)

----8)

serie :: Int -> Int -> Double
serie x n = (fromIntegral (sum [y | y <- [1..n], odd y]) / fromIntegral(x)) + (sum [fromIntegral(x)/fromIntegral(y) | y <- [1..n], even y])

----9)

fizzbuzz :: Int -> [String]
fizzbuzz n = [if mod i 2 == 0 && mod i 3 == 0 then "FizzBuzz" else if mod i 2 == 0 then "Fizz" else if mod i 3 == 0 then "Buzz" else if mod i 2 == 0 && mod i 3 == 0 then "FizzBuzz" else "No" | i <- [1..n]]

----10)

seleciona_multiplos :: Int -> [Int] -> [Int]
seleciona_multiplos x lista = [y | y <- lista, rem y x == 0]

----11)

unica_ocorrencia :: Int -> [Int] -> Bool
unica_ocorrencia e lista = if (length [x | x <- lista, x == e]) == 1 then True else False

----12)

intercala :: [a] -> [a] -> [a]
intercala [] [] = []
intercala e [] = e
intercala [] e = e
intercala (x:xs) (h:hs) = x:h:intercala xs hs 

----13)

zipar :: [a] -> [a] -> [[a]]
zipar [] [] = []
zipar x [] = []
zipar [] x = []
zipar (x:xs) (h:hs) = [x,h]:zipar xs hs

----14)

type Contato = (String, String, Int, String)

--a lista a seguir foi utilizada para testes enquanto construia o codigo a seguir
contatos :: [Contato]
contatos = [("Rosa", "Rua Saturno, 999", 97663321, "rosa@hotmail.com"), ("Jonas", "Rua Jupiter, 1610", 99817272, "jonas@hotmail.com"), ("Maria", "Rua Marte, 771", 32336677, "maria@hotmail.com"), ("Jose", "Rua Terra, 300", 32442355, "jose@hotmail.com")]

email :: Contato -> String
email (nome, endereco, numero, email) = email

nome :: Contato -> String
nome (nome, endereco, numero, email) = nome

encontra :: String -> [Contato] -> String
encontra e [] = "Email desconhecido"
encontra e (x:xs) 
    |e == email x = nome x
    |otherwise = encontra e xs

----15)
type Pessoa = (String, Float, Int, Char)

pessoas :: [Pessoa]
pessoas = [ ("Rosa",1.66, 27, 'C'), ("João", 1.85, 26, 'C'), ("Maria", 1.55, 62, 'S'), ("Jose", 1.78, 42, 'C'), ("Paulo", 1.93, 25, 'S'), ("Clara", 1.70, 33, 'C'), ("Bob", 1.45, 21, 'C'), ("Rosana", 1.58,39, 'S'), ("Daniel", 1.74, 72, 'S'), ("Jocileide", 1.69, 18, 'S')]

--A altura média entre todas as pessoas.
altura :: Pessoa -> Float
altura (nome, altura, idade, estado) = altura

altura_media :: Float
altura_media = sum [altura x | x <- pessoas] / 10

--A idade da pessoa mais nova
age :: Pessoa -> Int
age (nome, altura, idade, estado) = idade

nova :: Int
nova = minimum [age x | x <- pessoas]

--O nome e o estado civil da pessoa mais velha.
velha :: Pessoa -> (String, Char)
velha (nome, altura, idade, estado) = (nome, estado)

mais_velha :: [(String, Char)]
mais_velha = [velha x | x <- pessoas, maximum [age y | y <- pessoas] == age x]

--Todos os dados de cada pessoa com 50 anos ou mais.
dados50 :: [Pessoa]
dados50 = [x | x <- pessoas, age x >= 50]

--O número de pessoas casadas com idade superior a i (ex: i = 35).
estado_civil :: Pessoa -> Char
estado_civil (nome, altura, idade, estado) = estado

casadas_acima :: Int -> Int
casadas_acima i = length [x | x <- pessoas, estado_civil x == 'C', age x > i]

----16)

insere_ord :: Ord a => a -> [a] -> [a]
insere_ord x [] = [x]
insere_ord x (h:hs) = if x <= h then x: h: hs else h:insere_ord x hs

----17)

reverte :: [a] -> [a]
reverte [] = []
reverte (h:hs) = (reverte hs)++[h]

----18)

elimina :: Eq a => a -> [a] -> [a]
elimina x [] = []
elimina x (h:hs) = if x == h then elimina x hs else [h] ++ elimina x hs

elimina_repet :: Eq a => [a] -> [a]
elimina_repet [] = []
elimina_repet (x:xs) = x: elimina_repet (elimina x xs)

----19)---ESTUDAR

disponiveis = [1,2,5,10,20,50,100]

notasTroco :: Int -> [[Int]]
notasTroco 0 = [[]]
notasTroco x = [y:ys | y <- disponiveis, x >= y, ys <- notasTroco (x-y)] 

----20)