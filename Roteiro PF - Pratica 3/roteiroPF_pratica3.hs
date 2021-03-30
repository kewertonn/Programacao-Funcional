--- Roteiro 3 de PF <11811BCC049> <Kewerton Caetano Evangelista>

--1) Operador lógico OU (pré-fixo):
--a) Apresente 3 definições para o operador lógico OU, utilizando casamento de padrões.
--b) Apresente 2 definições para o operador lógico OU, utilizando expressões condicionais (no lugar de casamento de padrões)

--2) Defina uma função que recebe dois pontos no espaço e retorna a distância entre eles.
--Considere que um ponto no espaço é representado por uma dupla de números (float) que correspondem às coordenadas do ponto.

type Ponto = (Float, Float)
distancia :: Ponto -> Ponto -> Float
distancia (xa, ya) (xb, yb) = sqrt (((xb - xa)*(xb - xa)) + ((yb - ya)*(yb - ya)))

--3) Dado um valor inteiro, escreva a função recursiva fatorial. 
--Obs: Fazer uma definição usando guardas e outra com casamento de padrões.

fatorial :: Int -> Int
fatorial n 
    |n==0 = 1 
    |otherwise = n*fatorial(n-1)

--4) Dado um número inteiro n, escreva a função recursiva fibo que retorna o n-ésimo termo da sequência de Fibonacci a seguir, sendo os casos base F0 = 0 e F1 = 1. Utilize a definição recursiva vista em sala: fibo(n) = fibo(n-2) + fibo(n-1).

fibo :: Int -> Int
fibo n 
    |n==0 = 0 
    |n==1 = 1 
    |otherwise = fibo(n-2) + fibo(n-1)

--5) Dado um número inteiro n, escreva a função recursiva n_tri, que retorna o n-ésimo termo da sequência de números triangulares, dada a seguir.


--6) Escreva a função potencia2, que calcula a potência de 2 elevada a um expoente n de forma recursiva: 2n = 2n-1 * 2.

potencia :: Int -> Int
potencia n = (2^n-1)*2

potencia2 :: Int -> Int
potencia2 n
    | = potencia(n-1)
