--- Roteiro 2 de PF <11811BCC049> <Kewerton Caetano Evangelista>

--1) Reescreva as funções do último roteiro (dobro, quadruplicar, hipotenusa, distância), definindo a prototipação de tipos de cada função. Obs: se você já fez isso no primeiro roteiro, envie novamente para o exercício ficar completo.

dobro :: Int -> Int
dobro x = x + x

quadruplicar :: Int -> Int 
quadruplicar x = (dobro x) + (dobro x)

hipotenusa :: Float -> Float -> Float
hipotenusa a b = sqrt ((a * a) + (b * b))

distancia :: Float -> Float -> Float -> Float -> Float
distancia xa xb ya yb = sqrt (((xb - xa)*(xb - xa)) + ((yb - ya)*(yb - ya)))

--3) Dado um valor monetário em reais, escreva uma função conversao que retorna uma tupla-3 com o valor em Real, Dolar e Euro, sendo que 1 real = 3,96 dólares = 4,45 euros.

conversao :: Float -> (Float, Float, Float)
conversao x = (x, x*3.96, x*4.45)

--4) Implemente a função bissexto que, dado um ano (inteiro), indique se ele é bissexto ou não.

bissexto :: Int -> Bool
bissexto x 
    | (mod x 400 == 0) = True 
    | (mod x 4 == 0) && (mod x 100 /= 0) = True 
    | otherwise = False

--5) Defina o tipo Data dado a seguir. Escreva a função bissexto2 que recebe uma data e indique se ela pertence a um ano bissexto ou não.

type Data = (Int, Int, Int)
bissexto2 :: Data -> Bool
bissexto2 (dia, mes, ano) = if bissexto ano == True then True else False

--6) Escreva a função valida que indica se uma data é válida ou não.

valida :: Data -> Bool
valida (dia, mes, ano)
    |((mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) && (dia >= 1 && dia <= 31)) = True 
    |((mes == 4 || mes == 6 || mes == 9 || mes == 11) && (dia >= 1 && dia <= 30)) = True 
    |(mes == 2 && (dia >= 1 && dia <= 28)) || (bissexto ano == True && mes == 2 && dia >= 1 && dia <= 29) = True 
    | otherwise = False

--7) Escreva a função precede que recebe 2 datas e indica se a 1a data é anterior à 2a.

precede :: Data -> Data -> Bool
precede (dia1, mes1, ano1) (dia2, mes2, ano2) = if (valida (dia1, mes1, ano1) && valida (dia2, mes2, ano2)) == True && ((ano1 < ano2) || (ano1 == ano2 && mes1 < mes2) || (ano1 == ano2 && mes1 == mes2 && dia1 < dia2)) then True else False

--8) Implemente as estruturas de dados (tuplas) para um sistema de gerenciamento de bibliotecas e depois as defina como tipos. O sistema tem 3 estruturas básicas:
--Livro: composto por código do livro, título do livro, autor, editora e ano de publicação.
--Aluno: composto por código do aluno, nome, e-mail e telefone.
--Empréstimo: composto por código do livro, código do aluno, data de empréstimo, data de devolução e situação. Obs: utilize a estrutura/tipo auxiliar data do exercício 4.

type Livro = (String, String, String, String, Int)
type Aluno = (String, String, String, Int)
type Emprestimo = (String, String, Data, Data, String)

--9) Seja o tipo Emprestimo e o exemplo dado a seguir, composto por código do livro, código do aluno, data de empréstimo, data de devolução e situação. Escreva uma função que verifica se um empréstimo está em dia, dado um empréstimo e a data de hoje.

verifica :: Emprestimo -> Data -> Bool
verifica (codL, codA, (diaE, mesE, anoE), (diaD, mesD, anoD), sit) (dia, mes, ano) = if (valida(dia, mes, ano) == True && valida(diaD, mesD, anoD) == True) && (precede (dia, mes, ano) (diaD, mesD, anoD) == True) then True else False
