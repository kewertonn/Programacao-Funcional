---Roteiro 4 de PF <11811BCC049> <Kewerton Caetano Evangelista>

---3) Funções que utilizam listas enumeradas
--a) Utilizando enumeração, construir uma função que dados dois inteiros a e b construa a lista dos inteiros contidos no intervalo fechado [a,b]. Quando a for igual a b, a função devolve a lista unitária [a]. Quando a > b a função deverá devolver a lista vazia.

lst :: Int -> Int -> [Int]
lst a b 
    |a == b = [a]
    |a > b = []
    |otherwise = [a..b]

--b) Utilizando enumeração, construir uma função que dados dois inteiros a e b construa a lista dos inteiros pares contidos no intervalo aberto (a,b). Quando a for igual a b ou a > b a função devolve a lista vazia. (*Dica: verificar se a é par ou ímpar)

lst_par :: Int -> Int -> [Int]
lst_par a b 
    |a == b && even a = [a]
    |a > b = []
    |otherwise = [x | x<-[a..b], even x]

---5) Usando lista por compreensão, escreva a função quadrados que recebe dois inteiros e retorna os quadrados dos números entre eles. E.g.:

quadrados :: Int -> Int -> [Int]
quadrados a b = [x^2 | x <- [a..b]]

---6) Usando lista por compreensão, escreva a função seleciona_ímpares que recebe um lista de inteiros e retorna uma nova lista com todos os números ímpares presentes na lista de entrada.

seleciona_impares :: [Int] -> [Int]
seleciona_impares a = [x | x <- a, odd x]

---7) Escreva a função tabuada que recebe um valor inteiro e retorna a lista de seus dez primeiros múltiplos. E.g.:

tabuada :: Int -> [Int]
tabuada a = [x*a | x <- [1..10]]

---8) Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma lista com os valores que representam anos bissextos. Dica: use a função bissexto do roteiro anterior.

bissexto :: Int -> Bool
bissexto x 
    | (mod x 400 == 0) = True 
    | (mod x 4 == 0) && (mod x 100 /= 0) = True 
    | otherwise = False

bissextos :: [Int] -> [Int]
bissextos a = [x | x <- a, bissexto x]

---9) Usando lista por compreensão, escreva a função sublistas que recebe uma lista formada por sublistas de um mesmo tipo e retorna uma lista com todos os elementos da lista de entrada na mesma ordem, mas no nível da lista principal, sem sublistas.

sublistas :: [[a]] -> [a]
sublistas a = concat [concat x | x <- [a]]

---10) Sejam os tipos Data, Emprestimo, Emprestimos e a variável bdEmprestimo do exemplo da Biblioteca. Escreva a função atrasados que recebe um parâmetro do tipo Emprestimos e a Data atual, e retorna uma lista com todos os empréstimos atrasados. 

type Data = (Int, Int, Int)
type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"), ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

valida :: Data -> Bool
valida (dia, mes, ano)
    |((mes == 1 || mes == 3 || mes == 5 || mes == 7 || mes == 8 || mes == 10 || mes == 12) && (dia >= 1 && dia <= 31)) = True 
    |((mes == 4 || mes == 6 || mes == 9 || mes == 11) && (dia >= 1 && dia <= 30)) = True 
    |(mes == 2 && (dia >= 1 && dia <= 28)) || (bissexto ano == True && mes == 2 && dia >= 1 && dia <= 29) = True 
    | otherwise = False

precede :: Data -> Data -> Bool
precede (dia1, mes1, ano1) (dia2, mes2, ano2) = if (valida (dia1, mes1, ano1) && valida (dia2, mes2, ano2)) == True && ((ano1 < ano2) || (ano1 == ano2 && mes1 < mes2) || (ano1 == ano2 && mes1 == mes2 && dia1 < dia2)) then True else False

verifica :: Emprestimo -> Data -> Bool
verifica (codL, codA, (diaE, mesE, anoE), (diaD, mesD, anoD), sit) (dia, mes, ano) = if (valida(dia, mes, ano) == True && valida(diaD, mesD, anoD) == True) && (precede (dia, mes, ano) (diaD, mesD, anoD) == True) then True else False

atrasados bdEmprestimo (d,m,a) = [ x | x <- bdEmprestimo,not (verifica x (d, m, a))]

---11) Usando compreensão de listas, escreva a função uniaoNRec a seguir que faz a união de duas listas de modo que ela mantenha todos os elementos da 1a lista na mesma ordem e no final acrescenta apenas os elementos da 2a lista que não estejam presentes na 1a lista.

uniaoNRec a b = [x | x <- a]++[y | y <- b, not (elem y a)]