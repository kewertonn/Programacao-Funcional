---Roteiro 6 de PF <11811BCC049> <Kewerton Caetano Evangelista>

----1) Refaça as seguintes funções dos roteiros 2, 3 e 4, utilizando o comando “where” para definições locais (incluindo funções auxiliares que são necessárias na solução da função principal):

--a) Escreva a função valida que indica se uma data é válida ou não.

type Data = (Int, Int, Int)

bissexto :: Int -> Bool
bissexto x 
    | mult400 == 0 = True 
    | mult4 == 0 && mult100 /= 0 = True 
    | otherwise = False
    where
        mult400 = mod x 400;
        mult4 = mod x 4;
        mult100 = mod x 100

valida :: Data -> Bool
valida (dia, mes, ano)
    |((mes == janeiro || mes == marco || mes == maio || mes == julho || mes == agosto || mes == outubro || mes == dezembro) && (dia >= 1 && dia <= 31)) = True 
    |((mes == abril || mes == junho || mes == setembro || mes == novembro) && (dia >= 1 && dia <= 30)) = True 
    |(mes == fevereiro && (dia >= 1 && dia <= 28)) || (bissexto ano == True && mes == fevereiro && dia >= 1 && dia <= 29) = True 
    | otherwise = False
    where
        janeiro = 1;
        fevereiro = 2;
        marco = 3;
        abril = 4;
        maio = 5;
        junho = 6;
        julho = 7;
        agosto = 8;
        setembro = 9;
        outubro = 10;
        novembro = 11;
        dezembro = 12

--b) Escreva a função bissextos a seguir que recebe uma lista de inteiros e retorna uma lista com os valores que representam anos bissextos.

bissextos :: [Int] -> [Int]
bissextos a = listabissexto
    where 
        listabissexto = [x | x <- a, bissexto x]


--c) Escreva a função atrasados que recebe um parâmetro do tipo Emprestimos e a Data atual, e retorna uma lista com todos os empréstimos atrasados.

type Emprestimo = (String, String, Data, Data, String)
type Emprestimos = [Emprestimo]

bdEmprestimo::Emprestimos
bdEmprestimo = [("H123C9","BSI945",(12,9,2009),(20,09,2009),"aberto"), ("L433C5","BCC021",(01,9,2009),(10,09,2009),"encerrado"), ("M654C3","BCC008",(04,9,2009),(15,09,2009),"aberto")]

precede :: Data -> Data -> Bool
precede (dia1, mes1, ano1) (dia2, mes2, ano2) = if (valida data1 && valida data2) == True && (ano1menor || (anoigual && mes1menor) || (anoigual && mesigual && dia1menor)) then True else False 
    where 
        data1 = (dia1, mes1, ano1); 
        data2 = (dia2, mes2, ano2);
        ano1menor = ano1 < ano2;
        anoigual = ano1 == ano2;
        mes1menor = mes1 < mes2;
        mesigual = mes1 == mes2;
        dia1menor = dia1 < dia2

verifica :: Emprestimo -> Data -> Bool
verifica (codL, codA, (diaE, mesE, anoE), (diaD, mesD, anoD), sit) (dia, mes, ano) = if (valida dataatual == True && valida datadevolucao == True) && (precede dataatual datadevolucao == True) then True else False
    where
        dataatual = (dia, mes, ano);
        datadevolucao = (diaD, mesD, anoD)

atrasados bdEmprestimo (d,m,a) = atrasos 
    where 
        atrasos = [ x | x <- bdEmprestimo,not (verifica x (d, m, a))]

--d) Faça uma segunda definição da função recursiva fibo2 que retorna o n-ésimo termo da sequência de Fibonacci utilizando recursividade e os conceitos a seguir (use a função passo(x,y)).

--e) Escreva a função fatorial usando a função prodIntervalo.


----2) Refaça as funções do exercício 1, utilizando o comando “let” para definições locais (incluindo funções auxiliares que são necessárias na solução da função principal). Repetir para os itens “a” a “e”.
--a)

bissextol :: Int -> Bool
bissextol x = let mult400 = mod x 400; mult4 = mod x 4; mult100 = mod x 100
    in if mult400 == 0 then True else if mult4 == 0 && mult100 /= 0 then True else False

validal :: Data -> Bool
validal (dia, mes, ano) = let janeiro = 1; fevereiro = 2; marco = 3; abril = 4; maio = 5; junho = 6; julho = 7; agosto = 8; setembro = 9; outubro = 10; novembro = 11; dezembro = 12
    in if ((mes == janeiro || mes == marco || mes == maio || mes == julho || mes == agosto || mes == outubro || mes == dezembro) && (dia >= 1 && dia <= 31)) then True else if ((mes == abril || mes == junho || mes == setembro || mes == novembro) && (dia >= 1 && dia <= 30)) then True else if(mes == fevereiro && (dia >= 1 && dia <= 28)) || (bissextol ano == True && mes == fevereiro && dia >= 1 && dia <= 29) then True else False

--b)

bissextosl :: [Int] -> [Int]
bissextosl a = let listabissexto = [x | x <- a, bissexto x] in listabissexto


--c)

precedel :: Data -> Data -> Bool
precedel (dia1, mes1, ano1) (dia2, mes2, ano2) = let data1 = (dia1, mes1, ano1); data2 = (dia2, mes2, ano2); ano1menor = ano1 < ano2; anoigual = ano1 == ano2; mes1menor = mes1 < mes2; mesigual = mes1 == mes2; dia1menor = dia1 < dia2
    in if (valida data1 && valida data2) == True && (ano1menor || (anoigual && mes1menor) || (anoigual && mesigual && dia1menor)) then True else False 

verifical :: Emprestimo -> Data -> Bool
verifical (codL, codA, (diaE, mesE, anoE), (diaD, mesD, anoD), sit) (dia, mes, ano) = let dataatual = (dia, mes, ano); datadevolucao = (diaD, mesD, anoD) 
    in if (validal dataatual == True && validal datadevolucao == True) && (precedel dataatual datadevolucao == True) then True else False


atrasadosl bdEmprestimo (d,m,a) = let atrasos = [ x | x <- bdEmprestimo,not (verifical x (d, m, a))] in atrasos

--d)

--e)


----5) Codifique as seguintes expressões do cálculo lambda em Haskell e avalie as mesmas no GHCi:
---a) (λx λy. y)((λz. z)(λz. z))(λw. w) 5

(\y -> y) 5 

---b) ((λf. (λx. f(f x))) (λy. (y * y))) 3

(\y -> (y*y)) ((\y -> (y*y))3)
--ou
(\y -> (y*y)*(y*y))3

---c) ((λf. (λx. f(f x)))(λy.(+ y y))) 5

(\y ->(y + y)+(y + y))5

---d) ((λx. (λy. + x y) 5) ((λy. - y 3) 7))

(\y -> 5 + y) ((\y -> y - 3)7)
--ou
(\y -> 5 + y) 4

---e) (((λf. (λx. f(f(f x)))) (λy. (y * y))) 2)

(\y -> (y*y))((\y -> (y*y))((\y -> (y*y))2))

---f) (λx. λy. + x ((λx. - x 3) y)) 5 6

(\(x,y) -> x + y) ((\x -> (x-3))5 ,6)
