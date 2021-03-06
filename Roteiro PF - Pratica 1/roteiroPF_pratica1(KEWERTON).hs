--- Roteiro 1 de PF <11811BCC049> <Kewerton Caetano Evangelista>

--2) Escreva uma função para calcular o dobro de um número.

dobro x = x + x

--3) Escreva uma função para quadruplicar um número usando a função dobro definida no item anterior.

quadruplicar x = (dobro x) + (dobro x)

--4) Escreva uma função que, dadas as medidas dos catetos de um triângulo retângulo, retorne o valor de sua hipotenusa.

hipotenusa a b = sqrt ((a * a) + (b * b))

--5) Escreva uma função para calcular a distância entre dois pontos A e B num plano cartesiano.

distancia xa xb ya yb = sqrt (((xb - xa)*(xb - xa)) + ((yb - ya)*(yb - ya)))