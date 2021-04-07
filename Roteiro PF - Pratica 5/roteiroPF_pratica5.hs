---Roteiro 5 de PF <11811BCC049> <Kewerton Caetano Evangelista>

---1) Implemente as funções recursivas vistas nas vídeo-aulas 9 e 10: conta_ch, conta, maior, primeiros, pertence, uniaoR.

conta_ch :: [Char] -> Int
conta_ch [] = 0
conta_ch (x:resto) = 1 + conta_ch resto

conta :: [t] -> Int
conta [] = 0
conta (x:r) = 1 + conta r

maior :: [Int] -> Int
maior [x] = x
maior (x:y:resto)
    |x > y = maior (x:resto) 
    |otherwise = maior (y:resto)

primeiros :: Int -> [t] -> [t]
primeiros 0 _ = []
primeiros _ [] = []
primeiros n (x:xs) = x:primeiros (n-1) xs

pertence :: Eq t => t -> [t] -> Bool
pertence a [] = False
pertence a (x:z) = if (a == x) then True else pertence a z

uniaoR :: Eq t => [t] -> [t] -> [t]
uniaoR [] l = l
uniaoR (x:xs) l = if pertence x l == True then uniaoR xs l else x:uniaoR xs l

---2) Escreva a função recursiva npares que recebe uma lista de inteiros e retorna a quantidade de números pares pertencentes à lista.

npares :: [Int] -> Int
npares [] = 0
npares (x:xs) = if mod x 2 == 0 then (npares xs)+1 else npares xs

---3) Escreva a função recursiva produtorio que recebe uma lista de números e retorna o produto de todos os seus elementos.

produtorio :: [Int] -> Int
produtorio [] = 1
produtorio (x:xs) = x * produtorio xs

---4) Escreva a função recursiva comprime a seguir que recebe uma lista de listas e retorna uma lista contendo todos os elementos das sublistas.

comprime :: [[t]] -> [t]
comprime [] = []
comprime (x:resto) = x++comprime resto

---5) Escreva a função recursiva tamanho a seguir que recebe uma lista polimórfica (de qualquer tipo) e retorna a quantidade de elementos que ela possui.

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = (tamanho xs)+1

---6) Escreva a função recursiva uniaoRec2 que faz a união de duas listas de modo que mantenha todos os elementos da 1a lista na mesma ordem e no final acrescenta os elementos da 2a lista que não estejam presentes na primeira.

uniaoRec2 :: Eq t => [t] -> [t] -> [t]
uniaoRec2 as bs = as ++ [b | b <- bs, not (pertence b as)]