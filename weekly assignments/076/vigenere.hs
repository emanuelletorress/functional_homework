repeatKey key n = take n $ cycle key -- repete a chave para ficar do tamanho da mensagem original
alphabetNumberList = zip ['A'..'Z'] [1..26] -- forma tuplas tipo [('A',1),('B',2)..('Z',26)]
letterKey l = head [number | el@(letter,number) <- alphabetNumberList, l == fst el] -- recebe um char e retorna o numero correspondente
numberKey n = [letter | el@(letter,number) <- alphabetNumberList, n == snd el] -- recebe um numero e retorna o char correspondente
vigenere str key = concat [numberKey (crypto x y) | (x, y) <- zip str longKey]
    where
        messageLen = length str
        longKey = repeatKey key messageLen
        crypto l1 l2 = ((letterKey l1 + letterKey l2) `mod` 26) - 1 