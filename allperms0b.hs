{--
    Este codigo conta o numero de solucoes para o problema 8queens.
    eu usei para calcular o tempo da construcao das ramificacoes de solucoes
    
    Esta verssao diferencia da anterior allperms0 pela inclusao de um predicado
    que limita a ramificacao de splits. A funcao queen contem as regras que 
    limitam a construcao das combinacoes, resultando no final, em perms, apenas
    as permutacoes que coincidem com a solucao do problema das oito damas.
    
    usage:
        :set +s
        teste 
    
    result:
        92
        (0.61 secs, 45,607,344 bytes)
        
--}

type Abaco a = ([a], [a])

splits :: Integral a => Abaco a -> [Abaco a] 
splits (_, []) = []
splits ab@(a, p:b)
              | queen (p:a) = (p:a, b) : cauda ab
              | otherwise = cauda ab
              
cauda :: Integral a => Abaco a -> [Abaco a] 
cauda (a, p:b) = [(m, p:n) | (m, n) <- splits (a, b)]              

perms :: Integral a => [a] -> [Abaco a]
perms m = go m where
    go [] = [([], m)] 
    go (_:count) = 
        go count >>= splits

queen :: Integral a => [a] -> Bool
queen q = go [] q where
    go _ [] = True
    go pilha (p:xs) 
        | and [p /= c + n  && p /= c - n | (c, n) <- zip pilha [1..]] = go (p:pilha) xs
        | otherwise = False

----------------------------------------------------------------------    
teste = length $ perms [1..8]




