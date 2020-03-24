----------------------------------------------------------------------
data Arv a = No a [Arv a]
type Split a = (a, [a])

{-- Introducao 

    Quero resolver com este codigo uma classe de problemas do tipo que
    uma solucao eh a permutacao de k elementos. Por ex, as N colunas da
    posicao de n damas num tabuleiro NxN; a sequencia de N² casas de um
    tabuleiro NxN que representa o tour do movimento do cavalo que passa
    por todas as casas; a permutacao dos numeros de 1 a N² quadrado de 
    NxN celulas, conhecido como quadrado magico; 
    
    Este codigo, inicialmente, nao resolve problemas como por ex 8puzzle
    ou rubik cube pois, a priori, a solucao nao se trata da premutacao
    de um numero k de elementos (ou lances).
    Futuramente, no entanto, faremos a ligacao entre estes problemas, pois
    todos passam por uma busca em uma arvore.
    
    https://www.ime.usp.br/~pf/analise_de_algoritmos/aulas/NPcompleto.html
--}


{--
    Comeco, portanto, com um metodo de obtencao de permutacoes em haskell
--}

split :: [a] -> Split a
split (a:b) = (a, b)        

{--
    Deixei split separado de splits pra ficar mais claro o que esta sendo feito.
    split separa o head do tail de uma lista recebida e coloca numa tupla.
    splits faz isso varias vezes, devolvendo o head para o split anterior.
    com efeito, retorna-se uma lista de splits onde o que é separado da lista 
    a cada iteracao eh sequencialmente um dos itens dessa lista.
    
    ex: 
    *Main> splits [1..5]
    [(1,[2,3,4,5]),(2,[1,3,4,5]),(3,[1,2,4,5]),(4,[1,2,3,5]),(5,[1,2,3,4])]
    
    Esta lista nos ajudara a montar naodeterministicamente uma arvore cuja altura
    eh o numero de elementos de uma lista e o caminho da raiz ate alguma folha eh uma 
    permutacao desses elementos. 
    
--}

splits :: [a] -> [Split a]
splits [] = []
splits ab = split ab : cauda ab
cauda (x:xs) = [(a,x:b) | (a, b) <- splits xs]

perms :: [a] -> [Arv (Maybe a)]
perms [] = []
perms m = fmap f (splits m) where
    f(n, ns) = No (Just n) $ perms ns

root :: [Arv (Maybe a)] -> Arv (Maybe a)
root m = No Nothing m

build :: [a] -> Arv (Maybe a)
build = root . perms

ftree :: Arv (Maybe a) -> [[a]]
ftree (No Nothing ns) = concat [ftree n | n <- ns]
ftree (No (Just a) []) = [[a]]
ftree (No (Just a) ns) = [a:n | n <- ftree (No Nothing ns)]

-- Layout --------------------------------------------------------
seed = [1..8]
teste = length $ [ p | p <- ftree $ build seed, queen p]


----------------------------------------------------------------------    

queen :: Integral a => [a] -> Bool
queen q = go [] q where
    go _ [] = True
    go pilha (p:xs) 
        | and [p /= c + n  && p /= c - n | (c, n) <- zip pilha [1..]] = go (p:pilha) xs
        | otherwise = False

----------------------------------------------------------------------    



{---------------------------------------------------------------------

9! = 362880

com arvore (28.17 secs, 1,781,911,152 bytes)

com lista     (6.76 secs, 565,669,208 bytes)

microsegundos com fat




nqueens 92 sol

com arvore (5.63 secs, 395,820,608 bytes)
com listas (0.70 secs,  45,607,720 bytes)


--}

instance Show a => Show (Arv a) where
    show m = "\n\n" ++ (unlines $ go  m) ++ "\n\n"
      where
        go (No a ns) =  lines ("(" ++ show a ++ ")") ++ subtree ns
        subtree [] = []
        subtree [n] = " \x2502" : " \x2502" : (zipWith (++) (" \x2514\x2500" : repeat "    ") (go n))
        subtree (n:ns) = " \x2502" : " \x2502" : (zipWith (++) (" \x251c\x2500" : repeat " \x2502  ") (go n)) ++ subtree ns
