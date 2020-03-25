{--
    Este codigo diferencia do antecessor [allperms2] pelo predicado
    que limita a construcao da arvore em perms, obedecendo as regras
    do problema das oito damas.
    
    usage:
        :set +s
        teste
        
    return:
        92
        (0.73 secs, 50,091,264 bytes)
        
    A arvore pode ser observada com
        

--}
data Arv a = No a [Arv a]
type Split a = (a, [a])

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

{--
    Na minha estrategia com Abaco (allperms0b) eu consegui inserir um predicado em splits
    de forma a limitar a ramificacao das listas. No presente codigo o predicado precisou 
    ser inserido em perms, com a ajuda de uma pequena pilha.
--}

-- perms :: [a] -> [Arv (Maybe a)]
perms = go [] where
    go _ [] = []
    go pilha m = map f $ splits m where
        f(p, rest)
                | queen (p:pilha) =  No (Just p) $ go (p:pilha) rest
                | otherwise = No Nothing []

-- build :: [a] -> Arv (Maybe a)
build = No Nothing . perms

ftree :: Arv (Maybe a) -> [[a]]
ftree (No Nothing ns) = concat [ftree n | n <- ns]
ftree (No (Just a) []) = [[a]]
ftree (No (Just a) ns) = [a:n | n <- ftree (No Nothing ns)]



-- Layout ------------------------------------------------------------
teste = do
          let b = build [1..8]
          -- print b          -- arvore bonita, mas muito longa para 8queens
          foldMap (\x -> print x) $ ftree b
          putStr "numero de solucoes: "
          print $ length $ ftree b
          








-- predicado 8queens -------------------------------------------------------------    
queen :: [Int] -> Bool
queen q = go [] q where
    go _ [] = True
    go pilha (p:xs) 
        | and [p /= c + n  && p /= c - n | (c, n) <- zip pilha [1..]] = go (p:pilha) xs
        | otherwise = False













----------------------------------------------------------------------    
-- deixando aqui pra usar, talvez
----------------------------------------------------------------------  
-- https://wiki.haskell.org/Foldable_and_Traversable

instance Foldable Arv where
    foldr f x0 (No a ns) = 
        foldr f x0 (a : concat (fmap (foldMap (\x -> [x])) ns))

instance Functor Arv where
    fmap f (No a ns) = No (f a) (fmap (fmap f) ns)

instance Applicative Arv where
    pure x = No x []
    No f tfs <*> tx@(No x txs) =
        No (f x) (map (f <$>) txs ++ map (<*> tx) tfs)

instance Monad Arv where
    -- (>>=)  :: m a -> (a -> m b) -> m b
    -- return :: a -> m a
    return = pure
    No x ns >>= f = case f x of
        No x' ns' -> No x' (ns' ++ map (>>= f) ns)

instance Show a => Show (Arv a) where
    show m = "\n\n" ++ (unlines $ go  m) ++ "\n\n"
      where
        go (No a ns) =  lines ("(" ++ show a ++ ")") ++ subtree ns
        subtree [] = []
        subtree [n] = " \x2502" : " \x2502" : (zipWith (++) (" \x2514\x2500" : repeat "    ") (go n))
        subtree (n:ns) = " \x2502" : " \x2502" : (zipWith (++) (" \x251c\x2500" : repeat " \x2502  ") (go n)) ++ subtree ns

--  https://www.ime.usp.br/~pf/analise_de_algoritmos/aulas/NPcompleto.html
