----------------------------------------------------------------------
data Arv a = No a [Arv a]
type Split a = ([a], [a])

splits :: Integral a => Split a -> [Split a] 
splits (_, []) = []
splits ab@(a, p:b)
              | queen (p:a) =  (p:a, b) : cauda ab
              | otherwise = cauda ab
              
cauda :: Integral a => Split a -> [Split a] 
cauda (a, p:b) = [(m, p:n) | (m, n) <- splits (a, b)]              


perms :: Integral a => [a] -> [Split a]
perms m = go m where
    go [] = [([], m)] 
    go (_:count) = 
        go count >>= splits

{--

Essa eh a explicacao de porque perms tem essa recursao doida
e porque eu preciso deixar expandido no comentario porque
eu vou esquecer (de novo) como montei isso (de novo)

Eu tenho que chamar splits que vai me gerar uma coisa assim:

*Main> splits ([], [1..4])
>> [([1],[2,3,4]),([2],[1,3,4]),([3],[1,2,4]),([4],[1,2,3])]

Repare que eu chamei splits com o parametro no formato de 'Split' 
(uma tupla de duas listas) porque ela vai chamar a si propria 
por intermedio de 'cauda' que na verdade eh 'splits' 
com seus heads recuperados.

Esses Splits sao o coracao do algoritmo.
Basicamente, splits pega o head de uma lista e passa pra outra.
e faz isso tantas vezes quantos forem os elementos dela. depois,
recursivamente, devolve esses heads para a iteracao anterior.
com efeito temos uma lista com n splits, 
cada um com um dos itens 'guardados' na 'lista auxiliar', que
na verdade nao eh uma lista auxiliar e sim a lista que contera
os itens na ordem que vamos colocar. Seria mais ou menos como
um automato nao deterministico que se subdivide em todas as 
possibilidades a cada iteracao. 

De posse dessa primeira iteracao, basta chamar novamente splits
para cada item da saida de splits. Administrando as entradas e saidas,
pois o que queremos aplicar a split estara dentro de uma lista. 
Essa eh a funcao de perms que tera que repetir quantas iteracoes
forem necessarias ate esvaziar a lista de itens. 

A funcao f pega cada split e devolve uma lista de splits. 
O concatMap pega cada lista de splits e junta numa nova lista.


perms1 m = concatMap f $ splits ([], m)
    where
        f (a, b) = splits (a, b)


perms2 m = concatMap f $ perms1 m
    where
        f (a, b) = splits (a, b)


perms3 m = concatMap f $ perms2 m
    where
        f (a, b) = splits (a, b)


perms3 [1..4]  


Todas essas funcoes sao resumidas numa so, usando o bind com splits.



--}


-- 





{--
ftree :: Arv [a] -> [[a]]
ftree (No [] ns) = concat [ftree n | n <- ns]
ftree (No [a] []) = [[a]]
ftree (No [a] ns) = [a:n | n <- ftree (No [] ns)]
--}
----------------------------------------------------------------------    
-- seed = [1..9]
-- teste = length $ ftree $ build seed
teste = length $ perms [1..8]
-- teste = foldMap (\x -> print x) $ perms [1..8]



{--
    allperms0      (7.13 secs, 565,669,208 bytes)
    allperms3   (29.27 secs, 1,750,346,056 bytes)
    
--}


----------------------------------------------------------------------    

queen :: Integral a => [a] -> Bool
queen q = go [] q where
    go _ [] = True
    go pilha (p:xs) 
        | and [p /= c + n  && p /= c - n | (c, n) <- zip pilha [1..]] = go (p:pilha) xs
        | otherwise = False

----------------------------------------------------------------------    
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

