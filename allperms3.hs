----------------------------------------------------------------------
data Arv a = No a [Arv a]
type Split a = (a, [a])

split :: [a] -> Split a
split (a:b) = (a, b)        

splits :: [a] -> [Split a]
splits [] = []
splits ab = split ab : cauda ab
cauda (x:xs) = [(a,x:b) | (a, b) <- splits xs]

perms :: [a] -> [Arv (Maybe a)]
perms [] = []
perms m = f <$> splits m where
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
seed = [1..4]
teste = printXs $ ftree $ build seed

printXs m = go m where
    go [] = return ()
    go (x:xs) = do
                 print x
                 go xs




{---------------------------------------------------------------------

então agora vc  tem como montar uma árvore que enumera todas as permutações

agora vc precisa de uma função que gera a lista de todas as permutações, 

um processo de folding
Preencha os três casos para essa função:

foldMyTree (Node Nothing xs)  = ??   :: (Arv (Maybe a)) -> [[a]]
foldMyTree (Node (Just x) []) = ??
foldMyTree (Node (Just x) xs) = ??

no seu exemplo, ela vai gerar [[1,2,3,4,5], [1,2,3,5,4], [1,2,4,3,5],...]
a ideia geral é que se teremos uma forma estruturada de gerar essa lista fazendo:

foldMyTree (buildTree sementes)
e aproveitamos a avaliação preguiçosa para buscar a solução 
dentro dessa lista de listas sem gastar memória

----------------------------------------------------------------------

no caso da busca em profundidade, 
você primeiro gera uma solução completa, avalia e, 
se não for a solução, joga fora


(Nothing)
 |
 |
 +--.(Just 1)
 |   |
 |   |
 |   +--.(Just 2)
 |   |   |
 |   |   |
 |   |   +--.(Just 3)
 |   |   |   |
 |   |   |   |
 |   |   |   `--.(Just 4)
 |   |   |
 |   |   |
 |   |   `--.(Just 4)
 |   |       |
 |   |       |
 |   |       `--.(Just 3)
 |   |
 |   |
 |   +--.(Just 3)
 |   |   |
 |   |   |
 |   |   +--.(Just 2)
 |   |   |   |
 |   |   |   |
 |   |   |   `--.(Just 4)
 |   |   |
 |   |   |
 |   |   `--.(Just 4)
 |   |       |
 |   |       |
 |   |       `--.(Just 2)
 |   |
 |   |
 |   `--.(Just 4)
 |       |
 |       |
 |       +--.(Just 2)
 |       |   |
 |       |   |
 |       |   `--.(Just 3)
 |       |
 |       |
 |       `--.(Just 3)
 |           |
 |           |
 |           `--.(Just 2)
 |
 |
 +--.(Just 2)
 |   |
 |   |
 |   +--.(Just 1)
 |   |   |
 |   |   |
 |   |   +--.(Just 3)
 |   |   |   |
 |   |   |   |
 |   |   |   `--.(Just 4)
 |   |   |
 |   |   |
 |   |   `--.(Just 4)
 |   |       |
 |   |       |
 |   |       `--.(Just 3)
 |   |
 |   |
 |   +--.(Just 3)
 |   |   |
 |   |   |
 |   |   +--.(Just 1)
 |   |   |   |
 |   |   |   |
 |   |   |   `--.(Just 4)
 
 ... 

--}

instance Show a => Show (Arv a) where
    show m = "\n\n" ++ (unlines $ go  m) ++ "\n\n"
      where
        go (No a ns) =  lines ("(" ++ show a ++ ")") ++ subtree ns
        subtree [] = []
        subtree [n] = " \x2502" : " \x2502" : (zipWith (++) (" \x2514\x2500" : repeat "    ") (go n))
        subtree (n:ns) = " \x2502" : " \x2502" : (zipWith (++) (" \x251c\x2500" : repeat " \x2502  ") (go n)) ++ subtree ns
