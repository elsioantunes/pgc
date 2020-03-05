import System.Process
import Data.Ord
import Data.List

--------------------------------------------------------------------
-- As solucoes sao baseadas numa permutacao de inteiros 
-- que eventualmente eu posso converter em outra coisa
-- como a notacao algebrica do knight's tour
type Perm = [Int]
type Action = Int
type ProblemRule = Perm -> [Int] -> Action -> Bool


-- leaf-labeled rose tree
data Tree a = Folha a | Node [Tree a]  deriving Show


-- Chamei de Abaco, mas isso deve ter um nome certo
type Candidatos = [Int]
type Escolhidos = [Int]
type Abaco = (Escolhidos, Candidatos)


-- operador para expandir ramificar uma arvore
(-<) :: Tree t -> (t -> Tree a) -> Tree a
(-<) (Folha x) f = f x 
(-<) (Node ns) f = Node [n -< f | n <- ns]


-- Dado uma folha contendo um Abaco (e a regra do problema), 
-- retorno uma ramificacao de estados dele
--  ramifica :: Abaco -> Tree Abaco
ramifica :: ProblemRule -> Abaco -> Tree Abaco
ramifica regra (xs, ys) = 
    Node [Folha (abacoUpdt act) | act <- ys, regra xs ys act]
    where
        abacoUpdt act = (act:xs, delete act ys)
        delete act ys = [a | a <- ys, a /= act]


--------------------------------------------------------------------
-- Utils
toList (Folha x) = [fst x]
toList (Node []) = []
toList (Node (n:ns)) =  toList n ++ (toList $ Node ns)

printSol [] = return ()
printSol (x:xs) = do
               print x
               printSol xs

--------------------------------------------------------------------
-- Layout 

ch n = toEnum n :: Char
od c = fromEnum c

pQs _ [] = return ()
pQs col m = do
           print $ take col m
           pQs col $ drop col m

pAs ntab [] = return ()
pAs ntab (x:xs) = do 
              putStrLn $ unwords $ int2algeb <$> x
              putStrLn ""
              pAs ntab xs
    where 
        int2algeb n = [ch (p + 97), ch (q + 49)] ++ ", "
            where (p, q) = divMod n ntab  

--------------------------------------------------------------------
main = do 
        system "clear" -- linux
        
        putStrLn "Imprime 15 das 92 solucoes de nqueens 8"
        printSol $ take 15 $ nqueens 8
        putStrLn ""
        
        putStrLn "imprime caminho do cavalo no tab 8x8"
        pAs 8 $ take 1 $ cavalo 63
        putStrLn ""
        
        
        -- imprime quadrados magicos

                
--------------------------------------------------------------------
-- Um teste de velocidade 
-- listando o numero de solucoes 
-- pra cada tamanho de tabuleiro 

-- usage:
--  prelude> :set +s
--  prelude> testeVelocidade

testeVelocidade = [length $ nqueens n | n <- [0..11]]



----------------------------------------------------------------------
----------------------------------------------------------------------
-- problemas que o algoritmo resolve
-- n queens
nqueensRule :: ProblemRule
nqueensRule escolhidos _ p = and [p /= c + n  && p /= c - n | (c, n) <- zip escolhidos [1..]]

-- aplicar gmap a uma arvore, expande-lhe suas folhas (um nivel inteiro). 
-- temos que aplicar n vezes, onde n eh o tamanho do tabuleiro
nqueens n = toList $ arvc n  
    where
        arvc 0 = Folha ([], [1..n])
        arvc x = arvc (x-1) -< ramifica nqueensRule


---------------------------------------------------------------------- 
-- ProblemRule = Perm -> [Int] -> Action -> Bool
cavaloRule  [] _ _ = True
cavaloRule (x:ss) ys p 
                  | not $ valid x p = False
                  | p /= p' = False
                  | otherwise = True

               where
                  valid k h = (a-c)*(a-c) + (b-d)*(b-d) == 5
                      where 
                        ((a, b), (c, d)) = (divMod k 8, divMod h 8)
                 
                  -- Warnsdorff's Rule
                  p' = minimumBy criterio $ neigb x
                  neigb k = [h | h <- ys, valid k h]
                  criterio = comparing $ length . neigb

cavalo n = toList $ arvc n  
    where
        arvc 0 = Folha ([], [1..n])
        arvc x = arvc (x-1) -< ramifica cavaloRule


{----------------------------------------------------------------------

magic q _ p _ = renan $ length q
    where 
        m a g i c = a+g+i+c == 34
        filip (_:_:c:_:_:f:_:_:i:_) = m c f i p
        miojo (_:_:_:_:e:_:_:_:_:j:_:_:_:_:o:_) = m e j o p
        bruno (a:b:c:_) = m a b c p
        aline (_:_:_:d:_:_:_:h:_:_:_:l:_) = m d h l p
        renan n -- escovacao de bit demais. desculpe
                |(n == 12) = filip q && aline q
                |(n == 15) = miojo q
                |(n == 3 || n == 7 || n == 11) = bruno q
                |(n == 13 || n == 14) = aline q
                |otherwise = True

----------------------------------------------------------------------}
{--

[1,1,0,0,2,10,4,40,92,352,724,2680] -- com foldl
(10.51 secs, 2,256,783,552 bytes)

[1,1,0,0,2,10,4,40,92,352,724,2680] -- com iterate
(10.74 secs, 2,245,790,944 bytes)

[1,1,0,0,2,10,4,40,92,352,724,2680] -- com cauda
(14.10 secs, 2,704,378,168 bytes)

[6,1,5,2,8,3,7,4] -- uma das solucoes


-- arvore gerada em < arv 4 >
Node [
    Node [
        Node [
            Node [
                Folha ([4,3,2,1],[])
            ],
            Node [
                Folha ([3,4,2,1],[])]
            ],
            Node [Node [Folha ([4,2,3,1],[])],Node [Folha ([2,4,3,1],[])]],Node [Node [Folha ([3,2,4,1],[])],Node [Folha ([2,3,4,1],[])]]],Node [Node [Node [Folha ([4,3,1,2],[])],Node [Folha ([3,4,1,2],[])]],Node [Node [Folha ([4,1,3,2],[])],Node [Folha ([1,4,3,2],[])]],Node [Node [Folha ([3,1,4,2],[])],Node [Folha ([1,3,4,2],[])]]],Node [Node [Node [Folha ([4,2,1,3],[])],Node [Folha ([2,4,1,3],[])]],Node [Node [Folha ([4,1,2,3],[])],Node [Folha ([1,4,2,3],[])]],Node [Node [Folha ([2,1,4,3],[])],Node [Folha ([1,2,4,3],[])]]],Node [Node [Node [Folha ([3,2,1,4],[])],Node [Folha ([2,3,1,4],[])]],Node [Node [Folha ([3,1,2,4],[])],Node [Folha ([1,3,2,4],[])]],Node [Node [Folha ([2,1,3,4],[])],Node [Folha ([1,2,3,4],[])]]]]



Node [
    Node [
        Node [
            Node [
                Folha ([4,3,2,1],[])
            ],
            Node [
                Folha ([3,4,2,1],[])
            ]
        ],
        Node [
            Node [
                Folha ([4,2,3,1],[])
            ],
            Node [
                Folha ([2,4,3,1],[])
            ]
        ],
        Node [
            Node [
                Folha ([3,2,4,1],[])
            ],
            Node [Folha ([2,3,4,1],[])]]],Node [Node [Node [Folha ([4,3,1,2],[])],Node [Folha ([3,4,1,2],[])]],Node [Node [Folha ([4,1,3,2],[])],Node [Folha ([1,4,3,2],[])]],Node [Node [Folha ([3,1,4,2],[])],Node [Folha ([1,3,4,2],[])]]],Node [Node [Node [Folha ([4,2,1,3],[])],Node [Folha ([2,4,1,3],[])]],Node [Node [Folha ([4,1,2,3],[])],Node [Folha ([1,4,2,3],[])]],Node [Node [Folha ([2,1,4,3],[])],Node [Folha ([1,2,4,3],[])]]],Node [Node [Node [Folha ([3,2,1,4],[])],Node [Folha ([2,3,1,4],[])]],Node [Node [Folha ([3,1,2,4],[])],Node [Folha ([1,3,2,4],[])]],Node [Node [Folha ([2,1,3,4],[])],Node [Folha ([1,2,3,4],[])]]]]
*Main> 
--}










{-- como funciona a coisa

-- um objeto que eu chamei de Abaco, que eh tipo um abaco
type Abaco = ([Int], [Int])

-- uma arvore do tipo rose (sem complicacoes)
data Tree a = Node a [Tree a] deriving Show

-- um no raiz, contendo um Abaco
root :: Tree Abaco
root = Node ([],[1..3]) []

-- uma forma de expandir um no, gerando varios estados desse abaco
expande :: Abaco -> Tree Abaco
expande (xs, ys) = Node ([], []) [Node (p:xs, delete p ys) [] | p <- ys]

-- cada vez que eu percorro uma arvore, eu expando suas folhas
percorre :: Tree Abaco -> Tree Abaco
percorre (Node n []) = expande n
percorre (Node _ fls) = Node ([], []) [percorre f | f <- fls] 

-- falta definir uma forma automatica de fazer isso
-- e os criterios para que cada expancao aconteca 
teste = percorre $ percorre $ percorre root

o comando acima produz >>

            Node ([],[]) [
                Node ([],[]) [
                    Node ([],[]) [
                        Node ([3,2,1],[]) []
                    ],Node ([],[]) [
                        Node ([2,3,1],[]) []
                    ]
                ],Node ([],[]) [
                    Node ([],[]) [
                        Node ([3,1,2],[]) []
                    ],Node ([],[]) [
                        Node ([1,3,2],[]) []
                    ]
                ],Node ([],[]) [
                    Node ([],[]) [
                        Node ([2,1,3],[]) []
                    ],Node ([],[]) [
                        Node ([1,2,3],[]) []
                    ]
                ]
            ]



--}





