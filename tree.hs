import System.Process
import Data.Ord
-- import Data.List

type Abaco = ([Int], [Int])
data Tree a = Folha a | Node [Tree a]  deriving Show


delete1 _ [] = []
delete1 p (y:ys)
               | p == y = ys
               | otherwise = y:(delete1 p ys)

delete p ys = [y | y <- ys, y /= p]



gmap :: Tree Abaco ->  Tree Abaco       
gmap arv = case arv of 
    (Folha x) -> Node $ f x 
    (Node ns) -> Node [gmap n | n <- ns]
    where
        f (xs, ys) = [Folha $ sol xs ys p | p <- ys, cond xs p]        
        sol xs ys p = (p:xs, delete p ys)
        
        cond xs p = and [p /= c + n  && p /= c - n | (c, n) <- zip xs [1..]]



-- Para converter a arvore de solucoes em uma lista deles

toList :: Tree Abaco -> [[Int]]
toList (Folha x) = [fst x]
toList (Node []) = []
toList (Node (n:ns)) =  toList n ++ (toList $ Node ns)



-- tentei com foldl, com iterate e agora com cauda 
-- (deu no mesmo. assim pelo menos fica um código mais claro)
-- aplicar gmap a uma arvore, expande-lhe suas folhas. 
-- temos que aplicar n vezes, onde n eh o tamanho do tabuleiro

arv :: Int -> Tree Abaco
arv n = arv' n  
    where
        arv' 0 = Folha ([], [1..n])
        arv' x = gmap $ arv' $ x-1 



-- Um teste de velocidade 
-- listando o numero de solucoes 
-- pra cada tamanho de tabuleiro 

teste = [length $ toList $ arv n | n <- [0..11]]









{--

>> :set +s
>> teste
[1,1,0,0,2,10,4,40,92,352,724,2680] -- com foldl
(10.51 secs, 2,256,783,552 bytes)

[1,1,0,0,2,10,4,40,92,352,724,2680] -- com iterate
(10.74 secs, 2,245,790,944 bytes)

[1,1,0,0,2,10,4,40,92,352,724,2680] -- com cauda
(14.10 secs, 2,704,378,168 bytes)


[6,1,5,2,8,3,7,4]


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

-- um objeto que eu chamei de coisa, que eh tipo um abaco
type Coisa = ([Int], [Int])

-- uma arvore do tipo rose (sem complicacoes)
data Tree a = Node a [Tree a] deriving Show

-- um no raiz, contendo uma coisa
root :: Tree Coisa
root = Node ([],[1..3]) []

-- uma forma de expandir um no, gerando varios estados desse abaco
expande :: Coisa -> Tree Coisa
expande (xs, ys) = Node ([], []) [Node (p:xs, delete p ys) [] | p <- ys]

-- cada vez que eu percorro uma arvore, eu expando suas folhas
percorre :: Tree Coisa -> Tree Coisa
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








{--

data Arv a = Node a (Arv a) (Arv a) | Folha deriving Show

instance Functor Arv where
    fmap _ Folha = Folha
    fmap f (Node x e d) = Node (f x) (fmap f e) (fmap f d)

arv = Node 1 (Node 2 (Node 4 Folha (Node 2 Folha Folha)) Folha) (Node 3 (Node 2 Folha Folha) Folha)

testeF = fmap (*2) arv

--}
