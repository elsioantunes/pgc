{--
    Este codigo demonstra o funcionamento do algoritmo
    de obtencao de permutacoes em haskell
    
    usage: teste [1..4]
--}

type Abaco = ([Int], [Int])

split :: Abaco -> Abaco
split (a, p:b) = (p:a, b)        

splits :: Abaco -> [Abaco]
splits (_, [])  = []                
splits ab = split ab : cauda ab

cauda :: Abaco -> [Abaco]
cauda (a, p:b) = [(m, p:n) | (m, n) <- splits (a, b)]

allperms :: [Int] -> [Abaco]
allperms m = go m where
    go [] = [([], m)] 
    go (_:count) = go count >>= splits

-----------------------------------------------------
teste m = foldMap (\x -> print x) $ allperms m





{--

Essa eh a explicacao de porque perms tem essa recursao doida
e porque eu preciso deixar expandido no comentario porque
eu vou esquecer (de novo) como montei isso (de novo)

Eu tenho que chamar splits que vai me gerar uma coisa assim:

*Main> splits ([], [1..4])
>> [([1],[2,3,4]),([2],[1,3,4]),([3],[1,2,4]),([4],[1,2,3])]

Repare que eu chamei splits com o parametro no formato de 'Abaco' 
(uma tupla de duas listas) porque ela vai chamar a si propria 
por intermedio de 'cauda' que na verdade eh 'splits' 
com seus heads recuperados.

Esses Abacos e a funcao splits sao o coracao do algoritmo.
Basicamente, splits pega o head de uma lista e passa pra outra.
e faz isso tantas vezes quantos forem os elementos dela. depois,
recursivamente, devolve esses heads para a iteracao anterior.
com efeito temos uma lista com n Abacos, 
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

A funcao f pega cada Abaco e devolve uma lista de Abacos. 
O concatMap pega cada lista de Abacos e junta numa nova lista.


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
