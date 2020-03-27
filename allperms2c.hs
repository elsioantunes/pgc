{--
    Este codigo tem como objetivo mostrar o funcionamento dos predicados.
    
    Predicados, nesse contexto, sao funcoes que recebem uma lista e retornam um Bool
    com a intencao de limitar a construcao da arvore de permutacoes, de modo a sobreviver
    apenas as ramificacoes condizentes com a regra problema. 
    
    O uso de uma arvore ao invez de listas de permutacoes economiza espaco na memoria,
    que agora nao precisa guardar os primeiros elementos comuns a varias permutacoes,
    alem de outras vantagens ainda nao exploradas por este codigo.
    
    Na versao anterior do codigo, allperms2b eu modifiquei a funcao perms para que 
    funcionasse com um predicado, acrescentando uma pilha para acumular os valores 
    registrados em niveis superiores da arvore. 
    
    Neste codigo eu dou mais detalhes de como o predicado recebe os valores da arvore e
    como ele responde aos valores da pilha para que ramificacoes ilegais nao sejam criadas.

--}

import System.Process
data Arv a = No a [Arv a]
type Predicado a = [a] -> Bool
type Arvore a = Arv (Maybe a)

splits :: [a] -> [[a]]
splits [] = []
splits ab = ab : cauda ab
cauda (x:xs) = [(a:x:b) | (a:b) <- splits xs]

{--
    'perms' eh recursivo por intermedio de 'go'
    necessario para adm os parametros 'regra' e 'pilha'
--}

perms :: (Predicado a) -> [a] -> Arvore a
perms regra  = No Nothing . go [] where
    go _ [] = []
    go pilha m = f <$> splits m where
        f (n:ns)
                | regra (n:pilha) =  No (Just n) $ go (n:pilha) ns
                | otherwise = No Nothing []


{-- 
    ftree: processo de folding que traz os dados da arvore
    novamente em formato de listas para apresentacao dos resultados.
    Nesta versao a busca pela solucao nao eh feita nas listas e sim na arvore.
--}

ftree :: Arvore a -> [[a]]
ftree (No Nothing ns) = concat [ftree n | n <- ns]
ftree (No (Just a) []) = [[a]]
ftree (No (Just a) ns) = [a:n | n <- ftree (No Nothing ns)]

{-- predicado magic3x3 square ------------------------------------------------------
    
    Nas primeiras versoes, a validacao de uma permutacao como solucao do problema
    era feita diretamente com a permutacao pronta. Constatou-se ser mais eficiente
    que essa avaliacao fosse feita enquanto a permutacao estivesse sendo montada.
    
    Para isso, o predicado retornaria um booleano a cada chegada de um item a lista.
    Quando as permutacoes passaram a nao serem mais registradas em lista e sim em 
    arvore, surgiu um problema: era necessario acumular cada item recebido da arvore
    enquanto ela fosse montada. Implementei entao uma pilha que fazia esse acumulo
    dentro do predicado. Nesta versao, esta pilha foi movida diretamente para a 
    funcao perms, montadora da arvore, pois faz parte do algoritmo generico o processo
    de construcao da arvore, que difere do predicado que eh especifico do problema.

--}

-- para cada item recebido
magic3x3 :: Predicado Int
magic3x3 q = go q where 
    
    {---------------------------

      Caso inicial.
      se a lista estiver vazia
      autoriza o primeiro p

    ---------------------------}
    
    go [] = True

    {---------------------------
      
      Regras do problema.
      somas de linhas, colunas
      e diagonais
      
    ---------------------------}
    
    go (p:xs@[a,b]) -- 1a linha
        | a+b+p /= 15 = False
        | otherwise = go xs
    
    go (p:xs@[a,b,c,d,e])  -- 2a linha 
        | a+b+p /= 15 = False
        | otherwise = go xs

    go (p:xs@[a,b,c,d,e,f]) 
        | p+b+d /= 15 = False -- diagonal /
        | p+c+f /= 15 = False -- coluna 1
        | otherwise = go xs

    go (p:xs@[a,b,c,d,e,f,g]) 
        | p+c+f /= 15 = False -- coluna 2
        | otherwise = go xs

    go (p:xs@[a,b,c,d,e,f,g,h]) 
        | p+d+h /= 15 = False -- diagonal \
        | otherwise = go xs


    {---------------------------
      
      Caso final.
      permite dar o passo nos
      niveis da arvore.      
      
    ---------------------------}
    
    go (p:xs) = go xs

{-- predicado magic3x3 square ------------------------------------------------------
    
    Dessa forma, o predicado tem uma (ou mais) resposta para cada numero de itens
    da lista recebida. Para o tamanho de lista que nao ha resposta definida, a 
    ultima linha avanca mais um nivel da arvore ate que o ultimo item seja consu-
    mido, autorizando automaticamente toda a lista. Se no meio da lista surgir 
    algum motivo para interrompe-la, interrompe-se toda aquela cadeia ate o noh
    mais proximo, como num backtraking. 

--}
















{---------------------------------------------------------------------------
    Codigo necessario para a presentacao de resultados.
    separadamente do algoritmo, este trecho apenas apresenta
    o layout de duas solucoes do problema square magic 3x3
---------------------------------------------------------------------------}
main = do
         system "clear"
         let nSolucoes = 2
         let solucoes =  ftree $perms magic3x3 [1..9]
         putStr "Numero de solucoes encontradas: "
         print $ length solucoes
         
         putStrLn "Apresentando duas delas:\n"
         
         foldMap printSquare $ take nSolucoes $ solucoes

         where
            printSquare m = go m 0 where
                   go [] _ = putStrLn " |----|----|----|\n"
                   go (a:b:c:xs) k = do 
                           putStrLn " |----|----|----|"
                           printCell [a, b, c]
                           putStrLn $ "  " ++ (show $ sum [a, b, c])
                           go xs (k+1)
                           
                           foot k m
            
            foot 2 m = printFoot $ head $ sumCol m
            foot _ _ = putStrLn ""
            
            printFoot (a,b,c) = putStrLn $ foldMap (("   " ++) . show) [a,b,c]

            sumCol [] = []
            sumCol (a:b:c:rest) = [(a+e, b+f, c+g)|(e, f, g) <- sumCol rest] ++ [(a, b, c)]

printCell [] = putStr " |"
printCell (x:xs) = do
                    putStr $ justify x
                    printCell xs
justify x
         | x > 9 = " | "  ++ show x
         | otherwise = " |  "  ++ show x






{-------------------------------------------------------------------------------------
    Caso for interessante visualizar a arvore, essa instancia de show permite
    organizar a arvore de forma grafica, apesar de longa, pode ser usada para 
    imprimir num arquivo, por ex. Futuramente o mecanismo sera utilizado para
    imprimir um diagrama que mostre de forma mais eficiente essa longa arvore.
-------------------------------------------------------------------------------------}
instance Show a => Show (Arv a) where
    show m = "\n\n" ++ (unlines $ go  m) ++ "\n\n"
      where
        go (No a ns) =  lines ("(" ++ show a ++ ")") ++ subtree ns
        subtree [] = []
        subtree [n] = " \x2502" : " \x2502" : (zipWith (++) (" \x2514\x2500" : repeat "    ") (go n))
        subtree (n:ns) = " \x2502" : " \x2502" : (zipWith (++) (" \x251c\x2500" : repeat " \x2502  ") (go n)) ++ subtree ns
