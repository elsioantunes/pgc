type Abaco = ([Int], [Int])

split :: Abaco -> Abaco
split (a, p:b) = (p:a, b)        

splits :: Abaco -> [Abaco]
splits (_, [])  = []                
splits ab = split ab : cauda ab

cauda :: Abaco -> [Abaco]
cauda (a, p:b) = [(m, p:n) | (m, n) <- splits (a, b)]

allperms :: [Int] -> [Abaco]
allperms xs = 
    go xs where
    go [] = [([], xs)] 
    go (_:count) = 
        go count >>= splits


main = 
    go (allperms [1..4]) where
    go [] = return ()
    go (x:xs) = do 
                 print x
                 go xs



