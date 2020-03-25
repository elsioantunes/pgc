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

