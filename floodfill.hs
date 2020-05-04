-- 'imagem' de exemplo
imagem = [
    0,0,0,0,1,0,0,0,
    0,1,1,1,0,1,0,0,
    0,1,0,0,0,1,1,0,
    1,0,0,0,0,0,1,0,
    0,0,1,1,0,1,0,0,
    0,0,0,0,1,0,0,0,
    0,0,0,0,1,0,0,0,
    0,0,0,0,1,0,0,0 ]


    
-- algoritmo principal (recursivo)
floodfill img x y oldp
           | x < 0 || x > 7 || y < 0 || y > 7 = img
           | (getPixel img x y) /= oldp = img
           | otherwise = do
               let r0 = setPixel img x y 7 -- novacor
               let r1 = floodfill r0 (x+1) y oldp
               let r2 = floodfill r1 x (y+1) oldp
               let r3 = floodfill r2 (x-1) y oldp
               floodfill r3 x (y-1) oldp 

-- para pegar a 'cor' do pixel da imagem
getPixel img x y = head [p | (n, p) <- zip [0..] img, mapic n x y]

-- para setar a 'cor' do pixel da imagem
setPixel img x y newp = zipWith proc [0..] img
    where proc n p
                 | mapic n x y = newp
                 | otherwise = p

-- mapeia index coord
mapic n x y = (\(z, w) -> x == w && y == z) (divMod n 8)

-- parte responsavel por imprimir na tela o resultado
main = do
    putStrLn "\n Imagem original"
    putStrLn $ printImg imagem
    putStrLn "\n Imagem pintada"
    putStrLn $ printImg $ floodfill imagem 2 2 0
    where
        printImg = go where
            go [] = ""
            go m = (take 8 m >>= putPixel) ++
                   "\n" ++ (go $ drop 8 m)
        putPixel x
            | x == 1 = " # "
            | x == 7 = " 7 "
            | otherwise = " . "
