import System.IO
import System.Environment     


-- entrada:

-- a b a-pe 0.4 (origem, destino, metodo, tempo)
-- b a a-pe 0.6 (arestas direcionais, b para a demora mais que a para b)
-- b c a-pe 0.5 
-- c b a-pe 0.5
-- c d a-pe 0.3
-- d c a-pe 0.3
-- a d linha-370 0.1
-- d f a-pe 3
-- f h linha-567 1.2
-- f h a-pe 12.3
-- (linha em branco separa o grafo das informações das linhas)
-- linha-370 15.0 (linha, e periodo de passagem do onibus na linha)
-- linha-567 12.0 (custo de esperar o onibus é fixo e metade do periodo (6.00 no caso))
-- (linha em branco separa as linhas da origem/destino)
-- a h (origem, destino)

-- saída:
-- a a-pe b a-pe c linha-370 f a-pe h a-pe i (caminho)
-- 17.5 (tempo total)

tostr l = foldr (\x y -> if x == [] then "vazio/" ++ y else x ++ "/" ++ y) "" l
nodestostr l = foldr (\(origin, destiny, method, length) y ->
     "origin: " ++ origin ++ 
     " destiny: " ++ destiny ++ 
     " method: " ++ method ++ 
     " length: " ++ length ++ "\n" ++ y) "" l

getnodes l = map (\it -> linetonode (words it)) (filterlines l)
    where
        filterlines ([]:xs) = []
        filterlines (x:xs) = (x :(filterlines xs))
        linetonode [origin, destiny, method, length] = (origin, destiny, method, length)

main = do 
    putStrLn "Hello World"
    contents <- getContents
    let l = lines contents
    let nodes = getnodes l
    putStrLn (nodestostr nodes)