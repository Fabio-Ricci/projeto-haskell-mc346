import System.IO
import System.Environment     
import Data.List


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
data Aresta = Aresta {
    origem::String,
    destino::String,
    -- origem::No,
    -- destino::No,
    metodo::String,
    peso:: Float -- mudar para int
} deriving (Eq,Show,Read)

data No = No {
    nome::String,
    arestas::[Aresta]
} deriving (Eq,Show,Read)

data Grafo = Grafo {
    nos::[No]
} deriving (Eq,Show,Read)

data TempoDeEspera = TempoDeEspera {
    tempo::Float,
    linha::String
} deriving (Eq,Show,Read)

getNodes l = cleanDuplicates (foldr (\it rest -> it ++ rest) [] (map (\it -> lineToNode (words it)) (filterLines l)))
    where
        filterLines ([]:xs) = []
        filterLines (x:xs) = (x :(filterLines xs))
        lineToNode [origin, destination, _, _] = [
            No {
                nome = origin,
                arestas = []
            },
            No {
                nome = destination,
                arestas = []
            }]
        cleanDuplicates [] = []
        cleanDuplicates (x:xs) = (x:(cleanDuplicates (foldr (\it rest -> if it == x then rest else (it:rest))[] xs)))

getLinks nodes l = map (\it -> addEdgesToNode (createEdges l) it) nodes
    where
        createEdges l = (map (\it -> lineToEdge (words it)) (filterLines l))
        filterLines ([]:xs) = []
        filterLines (x:xs) = (x :(filterLines xs))
        addEdgesToNode edges node = foldl (\n it -> addEdgeToNode it n) node edges
        addEdgeToNode (Aresta {origem = origin, destino = destination, metodo = method, peso = length}) (No {nome = nome, arestas = arestas})
          | nome == origin = No {
            nome = nome,
            arestas = (Aresta {
              origem = origin,
              destino = destination,
              metodo = method, 
              peso = length
            }:arestas)
          }
          | otherwise = No {nome = nome, arestas = arestas}  
        lineToEdge [origin, destination, method, length] = Aresta {
            origem = origin,
            destino = destination,
            metodo = method, 
            peso = read length :: Float
        }

getWaitingTimes l = (map (\it -> lineToWaitingTime (words it)) (removeAfterEmpty (removeBeforeEmpty l)))
        where         
            removeAfterEmpty ([]:xs) = [] 
            removeAfterEmpty (x:xs) = (x :(removeAfterEmpty xs))
            removeBeforeEmpty ([]:xs) = xs
            removeBeforeEmpty (_:xs) = (removeBeforeEmpty xs)
            lineToWaitingTime [line, time] = TempoDeEspera {
                linha=line,
                tempo= read time :: Float
            }

getOriginDestination l = lineToOriginDestination (words (head (removeBeforeEmpty (removeBeforeEmpty l))))
            where
                removeBeforeEmpty :: [String] -> [String]
                removeBeforeEmpty ([]:xs) = xs
                removeBeforeEmpty (_:xs) = (removeBeforeEmpty xs)
                lineToOriginDestination [origin, destination] = (origin, destination)

getNodeByName nodes name = foldr (\it rest -> if name == (nome it) then it else rest) No{nome="not found", arestas=[]} nodes

notPassed :: [No] -> No -> Bool
notPassed passedNodes node = foldr (\it rest -> if it == node then False else rest) True passedNodes

filteredLinks :: [No] -> [Aresta] -> [No] -> [Aresta]
filteredLinks _ [] _ = []
filteredLinks nodes (x:xs) passed =  if (notPassed passed (getNodeByName nodes (origem x))) then (x:(filteredLinks nodes xs passed)) else (filteredLinks nodes xs passed)

flatten :: [[[Aresta]]] -> [[Aresta]]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs)

getPossiblePaths :: Grafo -> String -> String -> [[Aresta]]
getPossiblePaths Grafo { nos=nos } origin destination = getPossiblePaths' nos (getNodeByName nos origin) destination []
            where
                addOrigin :: Aresta -> [[Aresta]] -> [[Aresta]]
                addOrigin link paths = map (\path -> (link:path)) paths 
                getPossiblePaths' :: [No] -> No -> String -> [No] -> [[Aresta]]
                getPossiblePaths' nodes origin destination passed
                    | (nome origin) == destination = [[]]
                    | flinks == [] = []
                    | otherwise = flatten (
                        map (\it -> 
                            let p = (getPossiblePaths' nodes (getNodeByName nodes (destino it)) destination (origin:passed)) in
                            addOrigin it p
                            ) flinks
                    )
                    where 
                        flinks = (filteredLinks nodes (arestas origin) passed)

getTotalTime :: [Aresta] -> Float
getTotalTime [] = 0.0
getTotalTime [(Aresta {origem = origin, destino = destination, metodo = method, peso = length})] = if (isInfixOf "linha" method) then (1.5*length) else length  
getTotalTime ((Aresta {origem = origin, destino = destination, metodo = method, peso = length}):arestas) = length + (getTotalTime' method arestas)

getTotalTime' :: String -> [Aresta] -> Float
getTotalTime' _ [] = 0.0
getTotalTime' ant ((Aresta {origem = origin, destino = destination, metodo = method, peso = length}):arestas)
    | (isInfixOf "linha" method) && (ant /= method) = (1.5*length) + (getTotalTime' method arestas)
    | otherwise = length + (getTotalTime' method arestas)
  
getShortestTime :: [[Aresta]] -> Float
getShortestTime (path:paths) = foldl (\acc it -> if (getTotalTime it) < acc then (getTotalTime it) else acc) (getTotalTime path) paths

getShortestPath :: [[Aresta]] -> [Aresta]
getShortestPath (path:paths) = foldl (\acc it -> if (getTotalTime it) < (getTotalTime acc) then it else acc) path paths

pathToString :: [Aresta] -> String
pathToString path = (origem (head path)) ++ " " ++ foldr(\Aresta {destino=destino, metodo=metodo} rest -> metodo ++ " " ++ destino ++ " " ++ rest) "" path

main = do 
    putStrLn "Hello World"
    -- contents <- readFile "in.in"
    contents <- getContents
    let l = lines contents
    let nodes = getNodes l
    let waitingTimes = getWaitingTimes l
    let graph =  Grafo { nos = getLinks nodes l}
    putStrLn (show graph)
    --putStrLn (foldr (\it rest -> (show it) ++ rest) "" waitingTimes)
    --putStrLn (show (getNodeByName (nos graph) "a"))
    --putStrLn (show (filteredLinks (nos graph) (arestas (getNodeByName (nos graph) "a")) []))
    let originDestination = getOriginDestination l
    let possiblePaths = getPossiblePaths graph (fst originDestination) (snd originDestination)
    let shortestTime = getShortestTime possiblePaths
    let shortestPath = getShortestPath possiblePaths
    putStrLn (show originDestination)
    putStrLn (show possiblePaths)
    putStrLn (pathToString shortestPath)
    putStrLn (show shortestTime)