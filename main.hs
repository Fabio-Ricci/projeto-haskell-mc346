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

getNodes l = cleanDuplicates (map (\it -> lineToNode (words it)) (filterLines l))
    where
        filterLines ([]:xs) = []
        filterLines (x:xs) = (x :(filterLines xs))
        lineToNode [origin, _, _, _] = No {
            nome = origin,
            arestas = []
        }
        cleanDuplicates [] = []
        cleanDuplicates (x:xs) = (x:(cleanDuplicates (foldr (\it rest -> if it == x then rest else (it:rest))[] xs)))

getLinks nodes l = map (\it -> addEdgesToNode (createEdges l) it) nodes-- implementar isso
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

-- arrumar
getWaitingTimes l = (map (\it -> lineToWaitingTime (words it)) (removeAfterEmpty (removeBeforeEmpty l)))
        where         
            removeAfterEmpty ([]:xs) = [] -- problema aqui
            removeAfterEmpty (x:xs) = (x :(removeAfterEmpty xs))
            removeBeforeEmpty ([]:xs) = xs
            removeBeforeEmpty (_:xs) = (removeBeforeEmpty xs)
            lineToWaitingTime [line, time] = TempoDeEspera {
                linha=line,
                tempo= read time :: Float
            }

main = do 
    putStrLn "Hello World"
    -- contents <- readFile "in.in"
    contents <- getContents
    let l = lines contents
    let nodes = getNodes l
    let waitingTimes = getWaitingTimes l
    let graph =  Grafo { nos = getLinks nodes l}
    putStrLn (show graph)
    putStrLn (foldr (\it rest -> (show it) ++ rest) "" waitingTimes)