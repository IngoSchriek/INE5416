import qualified Data.Map.Strict as M
import Data.List (nub, transpose)

-- Cada elemento dessa matriz representa as possibilidades de valores para a célula (i,j) correspondente.
-- Por exemplo, [[2, 4, 5], [2], []] significa que a primeira célula pode ser 2, 4 e 5, a segunda só pode ser 2 e a terceira significa que não há solução.
type PossibilityGrid = [[[Int]]]

-- Cada elemento dessa matriz representa a região Kojun à qual a célula (i,j) correspondente pertence.
type KojunAreasIdGrid = [[Int]]

-- A solução possível é uma lista de matrizes, onde cada matriz é uma possível solução completa do Kojun.
type PossibleSolutions = [[[Int]]]


usedValuesByRegion :: PossibilityGrid -> KojunAreasIdGrid -> M.Map Int [Int]
usedValuesByRegion grid areas =
  -- Gera um mapa onde a chave é o ID da região e o valor é uma lista de valores usados nessa região.
  M.fromListWith (++) $
    concat $ zipWith rowUsedValues grid areas
  -- Para cada linha da grade de possibilidades e da grade de areas...
  where
    rowUsedValues :: [[Int]] -> [Int] -> [(Int, [Int])]
    rowUsedValues row areaRow =
      [ (region, [v])
      | (cell, region) <- zip row areaRow -- Para cada célula (i,j) e região (i,j) correspondente...
      , [v] <- [cell] -- Se a célula tem exatamente um valor possível é adicionada ao mapa com o ID da região correspondente como chave.
      ]

-- Remove da lista de valores possíveis aqueles que já foram utilizados na região correspondente.
-- Utiliza o mapa de valores usados por região gerado anteriormente.
pruneUsedValues :: PossibilityGrid -> KojunAreasIdGrid -> M.Map Int [Int] -> PossibilityGrid
pruneUsedValues grid areas used =
  zipWith pruneRow grid areas
  where
    pruneRow row areaRow =
      [ if length cell == 1 then cell
        else filter (`notElem` M.findWithDefault [] region used) cell -- Gera uma lista de valores proibidos e a utiliza para filtrar os valores possíveis.
      | (cell, region) <- zip row areaRow
      ]

-- Para fins de perfomance, essa função reduz uma coluna de acordos com as regras do Kojun: 
-- * adjacência vertical (independente da região) 
-- * em ordem decrescente na coluna (dependente da regiao).
reduceListWithRegionsDecreasingOrder :: [([Int], Int)] -> [[Int]]
reduceListWithRegionsDecreasingOrder =
    reverse . (\(_, _, _, xs) -> xs) . foldl reduceStep (10, [], Nothing, [])
    where
      -- Processa uma coluna de células de cima para baixo, 
      -- mantendo um "estado" que contém informações sobre a célula anterior.
        reduceStep (prevMax, prevCell, prevRegion, acc) (cell, region) =

          -- O máximo permitido para a célula atual é o é o teto máximo que foi definido pela célula anterior,
          -- exceto se a região não for a mesma da célula anterior.
            let effectiveMax = if Just region == prevRegion then prevMax else 10

            -- Não pode existir valores possíveis para celula atual maiores que o máximo da celula anterior pois
            -- estes nunca conseguiriam ser menores do que qualquer valor possível da celula anterior.
                allowed = filter (< effectiveMax) cell

            -- Células verticalmente adjacentes não podem ter o mesmo valor, então se a célula anterior tiver apenas um valor possível,
            -- este não pode ser o mesmo da célula atual.
                forbidden = if length prevCell == 1 then prevCell else []

            -- Dos valores permitidos pela ordem decrescente, remove aquele que já foi usado na célula anterior.
                filtered = filter (`notElem` forbidden) allowed

            -- Calcula o novo máximo, passado para próxima iteração.
                newMax  = if null filtered then effectiveMax else maximum filtered
            in (newMax, cell, Just region, filtered : acc)

-- Transpos a a matriz para processar as colunas como linhas (listas).
pruneByCols :: PossibilityGrid -> KojunAreasIdGrid -> PossibilityGrid
pruneByCols grid regions =
  transpose $ 
    zipWith (\gCol rCol -> reduceListWithRegionsDecreasingOrder (zip gCol rCol))
      (transpose grid)
      (transpose regions)

-- Quebra em casos: sem possibilidades, uma possibilidade ou mais de uma possibilidade.
-- Se a célula tiver mais de uma possibilidade, para cada par de células vizinhas, se uma delas estiver resolvida
-- (for um singleton), remove seu valor das possibilidades da outra.
pruneByRows :: PossibilityGrid -> PossibilityGrid
pruneByRows = map (pruneRowBidirectional)
  where
    pruneRow' [] = []
    pruneRow' [x] = [x]
    pruneRow' (a:b:rest) =
      let a' = if length b == 1 then filter (/= head b) a else a
          b' = if length a == 1 then filter (/= head a) b else b
      in a' : pruneRow' (b' : rest)
    pruneRowBidirectional :: [[Int]] -> [[Int]] -- se a poda de b em a abre uma nova possibilidade para b, essa informação não voltaria para a sem ser bidirecional
    pruneRowBidirectional row = reverse (pruneRow' (reverse (pruneRow' row)))

-- Prepara o tabuleiro inicial. Para cada célula vazia, preenche com as possibilidades
-- corretas [1..N], onde N é o tamanho da região da célula.
fillPossibilities :: PossibilityGrid -> KojunAreasIdGrid -> PossibilityGrid
fillPossibilities grid areas =
  let
    regionSizes = calculateRegionSizes areas
  in
    -- Percorre o tabuleiro e as áreas linha por linha.
    zipWith (fillRow regionSizes) grid areas
  where
    -- Preenche cada linha do tabuleiro com as possibilidades corretas.
    fillRow sizes possibilityRow areaRow = zipWith (fillCell sizes) possibilityRow areaRow
    fillCell sizes cell regionId =
      if length cell == 1 -- Se a célula já estiver resolvida a mantém
        then cell
        else
          let maxSize = M.findWithDefault 0 regionId sizes
          in [1..maxSize]

-- Verifica se o tabuleiro se tornou inválido
void :: PossibilityGrid -> Bool
void m = any (any null) m

-- Busca por soluções possíveis no tabuleiro:
-- * Se o tabuleiro nao houver solucao, retorna uma lista vazia.
-- * Se todas as células tiverem apenas uma possibilidade, colapsa o tabuleiro e retorna a solução.
-- * Caso contrário, gera novas possibilidades fazendo uma "guess"
search :: PossibilityGrid -> KojunAreasIdGrid -> PossibleSolutions
search m regions
    | void m              = [] 
    | all (all singleton) m = collapse m
    | otherwise           = concat [ search (prune m' regions) regions | m' <- putANumber m ]

-- Calcula o tamanho de cada região contando as ocorrências de cada ID no grid de áreas.
-- Retorna um mapa onde a chave é o ID da região e o valor é seu tamanho.
calculateRegionSizes :: KojunAreasIdGrid -> M.Map Int Int
calculateRegionSizes areas =
    -- M.fromListWith (+) cria um mapa a partir de uma lista de pares (chave, valor).
    -- Se uma chave aparece múltiplas vezes, ele soma os valores.
    M.fromListWith (+) $
    map (\regionId -> (regionId, 1)) $
    -- concat achata a matriz de áreas em uma única lista de IDs.
    concat areas

-- Encontra as coordenadas (linha, coluna) da primeira célula que ainda não está resolvida (que tem mais de uma possibilidade)
findBranchCell :: PossibilityGrid -> Maybe (Int, Int)
findBranchCell grid = go 0 grid
  where
    go _ [] = Nothing
    go r (row:rows) =
      case lookup True (zip (map (not . singleton) row) [0..]) of
        Just c  -> Just (r, c)
        Nothing -> go (r+1) rows

-- Função utilitária para criar uma cópia do tabuleiro com uma única célula substituída.
replaceCell :: (Int, Int) -> [Int] -> PossibilityGrid -> PossibilityGrid
replaceCell (r, c) newCell grid =
  take r grid ++ -- pega todas as linhas até a linha r-1 (PREFIXO) 
  [take c (grid !! r) ++ [newCell] ++ drop (c+1) (grid !! r)] ++ -- linha modificada
  drop (r+1) grid -- descarta as duas primeiras linhas e pega o resto (SUFIXO)

-- Gera a "ramificação" da busca. Encontra uma célula não resolvida e cria uma lista
-- de novos tabuleiros, um para cada possibilidade daquela célula. É a função de "chute".
putANumber :: PossibilityGrid -> [PossibilityGrid]
putANumber grid =
  case findBranchCell grid of
    Nothing -> []
    Just (r, c) ->
      let possibilities = grid !! r !! c -- Obtém as possibilidades da célula (r, c)
      in [replaceCell (r, c) [p] grid | p <- possibilities] -- Para cada possibilidade, cria um novo tabuleiro substituindo a célula (r, c) por essa possibilidade.

-- Verifica se lista é um "singleton"
singleton :: [Int] -> Bool
singleton [_] = True
singleton _ = False

-- Passa matriz a limpo aplicando as regras do Kojun, elimando valores impossíveis
prune :: PossibilityGrid -> KojunAreasIdGrid -> PossibilityGrid
prune grid regions =
  let
    -- Executa uma única passagem completa de poda.
    pruneOnce g =
      let prunedRows = pruneByRows g
          prunedCols = pruneByCols prunedRows regions
          regionUsed = usedValuesByRegion prunedCols regions
      in pruneUsedValues prunedCols regions regionUsed

    -- Aplica a poda recursivamente até que a grade de entrada seja igual à de saída.
    fixpointPrune currentGrid =
      let nextGrid = pruneOnce currentGrid
      in if currentGrid == nextGrid
         then currentGrid -- Ponto fixo atingido!
         else fixpointPrune nextGrid -- Continua a podar.
         
  in fixpointPrune grid


-- Colapsa a matriz de possibilidades em uma solução única, assumindo que todas as células têm apenas uma possibilidade.
collapse :: PossibilityGrid -> PossibleSolutions
collapse m = [map (map head) m] 

--- Exemplo 36 Janko At
kojunGridExample :: PossibilityGrid
kojunGridExample =
    [[[5], [], [1], [6], [5], [], [4], [], [3], [5]]
    , [[3], [], [], [], [], [6], [], [], [], []]
    , [[6], [], [5], [], [2], [], [1], [], [], [1]]
    , [[], [3], [2], [], [3], [], [], [], [1], []]
    , [[6], [7], [], [4], [6], [], [], [4], [], [5]]
    , [[3], [], [4], [], [3], [], [], [], [], [4]]
    , [[], [1], [], [], [], [], [], [1], [], []]
    , [[5], [], [], [], [1], [], [], [], [], [7]]
    , [[], [], [5], [3], [], [], [6], [3], [], [4]]
    , [[], [2], [1], [], [], [5], [2], [], [], []]
    ]

-- Cada posicao (i,j) representa o ID da região Kojun à qual a célula (i,j) correspondente pertence.
kojunAreasIdGridExample :: KojunAreasIdGrid
kojunAreasIdGridExample =
    [ [0, 0, 1, 1, 1, 1, 2, 3, 3, 3]
    , [0, 0, 0, 1, 4, 2, 2, 3, 3, 3]
    , [5, 5, 5, 1, 1, 2, 2, 2, 6, 3]
    , [5, 5, 5, 7, 8, 8, 8, 8, 6, 6]
    , [7, 7, 7, 7, 9, 9, 9, 8, 10, 6]
    , [7, 7, 11, 9, 9, 12, 12, 10, 10, 6]
    , [13, 13, 11, 14, 9, 15, 15, 15, 10, 6]
    , [11, 11, 11, 14, 14, 16, 17, 17, 10, 10]
    , [18, 11, 19, 19, 19, 16, 20, 20, 20, 10]
    , [18, 19, 19, 21, 21, 20, 20, 20, 20, 22]
    ]

solve :: PossibilityGrid -> KojunAreasIdGrid -> PossibleSolutions
solve rawGrid areas =
    let initialGrid = fillPossibilities rawGrid areas
        prunedGrid = prune initialGrid areas
    in search prunedGrid areas

main :: IO ()
main = do
    let solutions = solve kojunGridExample kojunAreasIdGridExample
    case solutions of -- De todas solucoes encontradas, pega a primeira.
      [] -> putStrLn "No solution found."
      (firstSolution:_) -> do
        putStrLn "Found solution:"
        print firstSolution