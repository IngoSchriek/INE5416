import qualified Data.Map.Strict as M
import Data.List (nub, transpose)

type PossibilityGrid = [[[Int]]]
type KojunAreasIdGrid = [[Int]]
type PossibleSolutions = [[[Int]]]

-- Corrigido: Baseado no grid já podado
usedValuesByRegion :: PossibilityGrid -> KojunAreasIdGrid -> M.Map Int [Int]
usedValuesByRegion grid areas =
  M.fromListWith (++) $
    concat $ zipWith rowUsedValues grid areas
  where
    rowUsedValues :: [[Int]] -> [Int] -> [(Int, [Int])]
    rowUsedValues row areaRow =
      [ (region, [v])
      | (cell, region) <- zip row areaRow
      , [v] <- [cell]
      ]

pruneUsedValues :: PossibilityGrid -> KojunAreasIdGrid -> M.Map Int [Int] -> PossibilityGrid
pruneUsedValues grid areas used =
  zipWith pruneRow grid areas
  where
    pruneRow row areaRow =
      [ if length cell == 1 then cell
        else filter (`notElem` M.findWithDefault [] region used) cell
      | (cell, region) <- zip row areaRow
      ]

reduceRowWithRegionsDecreasingOrder :: [([Int], Int)] -> [[Int]]
reduceRowWithRegionsDecreasingOrder =
    reverse . (\(_, _, _, xs) -> xs) . foldl reduceStep (10, [], Nothing, [])
    where
        reduceStep (prevMax, prevCell, prevRegion, acc) (cell, region) =
            let effectiveMax = if Just region == prevRegion then prevMax else 10
                allowed = filter (< effectiveMax) cell
                forbidden = if length prevCell == 1 then prevCell else []
                filtered = filter (`notElem` forbidden) allowed
                newMax  = if null filtered then effectiveMax else maximum filtered
            in (newMax, cell, Just region, filtered : acc)

pruneByCols :: PossibilityGrid -> KojunAreasIdGrid -> PossibilityGrid
pruneByCols grid regions =
  transpose $
    zipWith (\gCol rCol -> reduceRowWithRegionsDecreasingOrder (zip gCol rCol))
      (transpose grid)
      (transpose regions)

-- Corrigido: Padrões exaustivos para pruneRow
pruneByRows :: PossibilityGrid -> PossibilityGrid
pruneByRows = map pruneRow
  where
    pruneRow [] = []
    pruneRow [x] = [x]
    pruneRow (a:b:rest) =
      let a' = if length b == 1 then filter (/= head b) a else a
          b' = if length a == 1 then filter (/= head a) b else b
      in a' : pruneRow (b' : rest)

fillPossibilities :: PossibilityGrid -> PossibilityGrid
fillPossibilities = map (map possibility)
    where
        possibility :: [Int] -> [Int]
        possibility v = if length v == 1 then v else [1..9]

void :: PossibilityGrid -> Bool
void m = any (any null) m

search :: PossibilityGrid -> KojunAreasIdGrid -> PossibleSolutions
search m regions
    | void m              = [] -- Corrected: A void grid means no solution for this branch
    | all (all singleton) m = collapse m
    | otherwise           = concat [ search (prune m' regions) regions | m' <- putANumber m ]

-- Finds the (row, col) of the first cell with multiple possibilities
findBranchCell :: PossibilityGrid -> Maybe (Int, Int)
findBranchCell grid = go 0 grid
  where
    go _ [] = Nothing
    go r (row:rows) =
      case lookup True (zip (map (not . singleton) row) [0..]) of
        Just c  -> Just (r, c)
        Nothing -> go (r+1) rows

-- Replaces a cell at a specific (row, col)
replaceCell :: (Int, Int) -> [Int] -> PossibilityGrid -> PossibilityGrid
replaceCell (r, c) newCell grid =
  take r grid ++
  [take c (grid !! r) ++ [newCell] ++ drop (c+1) (grid !! r)] ++
  drop (r+1) grid

-- Generates a list of all possible next grids to check
putANumber :: PossibilityGrid -> [PossibilityGrid]
putANumber grid =
  case findBranchCell grid of
    Nothing -> [] -- No cell to branch on
    Just (r, c) ->
      let possibilities = grid !! r !! c
      in [replaceCell (r, c) [p] grid | p <- possibilities]

    


singleton :: [Int] -> Bool
singleton [_] = True
singleton _ = False

-- Corrigido: Ordem de poda e cálculo de regiões usadas
prune :: PossibilityGrid -> KojunAreasIdGrid -> PossibilityGrid
prune grid regions =
  let prunedOnce = pruneByRows grid
      prunedTwice = pruneByCols prunedOnce regions
      regionUsed = usedValuesByRegion prunedTwice regions
  in pruneUsedValues prunedTwice regions regionUsed

collapse :: PossibilityGrid -> PossibleSolutions
collapse m = [map (map head) m] -- Simply extract the value from each singleton cell

printGrid :: PossibilityGrid -> IO ()
printGrid grid = mapM_ (putStrLn . showRow) grid
  where
    showRow = unwords . map showCell
    showCell x = show x
    ---showCell (x:xs) = show x


--- Example 36 Janko At
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

-- The type signature is now correct, as search returns PossibleSolutions
solve :: PossibilityGrid -> KojunAreasIdGrid -> PossibleSolutions
solve rawGrid areas = search (prune (fillPossibilities rawGrid) areas) areas

-- Corrected main to handle a list of solutions
main :: IO ()
main = do
    let solutions = solve kojunGridExample kojunAreasIdGridExample
    case solutions of
      [] -> putStrLn "No solution found."
      (firstSolution:_) -> do
        putStrLn "Found solution:"
        -- We need a printer for the solved grid format [[Int]]
        -- For now, we can just use the standard show
        print firstSolution