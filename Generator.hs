module Generator (
  Sudoku,
  emptySudoku,
  generateSudoku,
  generatePuzzle,
  isSolved,
  isValid,
  put
) where

import System.Random (randomRIO)

type Sudoku = [[Int]]

emptySudoku :: Sudoku
emptySudoku = replicate 9 (replicate 9 0)

generateSudoku :: IO Sudoku
generateSudoku = fillSudoku emptySudoku 0 0

fillSudoku :: Sudoku -> Int -> Int -> IO Sudoku
fillSudoku sudoku row col
  | row == 9 = return sudoku
  | col == 9 = fillSudoku sudoku (row + 1) 0
  | (sudoku !! row) !! col /= 0 =
      fillSudoku sudoku row (col + 1)
  | otherwise = do
      nums <- shuffle [1..9]
      tryNumbers nums sudoku row col

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  idx <- randomRIO (0, length xs - 1)
  let (left, (y:right)) = splitAt idx xs
  rest <- shuffle (left ++ right)
  return (y : rest)

tryNumbers :: [Int] -> Sudoku -> Int -> Int -> IO Sudoku
tryNumbers [] sudoku _ _ = return sudoku
tryNumbers (n:ns) sudoku row col
  | isValid sudoku row col n = do
      let newSudoku = put sudoku row col n
      result <- fillSudoku newSudoku row (col + 1)
      if isSolved result
        then return result
        else tryNumbers ns sudoku row col
  | otherwise =
      tryNumbers ns sudoku row col


generatePuzzle :: Sudoku -> Int -> IO Sudoku
generatePuzzle sudoku holes = removeValues sudoku holes

removeValues :: Sudoku -> Int -> IO Sudoku
removeValues sudoku 0 = return sudoku
removeValues sudoku n = do
  row <- randomRIO (0, 8)
  col <- randomRIO (0, 8)
  if (sudoku !! row) !! col == 0
    then removeValues sudoku n
    else do
      let newSudoku = put sudoku row col 0
      removeValues newSudoku (n - 1)

put :: Sudoku -> Int -> Int -> Int -> Sudoku
put sudoku row col n =
   take row sudoku ++
  [ take col (sudoku !! row) ++ [n] ++ drop (col + 1) (sudoku !! row) ] ++
  drop (row + 1) sudoku

rowValues :: Sudoku -> Int -> [Int]
rowValues sudoku row = sudoku !! row

colValues :: Sudoku -> Int -> [Int]
colValues sudoku col = map (!! col) sudoku

getBox :: Sudoku -> Int -> Int -> [Int]
getBox sudoku row col =
  [ sudoku !! r !! c
  | r <- [boxRowStart .. boxRowStart + 2]
  , c <- [boxColStart .. boxColStart + 2]
  ]
  where
    boxRowStart = (row `div` 3) * 3
    boxColStart = (col `div` 3) * 3

isValid :: Sudoku -> Int -> Int -> Int -> Bool
isValid sudoku row col n =
  n /= 0 &&
  notElem n (rowValues sudoku row) &&
  notElem n (colValues sudoku col) &&
  notElem n (getBox sudoku row col)

isSolved :: Sudoku -> Bool
isSolved sudoku = all (all (/= 0)) sudoku




