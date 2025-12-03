module Solver (
  solve
) where

import Generator (Sudoku, isValid, put)

-- Ein eindeutig lösbares Sudoku-Rätsel
sudokuExample :: Sudoku
sudokuExample =
--  c0 c1
  [ [5, 3, 0, 0, 7, 0, 0, 0, 0] -- r0
  , [6, 0, 0, 1, 9, 5, 0, 0, 0] -- r1
  , [0, 9, 8, 0, 0, 0, 0, 6, 0]
  , [8, 0, 0, 0, 6, 0, 0, 0, 3]
  , [4, 0, 0, 8, 0, 3, 0, 0, 1]
  , [7, 0, 0, 0, 2, 0, 0, 0, 6]
  , [0, 6, 0, 0, 0, 0, 2, 8, 0]
  , [0, 0, 0, 4, 1, 9, 0, 0, 5]
  , [0, 0, 0, 0, 8, 0, 0, 7, 9]
  ]

-- Startfunktion: versucht das gegebene Sudoku zu loesen
solve :: Sudoku -> (Sudoku, Bool)
solve sudoku = solveAt sudoku 0 0

-- Laeuft Zelle fuer Zelle durchs Sudoku und fuellt leere Felder
solveAt :: Sudoku -> Int -> Int -> (Sudoku, Bool)
solveAt sudoku row col
  | row == 9 = (sudoku, True)                  
  | col == 9 = solveAt sudoku (row + 1) 0      
  | (sudoku !! row) !! col /= 0 =  solveAt sudoku row (col + 1)
  | otherwise =tryNumbersAt sudoku row col 1           

-- Probiert Zahlen von 1 bis 9 an Position (row,col) 
tryNumbersAt :: Sudoku -> Int -> Int -> Int -> (Sudoku, Bool)
tryNumbersAt sudoku row col num
  | num == 10 = (sudoku, False)              
  | isValid sudoku row col num =
      let nextSudoku = put sudoku row col num  
          (solvedSudoku, success) = solveAt nextSudoku row (col + 1)
      in if success
           then (solvedSudoku, True)         
           else tryNumbersAt sudoku row col (num + 1)  
  | otherwise =
      tryNumbersAt sudoku row col (num + 1)   
