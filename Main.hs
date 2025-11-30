module Main where

import Generator
import Solver (solve)

type Sudoku = [[Int]]

-- Ausgabe eines Sudoku-Feldes in der Konsole, mit Zeilen-/Spalten-Indices
printSudoku :: Sudoku -> IO ()
printSudoku sudoku = do
    putStrLn "     1 2 3   4 5 6   7 8 9"
    putStrLn "   +-------+-------+-------+"
    mapM_ putStrLn (formatRows sudoku 1)
  where
    formatRows :: Sudoku -> Int -> [String]
    formatRows [] _ = []
    formatRows (row:rs) r =
      let rowStr = formatRow r row
          sep    = if r `mod` 3 == 0 && r /= 9
                     then ["  +-------+-------+-------+"]
                     else []
      in rowStr : sep ++ formatRows rs (r + 1)

    formatRow :: Int -> [Int] -> String
    formatRow r row =
      let cells = [ cellString c n | (c, n) <- zip [1..9] row ]
      in padRow r ++ "| " ++ concat cells ++ "|"

    cellString :: Int -> Int -> String
    cellString c n =
      let v = if n == 0 then ". " else show n ++ " "
      in if c `mod` 3 == 0 && c /= 9
            then v ++ "| "
            else v

    padRow :: Int -> String
    padRow r =
      if r < 10 then " " ++ show r ++ " " else show r ++ " "


-- Benutzerwahl fuer Anzahl der leeren Felder
getPuzzleDifficulty :: IO Int
getPuzzleDifficulty = do
    putStrLn "Wie viele Felder sollen leer sein? (1-64):"
    input <- getLine
    let emptyFields = read input :: Int
    if emptyFields >= 1 && emptyFields <= 64
        then return emptyFields
        else do
            putStrLn "Ungueltige Zahl. Bitte einen Wert zwischen 1 und 64 eingeben."
            getPuzzleDifficulty

-- Aktualisiert das Sudoku-Feld mit der Benutzereingabe und Hints
updateSudoku :: Sudoku -> Int -> Int -> IO (Sudoku, Int, Int)
updateSudoku sudoku errors hintsUsed = do
    putStrLn "\nEingabeoptionen:"
    putStrLn "'Zeile Spalte Zahl' um eine Zahl zu setzen (z.B. 1 3 9)"
    putStrLn "'hint Zeile Spalte' fuer einen Hinweis"
    putStrLn "'-1' um das Sudoku automatisch zu loesen"
    input <- getLine
    let parts = words input
    case parts of
      ["-1"] -> do
          let (solvedSudoku, success) = solve sudoku
          if success then do
              putStrLn "\nDas Sudoku wurde geloest!"
              printSudoku solvedSudoku
              return (solvedSudoku, errors, hintsUsed)
          else do
              putStrLn "Keine gueltige Loesung gefunden."
              return (sudoku, errors, hintsUsed)

      ["hint", r, c] -> do
          let row = read r - 1
              col = read c - 1
          -- Bounds-Check fuer Zeile/Spalte
          if row < 0 || row >= 9 || col < 0 || col >= 9 then do
              putStrLn "Ungueltige Position. Zeile und Spalte muessen zwischen 1 und 9 liegen."
              return (sudoku, errors, hintsUsed)
          else if hintsUsed >= 5 then do
              putStrLn "\nDu hast bereits 5 Hinweise verwendet. Keine weiteren erlaubt!"
              return (sudoku, errors, hintsUsed)
          else if (sudoku !! row) !! col /= 0 then do
              putStrLn $ "Feld (" ++ show (row+1) ++ "," ++ show (col+1) ++ ") ist bereits ausgefuellt mit " ++ show ((sudoku !! row) !! col) ++ ". Kein Hinweis noetig."
              return (sudoku, errors, hintsUsed)
          else do
              let (solution, success) = solve sudoku
              if success then do
                  let hintValue = (solution !! row) !! col
                  putStrLn $ "Hinweis fuer (" ++ show (row+1) ++ "," ++ show (col+1) ++ "): " ++ show hintValue
                  let newHintsUsed = hintsUsed + 1
                  putStrLn $ "Benutzte Hinweise: " ++ show newHintsUsed ++ "/5"
                  return (sudoku, errors, newHintsUsed)
              else do
                  putStrLn "Keine Loesung moeglich – das Sudoku ist nicht loesbar."
                  return (sudoku, errors, hintsUsed)

      [r, c, n] -> do
          let row = read r - 1
              col = read c - 1
              num = read n
          -- Bounds-Check fuer Zeile/Spalte
          if row < 0 || row >= 9 || col < 0 || col >= 9 then do
              putStrLn "Ungueltige Position. Zeile und Spalte muessen zwischen 1 und 9 liegen."
              return (sudoku, errors, hintsUsed)
          -- Optional: auch Zahlbereich pruefen
          else if num < 1 || num > 9 then do
              putStrLn "Ungueltige Zahl. Bitte eine Zahl zwischen 1 und 9 eingeben."
              return (sudoku, errors, hintsUsed)
          else if isValid sudoku row col num then
              return (put sudoku row col num, errors, hintsUsed)
          else do
              putStrLn "\nUngueltige Eingabe, bitte erneut versuchen.\n"
              let newErrors = errors + 1
              putStrLn $ "Fehleranzahl: " ++ show newErrors
              return (sudoku, newErrors, hintsUsed)

      _ -> do
          putStrLn "Ungueltiges Format. Bitte erneut versuchen."
          return (sudoku, errors, hintsUsed)


-- Hauptspiel-Funktion
playSudoku :: Sudoku -> Int -> Int -> IO ()
playSudoku sudoku errors hintsUsed = do
    putStrLn "\n========================================"
    putStrLn "              S U D O K U"
    putStrLn "========================================"
    putStrLn $ "Fehler: " ++ show errors ++ " | Benutzte Hinweise: " ++ show hintsUsed ++ "/5"
    putStrLn "========================================"
    printSudoku sudoku

    if isSolved sudoku
        then putStrLn $ "Herzlichen Glueckwunsch! Das Sudoku wurde geloest mit " ++ show errors ++ " Fehler(n)."
        else do
            (updatedSudoku, newErrors, newHintsUsed) <- updateSudoku sudoku errors hintsUsed
            playSudoku updatedSudoku newErrors newHintsUsed

-- Beispiel-Hauptfunktion
main :: IO ()
main = do
    putStrLn "\nGeneriere ein zufaelliges Raetsel..."
    putStrLn "\nMit '-1' als Eingabe kannst du das Sudoku automatisch loesen."
    emptyFields <- getPuzzleDifficulty
    generatedSudoku <- generateSudoku
    puzzle <- generatePuzzle generatedSudoku emptyFields
    playSudoku puzzle 0 0
