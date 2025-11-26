# SudokuGame (Haskell-Konsolenspiel)

Ein einfaches Sudoku-Spiel für die Kommandozeile, geschrieben in **Haskell**.

Das Programm kann:

- ein **zufälliges, vollständig gelöstes Sudoku** erzeugen
- daraus ein **Rätsel mit wählbarer Schwierigkeit** erstellen
- den Benutzer das Sudoku **interaktiv lösen lassen**
- bis zu **5 Hinweise** geben
- das Sudoku auf Wunsch **automatisch lösen**

---

## Voraussetzungen

- **GHC / Haskell-Compiler**
- **Cabal 3.6** (oder kompatibel)

Falls Cabal noch nicht installiert ist, kann es über **ghcup** eingerichtet werden:

```bash
ghcup install cabal 3.6
ghcup set cabal 3.6


## Kompilieren & Ausführen

Im Projektverzeichnis ausführen:

```bash
cabal clean
cabal build
cabal run SudokuGame
