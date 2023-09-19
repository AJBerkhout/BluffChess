module Chess.Board where

import Prelude

import Chess.Pieces (Color(..), Piece(..))
import Data.Array (find, foldl, (..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)


type Coordinate = { rank :: Int, file :: Int, piece :: Piece }
type Board = Array Coordinate 

concatString :: Array String -> String
concatString = foldl (\acc x -> acc <> " " <> x) ""

initialBoard :: Board
initialBoard = [
    { rank: 1, file: 1, piece: Rook White },
    { rank: 1, file: 2, piece: Knight White },
    { rank: 1, file: 3, piece: Bishop White },
    { rank: 1, file: 4, piece: Queen White },
    { rank: 1, file: 5, piece: King White },
    { rank: 1, file: 6, piece: Bishop White },
    { rank: 1, file: 7, piece: Knight White },
    { rank: 1, file: 8, piece: Rook White },
    { rank: 2, file: 1, piece: Pawn White },
    { rank: 2, file: 2, piece: Pawn White },
    { rank: 2, file: 3, piece: Pawn White },
    { rank: 2, file: 4, piece: Pawn White },
    { rank: 2, file: 5, piece: Pawn White },
    { rank: 2, file: 6, piece: Pawn White },
    { rank: 2, file: 7, piece: Pawn White },
    { rank: 2, file: 8, piece: Pawn White },
    { rank: 7, file: 1, piece: Pawn Black },
    { rank: 7, file: 2, piece: Pawn Black },
    { rank: 7, file: 3, piece: Pawn Black },
    { rank: 7, file: 4, piece: Pawn Black },
    { rank: 7, file: 5, piece: Pawn Black },
    { rank: 7, file: 6, piece: Pawn Black },
    { rank: 7, file: 7, piece: Pawn Black },
    { rank: 7, file: 8, piece: Pawn Black },
    { rank: 8, file: 1, piece: Rook Black },
    { rank: 8, file: 2, piece: Knight Black },
    { rank: 8, file: 3, piece: Bishop Black },
    { rank: 8, file: 4, piece: Queen Black },
    { rank: 8, file: 5, piece: King Black },
    { rank: 8, file: 6, piece: Bishop Black },
    { rank: 8, file: 7, piece: Knight Black },
    { rank: 8, file: 8, piece: Rook Black }
  ]

printBoard :: Board -> Effect Unit
printBoard board = do

  
  let 
    rankNames = ["A", "B", "C", "D", "E", "F", "G", "H"]
    ranks = 1..8

  log $ concatString $ rankNames <#> (\x -> "  " <> x <> " ")
  traverse_ (printRank board) ranks
  pure unit
  where
    printRank b rank = do
      let 
        files = 1..8
        rankStr = show rank
        rankPieces f = b # find (\c -> c.rank == rank && c.file == f)
        fullRank = files <#> (\f -> formatCoord $ rankPieces f) # concatString
      log $ rankStr <> fullRank <> " " <> rankStr

    formatCoord coord = 
      case coord of
        Nothing -> "|   "
        Just { piece } -> "| " <> show piece <> " "