module Game.Chess.Board where

import Prelude

import Game.Chess.Pieces (Color(..), Piece(..))
import Data.Array (find, foldl, (..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)


type Coordinate = { rank :: Int, file :: Int, piece :: Piece }
type Board = Array Coordinate 

data GameResult = Checkmate | Stalemate | InProgress

concatString :: Array String -> String
concatString = foldl (\acc x -> acc <> " " <> x) ""

initialBoard :: Board
initialBoard = [
    { rank: 1, file: 1, piece: Rook { color : White, hasMoved: false } },
    { rank: 1, file: 2, piece: Knight { color : White, hasMoved: false } },
    { rank: 1, file: 3, piece: Bishop { color : White, hasMoved: false } },
    { rank: 1, file: 4, piece: Queen { color : White, hasMoved: false } },
    { rank: 1, file: 5, piece: King { color : White, hasMoved: false } },
    { rank: 1, file: 6, piece: Bishop { color : White, hasMoved: false } },
    { rank: 1, file: 7, piece: Knight { color : White, hasMoved: false } },
    { rank: 1, file: 8, piece: Rook { color : White, hasMoved: false } },
    { rank: 2, file: 1, piece: Pawn { color : White, hasMoved: false } },
    { rank: 2, file: 2, piece: Pawn { color : White, hasMoved: false } },
    { rank: 2, file: 3, piece: Pawn { color : White, hasMoved: false } },
    { rank: 2, file: 4, piece: Pawn { color : White, hasMoved: false } },
    { rank: 2, file: 5, piece: Pawn { color : White, hasMoved: false } },
    { rank: 2, file: 6, piece: Pawn { color : White, hasMoved: false } },
    { rank: 2, file: 7, piece: Pawn { color : White, hasMoved: false } },
    { rank: 2, file: 8, piece: Pawn { color : White, hasMoved: false } },
    { rank: 7, file: 1, piece: Pawn { color : Black, hasMoved: false } },
    { rank: 7, file: 2, piece: Pawn { color : Black, hasMoved: false } },
    { rank: 7, file: 3, piece: Pawn { color : Black, hasMoved: false } },
    { rank: 7, file: 4, piece: Pawn { color : Black, hasMoved: false } },
    { rank: 7, file: 5, piece: Pawn { color : Black, hasMoved: false } },
    { rank: 7, file: 6, piece: Pawn { color : Black, hasMoved: false } },
    { rank: 7, file: 7, piece: Pawn { color : Black, hasMoved: false } },
    { rank: 7, file: 8, piece: Pawn { color : Black, hasMoved: false } },
    { rank: 8, file: 1, piece: Rook { color : Black, hasMoved: false } },
    { rank: 8, file: 2, piece: Knight { color : Black, hasMoved: false } },
    { rank: 8, file: 3, piece: Bishop { color : Black, hasMoved: false } },
    { rank: 8, file: 4, piece: Queen { color : Black, hasMoved: false } },
    { rank: 8, file: 5, piece: King { color : Black, hasMoved: false } },
    { rank: 8, file: 6, piece: Bishop { color : Black, hasMoved: false } },
    { rank: 8, file: 7, piece: Knight { color : Black, hasMoved: false } },
    { rank: 8, file: 8, piece: Rook { color : Black, hasMoved: false } }
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