module Game.Chess.Move where

import Prelude 

import Game.Chess.Board (Coordinate, Board)
import Data.Array (filter)


type Move = { from :: Coordinate, to :: Coordinate}

removePiece :: {rank :: Int, file :: Int} -> Board -> Board
removePiece move board = board # filter (\piece -> piece.rank /= move.rank || piece.file /= move.file)

movePiece :: Move -> Board -> Board
movePiece move board = board <> [move.to]

handleMove :: Move -> Board -> Board
handleMove move board = 
  board 
  # removePiece {rank: move.from.rank, file: move.from.file}
  # removePiece {rank: move.to.rank, file: move.to.file}
  # movePiece move