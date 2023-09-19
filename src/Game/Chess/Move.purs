module Game.Chess.Move where

import Prelude 

import Chess.Board (Coordinate, Board)
import Data.Array (filter)


type Move = { from :: Coordinate, to :: Coordinate}

removePiece :: Move -> Board -> Board
removePiece move board = board # filter ((/=) move.from)

movePiece :: Move -> Board -> Board
movePiece move board = board <> [move.to]

handleMove :: Move -> Board -> Board
handleMove move board = 
  board # removePiece move # movePiece move