module Game.Chess.Pieces where

import Prelude

data Color = White | Black
derive instance Eq Color
instance Show Color where
  show White = "white"
  show Black = "black"

type PieceData = { color :: Color, hasMoved :: Boolean}

data Piece = Pawn PieceData | Knight PieceData | Bishop PieceData| Rook PieceData | Queen PieceData | King PieceData
derive instance Eq Piece
instance Show Piece where
  show (Pawn _) = "pawn"
  show (Knight _) = "knight"
  show (Bishop _) = "bishop"
  show (Rook _) = "rook"
  show (Queen _) = "queen"
  show (King _) = "king"
  
instance Ord Piece where
  compare (Pawn _) (Pawn _) = EQ
  compare (Pawn _) _ = LT
  compare (Knight _) (Knight _) = EQ
  compare (Knight _) _ = LT
  compare (Bishop _) (Bishop _) = EQ
  compare (Bishop _) _ = LT
  compare (Rook _) (Rook _) = EQ
  compare (Rook _) _ = LT
  compare (Queen _) (Queen _) = EQ
  compare (Queen _) _ = LT
  compare (King _) (King _) = EQ
  compare (King _) _ = LT

getData :: Piece -> PieceData
getData (Pawn d) = d
getData (Knight d) = d
getData (Bishop d) = d
getData (Rook d) = d
getData (Queen d) = d
getData (King d) = d
