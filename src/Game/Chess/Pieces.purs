module Chess.Pieces where

import Prelude

data Color = White | Black
derive instance Eq Color


data Piece = Pawn Color | Knight Color | Bishop Color| Rook Color | Queen Color | King Color
derive instance Eq Piece
instance Show Piece where
  show (Pawn _) = "P"
  show (Knight _) = "N"
  show (Bishop _) = "B"
  show (Rook _) = "R"
  show (Queen _) = "Q"
  show (King _) = "K"