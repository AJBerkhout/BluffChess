module Game.UI.Square where

import Prelude

import CSS (Color, backgroundColor, black, height, px, white, width)
import Data.Maybe (Maybe(..))
import Game.Chess.Pieces (Piece)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Type.Proxy (Proxy(..))


type Input = { piece :: Maybe Piece, rank :: Int, file :: Int }

type State = { piece :: Maybe Piece , rank :: Int, file :: Int }

_square :: Proxy "button"
_square = Proxy

findColor :: Int -> Int -> Color
findColor rank file = 
  if (rank + file) `mod` 2 == 0 then
    black
  else
    white

square :: forall query output m. H.Component query Input output m
square =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval
    }
  where
  initialState :: Input -> State
  initialState input = input

  render :: forall action. State -> H.ComponentHTML action () m
  render { piece, rank, file } = 
    HH.div 
      [ CSS.style do 
          backgroundColor (findColor rank file)
          width $ px 50.0
          height $ px 50.0
      ]
      [ HH.text (
          case piece of
            Just p -> show p
            Nothing -> ""
        ) 
      ]
