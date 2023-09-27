module Game.UI.Square where

import Prelude

import Game.Chess.Pieces (Piece)
import Data.Maybe (Maybe)
import Halogen as H
import Halogen.HTML.CSS as CSS
import CSS (backgroundColor)
import Halogen.HTML as HH


type Input = { piece :: Maybe Piece, rank :: Int, file :: Int }

type State = { piece :: Maybe Piece , rank :: Int, file :: Int }

findColor :: Int -> Int -> String
findColor rank file = 
  if (rank + file) `mod` 2 == 0 then
    "black"
  else
    "white"

render :: forall m action. State -> H.ComponentHTML action () m
render { piece, rank, file } = 
  HH.div 
    [ CSS.style # do 
        backgroundColor (findColor rank file)
    ]
    [ HH.text (show piece) ]

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

