module Game.UI.Board where

import Prelude

import CSS (display, flex)
import Data.Array (find, (..))
import Game.Chess.Board (Board, initialBoard)
import Game.Chess.Move (Move)
import Game.UI.Square (_square, square)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS



type State = Board

data Action = MovePiece Move | PrintBoard Board

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> initialBoard
    , render
    , eval: H.mkEval H.defaultEval
    }

type Slots = ( button :: forall query. H.Slot query Void Int )

render :: forall action m. State -> H.ComponentHTML action Slots m
render state = do
  HH.div
    [ CSS.style do 
          display flex
      ]
    board

  where
    ranks = 1..8
    files = 1..8
    board =
      files <#>
      (\f -> 
        HH.div_ 
          (
            ranks
            <#> 
              (\r ->
                let
                  matchingPiece = state # find (\{file, rank} -> file == f && rank == r) # map (\{piece} -> piece)
                in
                HH.div_ [ HH.slot_ _square 0 square { piece : matchingPiece, rank : r, file : f }]
              )
            )
      )
