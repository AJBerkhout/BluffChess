module Game.UI.Board where

import Prelude

import Effect.Class (class MonadEffect)
import Game.Chess.Board (Board, initialBoard, printBoard)
import Game.Chess.Move (Move, handleMove)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Web.HTML.Event.DataTransfer (files)

type State = Board

data Action = MovePiece Move | PrintBoard Board

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState: \_ -> initialBoard
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
  let 
    ranks = [1..8]
    files = ['a'..'h']

  HH.div_
    

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
  MovePiece move -> H.modify_ (handleMove move)
  PrintBoard state -> do
    H.liftEffect $ printBoard state
    H.modify_ (const state)
