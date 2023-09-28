module Game.UI.Board where

import Prelude

import CSS (display, flex)
import Data.Array (find, (..))
import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Game.Chess.Board (Board, initialBoard)
import Game.Chess.Move (Move, findLegalMoves, handleMove)
import Game.UI.Square (Clickable(..), Output(..), _square, square)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties as HP

data ClickedStatus = Clicked (Array Move) | NotClicked
type State = { board :: Board, clickedStatus :: ClickedStatus }

data Action = HandleClick Output

component :: forall query input output. H.Component query input output Aff
component =
  H.mkComponent
    { initialState: \_ -> {board : initialBoard, clickedStatus : NotClicked}
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

type Slots = ( square :: H.Slot (Const Void) Output Int )

render :: forall m. State -> H.ComponentHTML Action Slots m
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
                  matchingPiece = state.board # find (\{file, rank} -> file == f && rank == r) # map (\{piece} -> piece)
                  clickableStatus =
                    case state.clickedStatus of
                      Clicked moves -> 
                        case moves # find (\{to} -> to.rank == r && to.file == f ) of
                          Just move -> Move move.from
                          Nothing -> NotClickable
                      NotClicked -> 
                        case matchingPiece of
                          Just _ -> Piece
                          Nothing -> NotClickable
                in
                HH.div  
                  [ HP.classes [HH.ClassName (show f <> show r)] ]
                  [ HH.slot _square (r * 10 + f) square { piece : matchingPiece, rank : r, file : f, clickable: clickableStatus } HandleClick]
              )
            )
      )

handleAction :: forall output. Action -> H.HalogenM State Action Slots output Aff Unit
handleAction = case _ of
  HandleClick output -> 
    case output of
      InitialClick coord -> do
        liftEffect $ log "a"
        state <- H.get
        let moves = findLegalMoves state.board coord
        H.modify_ (\st -> st {clickedStatus = Clicked moves})
      MoveClick move -> do
        liftEffect $ log "a"
        state <- H.get
        let newBoard = handleMove move state.board
        H.modify_ (\_ -> {board : newBoard, clickedStatus : NotClicked})