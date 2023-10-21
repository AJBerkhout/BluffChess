module Game.UI.Square where

import Prelude

import CSS (Color, backgroundColor, border, borderBox, boxSizing, display, flex, height, px, rgb, solid, white, width)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Game.Chess.Board (Coordinate)
import Game.Chess.Move (Move)
import Game.Chess.Pieces (Piece)
import Game.UI.Pieces (pieceToImage)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Type.Proxy (Proxy(..))

data SquareSlot = SquareSlot { rank :: Int, file :: Int}
instance EncodeJson SquareSlot where
  encodeJson = genericEncodeJson
derive instance Generic SquareSlot _
instance Show SquareSlot where
  show (SquareSlot { rank, file }) = show rank <> show file
derive instance Eq SquareSlot
derive instance Ord SquareSlot

data Output = 
    InitialClick Coordinate
  | MoveClick Move
  | CancelClick


data Clickable = Move Coordinate | Piece | NotClickable

data Action = IClick Coordinate | MClick Move | Cancel | ReceiveParentInput Input

type Input = { piece :: Maybe Piece, rank :: Int, file :: Int, clickable :: Clickable }

type State = 
  { piece :: Maybe Piece 
  , rank :: Int
  , file :: Int 
  , clickable :: Clickable
  }

_square :: Proxy "square"
_square = Proxy

findColor :: Int -> Int -> Color
findColor rank file = 
  if (rank + file) `mod` 2 == 0 then
    rgb 36 115 61
  else
    white

square :: forall query m. H.Component query Input Output m
square =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, receive = receiveNewInput }
    }
  where
  initialState :: Input -> State
  initialState input = input

  receiveNewInput :: Input -> Maybe Action
  receiveNewInput nextInputVal = Just $ ReceiveParentInput nextInputVal

  render :: State -> H.ComponentHTML Action () m
  render { piece, rank, file, clickable } = 
    let 
      dim = px 64.0
      divFeatures =
        case clickable of 
          NotClickable -> 
            [ CSS.style do 
                backgroundColor (findColor rank file)
                width $ dim
                height $ dim
                display flex
                
            , HE.onClick \_ -> Cancel
            ]
          Piece->
            case piece of
              Just p ->
                [ CSS.style do 
                  backgroundColor (findColor rank file)
                  width $ dim
                  height $ dim
                  display flex
                , (HE.onClick \_ -> IClick {piece : p, rank : rank, file : file})
                ]
              Nothing -> 
                [ CSS.style do 
                    backgroundColor (findColor rank file)
                    width $ dim
                    height $ dim
                    display flex
                , HE.onClick \_ -> Cancel
                ]
          Move fromCoord ->
            [ CSS.style do 
              backgroundColor (findColor rank file)
              width $ dim
              height $ dim
              border solid (px 2.0) (rgb 255 0 0)
              boxSizing borderBox
              display flex
            , (HE.onClick \_ -> MClick {from : fromCoord, to: {piece : fromCoord.piece, rank : rank, file : file}})
            ]
    in
    HH.div 
      divFeatures
      [ case piece of
          Just p -> pieceToImage p
          Nothing -> HH.text ""
      ]

  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    IClick coord -> H.raise $ InitialClick coord
    MClick move -> H.raise $ MoveClick move
    ReceiveParentInput input -> H.modify_ \_ -> input
    Cancel -> H.raise CancelClick
