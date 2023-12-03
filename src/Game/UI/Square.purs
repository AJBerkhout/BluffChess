module Game.UI.Square where

import Prelude

import CSS (Color, absolute, backgroundColor, border, borderBox, boxSizing, column, cursor, display, flex, flexDirection, height, position, px, rgb, solid, white, width)
import CSS.Cursor (pointer)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Game.Chess.Board (Coordinate)
import Game.Chess.Move (Move(..), RawMove)
import Game.Chess.Pieces (Piece(..))
import Game.Chess.Pieces as Pieces
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
  | MoveClick (Array RawMove)
  | CancelClick


data Clickable = PieceMove Move | Piece | PromotionMove Move | NotClickable

data Action = IClick Coordinate | MClick Move | Cancel | ReceiveParentInput Input

type Input = { piece :: Maybe Piece, rank :: Int, file :: Int, clickable :: Clickable, turn :: Pieces.Color }

type State = 
  { piece :: Maybe Piece 
  , rank :: Int
  , file :: Int 
  , clickable :: Clickable
  , turn :: Pieces.Color
  }

_square :: Proxy "square"
_square = Proxy

findColor :: Int -> Int -> Color
findColor rank file = 
  if (rank + file) `mod` 2 == 0 then
    rgb 36 115 61
  else
    white

convertMove :: Piece -> Move -> Move
convertMove piece move =
  case move of 
    Promotion m -> Move { from : m.from, to : { rank : m.to.rank, file : m.to.file, piece : piece }}
    Move m -> Move m
    Castle m -> Castle m

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
  render { piece, rank, file, clickable, turn } = 
    let 
      dim = px 64.0
      bigDim = px (64.0 * 4.0)
      divFeatures =
        case clickable of 
          NotClickable -> 
            HH.div 
              [ CSS.style do 
                  backgroundColor (findColor rank file)
                  width $ dim
                  height $ dim
                  display flex
              , HE.onClick \_ -> Cancel
              ]
              [ case piece of
                Just p -> pieceToImage p
                Nothing -> HH.text ""
              ]
              
          Piece->
            case piece of
              Just p ->
                HH.div 
                  [ CSS.style do 
                    backgroundColor (findColor rank file)
                    width $ dim
                    height $ dim
                    display flex
                    cursor pointer
                  , (HE.onClick \_ -> IClick {piece : p, rank : rank, file : file})
                  ]
                  [ case piece of
                    Just actualPiece -> pieceToImage actualPiece
                    Nothing -> HH.text ""
                  ]
              Nothing -> 
                HH.div
                  [ CSS.style do 
                      backgroundColor (findColor rank file)
                      width $ dim
                      height $ dim
                      display flex
                  , HE.onClick \_ -> Cancel
                  ]
                  [ case piece of
                    Just p -> pieceToImage p
                    Nothing -> HH.text ""
                  ]
          PieceMove move ->
            HH.div  
              [ CSS.style do 
                backgroundColor (findColor rank file)
                width $ dim
                height $ dim
                border solid (px 2.0) (rgb 255 0 0)
                boxSizing borderBox
                display flex
                cursor pointer
              , (HE.onClick \_ -> MClick move)
              ]
              [ case piece of
                Just p -> pieceToImage p
                Nothing -> HH.text ""
              ]

          PromotionMove move ->
            HH.div 
              [ CSS.style do 
                backgroundColor (findColor rank file)
                width $ dim
                height $ dim
                border solid (px 2.0) (rgb 255 0 0)
                boxSizing borderBox
                display flex
                cursor pointer
              ]
              [ 
                HH.div 
                  [ CSS.style do 
                    width $ dim
                    height $ bigDim
                    display flex
                    flexDirection column
                    backgroundColor $ rgb 211 211 211
                    position absolute
                    cursor pointer
                  ]
                  [
                    HH.div 
                      [ CSS.style do 
                        width $ dim
                        height $ dim
                        display flex
                        cursor pointer
                      , (HE.onClick \_ -> MClick (convertMove (Queen { color : turn, hasMoved: true }) move))
                      ]
                      [ pieceToImage (Queen { color : turn, hasMoved: true }) ]
                  , HH.div 
                      [ CSS.style do 
                        width $ dim
                        height $ dim
                        display flex
                        cursor pointer
                      , (HE.onClick \_ -> MClick (convertMove (Rook { color : turn, hasMoved: true }) move))
                      ]
                      [ pieceToImage (Rook { color : turn, hasMoved: true }) ]
                  , HH.div 
                      [ CSS.style do 
                        width $ dim
                        height $ dim
                        display flex
                        cursor pointer
                      , (HE.onClick \_ -> MClick (convertMove (Knight { color : turn, hasMoved: true }) move))
                      ]
                      [ pieceToImage (Knight { color : turn, hasMoved: true }) ]
                  , HH.div 
                      [ CSS.style do 
                        width $ dim
                        height $ dim
                        display flex

                        cursor pointer
                      , (HE.onClick \_ -> MClick (convertMove (Bishop { color : turn, hasMoved: true }) move))
                      ]
                      [ pieceToImage (Bishop { color : turn, hasMoved: true }) ]                  
                  ]          
              ]
    in
    
      divFeatures
      
  handleAction :: Action -> H.HalogenM State Action () Output m Unit
  handleAction = case _ of
    IClick coord -> H.raise $ InitialClick coord
    MClick move ->
      case move of 
        Promotion _m -> H.modify_ \s -> s { clickable = PromotionMove move } 
        Move m ->  H.raise $  MoveClick [m]
        Castle {king : km, rook : rm} -> 
          H.raise $ MoveClick [km, rm]
    ReceiveParentInput input -> H.modify_ \_ -> input
    Cancel -> H.raise CancelClick
