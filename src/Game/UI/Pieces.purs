module Game.UI.Pieces where

import Prelude

import Game.Chess.Pieces (Piece, getData)
import Halogen.HTML (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP




pieceToImage :: forall w i. Piece -> HTML w i
pieceToImage piece = 
  HH.img [ HP.src $ "/static/images/" <> imageName  <> ".png"]
  where 
    imageName = 
      show piece <> "_" <> show (getData piece).color
