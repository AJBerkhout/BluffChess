module Test.Game.Chess.Test.Move where

import Prelude

import Game.Chess.Move (movePiece)
import Game.Chess.Pieces (Color(..), Piece(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = do
  describe "move pieces" do
    it "moves piece to empty square" do 
      let 
        simpleTestBoard = [
            {rank : 1, file : 1, piece : Pawn White},
            {rank : 2, file : 1, piece : Pawn Black}
          ]

        testMove = {
            from : {rank : 1, file : 1, piece : Pawn White},
            to : {rank : 1, file : 2, piece : Pawn White}
          }
        

        expectedBoard = [
            {rank : 1, file : 2, piece : Pawn White},
            {rank : 2, file : 1, piece : Pawn Black}
          ]

      movePiece testMove simpleTestBoard  `shouldEqual` expectedBoard

    it "moves piece to square occupied by opponent" do
      let 
        simpleTestBoard = [
            {rank : 1, file : 1, piece : Pawn White},
            {rank : 2, file : 1, piece : Pawn Black}
          ]

        testMove = {
            from : {rank : 1, file : 1, piece : Pawn White},
            to : {rank : 2, file : 1, piece : Pawn White}
          }
        

        expectedBoard = [
            {rank : 2, file : 1, piece : Pawn White}
          ]

      movePiece testMove simpleTestBoard  `shouldEqual` expectedBoard