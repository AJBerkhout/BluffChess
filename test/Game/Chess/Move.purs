module Test.Game.Chess.Move where

import Prelude

import Data.Array (sort)
import Game.Chess.Move (handleMove)
import Game.Chess.Pieces (Color(..), Piece(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

moveSpec :: Spec Unit
moveSpec = do
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
          ] # sort

      (handleMove testMove simpleTestBoard # sort)  `shouldEqual` expectedBoard

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

      (handleMove testMove simpleTestBoard # sort)  `shouldEqual` expectedBoard