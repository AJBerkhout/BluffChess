module Test.Game.Chess.Move where

import Prelude

import Data.Array (length, sort)
import Game.Chess.Board (GameResult(..))
import Game.Chess.Move (checkGameResult, extrapolateDiagonal, filterMovesThatPutKingInCheck, filterOccupied, findLegalMoves, handleMove, isInCheck)
import Game.Chess.Pieces (Color(..), Piece(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

moveSpec :: Spec Unit
moveSpec = do
  describe "move pieces" do
    it "moves piece to empty square" do 
      let 
        simpleTestBoard = [
            {rank : 1, file : 1, piece : Pawn { color : White, hasMoved: false}},
            {rank : 2, file : 1, piece : Pawn { color : Black, hasMoved: false }}
          ]

        testMove = {
            from : {rank : 1, file : 1, piece : Pawn { color : White, hasMoved: false}},
            to : {rank : 1, file : 2, piece : Pawn { color : White, hasMoved: false}}
          }
        

        expectedBoard = [
            {rank : 1, file : 2, piece : Pawn { color : White, hasMoved: false}},
            {rank : 2, file : 1, piece : Pawn { color : Black, hasMoved: false }}
          ] # sort

      (handleMove testMove simpleTestBoard # sort)  `shouldEqual` expectedBoard

    it "moves piece to square occupied by opponent" do
      let 
        simpleTestBoard = [
            {rank : 1, file : 1, piece : Pawn { color : White, hasMoved: false}},
            {rank : 2, file : 1, piece : Pawn { color : Black, hasMoved: false }}
          ]

        testMove = {
            from : {rank : 1, file : 1, piece : Pawn { color : White, hasMoved: false}},
            to : {rank : 2, file : 1, piece : Pawn { color : White, hasMoved: false}}
          }
        

        expectedBoard = [
            {rank : 2, file : 1, piece : Pawn { color : White, hasMoved: false}}
          ]

      (handleMove testMove simpleTestBoard # sort)  `shouldEqual` expectedBoard

  describe "legal move helpers" do
    it "filters out occupied squares" do
      let 
        simpleTestBoard = [
            {rank : 1, file : 1, piece : Pawn { color : White, hasMoved: false}},
            {rank : 2, file : 1, piece : Pawn { color : Black, hasMoved: false }}
          ]

        testMove = [{
            from : {rank : 1, file : 1, piece : Pawn { color : White, hasMoved: false}},
            to : {rank : 2, file : 1, piece : Pawn { color : White, hasMoved: false}}
          }]
        

        expectedMoves = []

      filterOccupied simpleTestBoard testMove  `shouldEqual` expectedMoves

    it "continues on empty diagonal until bound" do 
      let 
        direction = (\{rank, file} -> {rank : rank + 1, file : file + 1})
        coordinate = {rank : 1, file : 1, piece : Bishop { color : White, hasMoved: false}}
        board = [coordinate]


        expectedMoves = 
          [
            { from: coordinate, to: {rank : 2, file : 2, piece : Bishop { color : White, hasMoved: false}}},
            { from: coordinate, to: {rank : 3, file : 3, piece : Bishop { color : White, hasMoved: false}}},
            { from: coordinate, to: {rank : 4, file : 4, piece : Bishop { color : White, hasMoved: false}}},
            { from: coordinate, to: {rank : 5, file : 5, piece : Bishop { color : White, hasMoved: false}}},
            { from: coordinate, to: {rank : 6, file : 6, piece : Bishop { color : White, hasMoved: false}}},
            { from: coordinate, to: {rank : 7, file : 7, piece : Bishop { color : White, hasMoved: false}}},
            { from: coordinate, to: {rank : 8, file : 8, piece : Bishop { color : White, hasMoved: false}}}
          ]
        actualMoves = extrapolateDiagonal direction board coordinate {rank:coordinate.rank, file:coordinate.file} 

      (actualMoves # sort) `shouldEqual` (expectedMoves # sort)

  describe "finds legal pawn moves" do
    it "white pawn moves 2 spaces on first move" do
      let 
        simpleTestBoard = [
            {rank : 2, file : 1, piece : Pawn { color : White, hasMoved: false}}
          ]
        coordinateToMove = {rank : 2, file : 1, piece : Pawn { color : White, hasMoved: false}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 1, piece : Pawn { color : White, hasMoved: false}}},
            {from: coordinateToMove, to:{rank : 4, file : 1, piece : Pawn { color : White, hasMoved: false}}}
          ]

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "white pawn moves 1 space after first move" do
      let 
        simpleTestBoard = [
            {rank : 3, file : 1, piece : Pawn { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 3, file : 1, piece : Pawn { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 4, file : 1, piece : Pawn { color : White, hasMoved: true}}}
          ]

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "white pawn captures diagonally" do
      let 
        simpleTestBoard = [
            {rank : 3, file : 1, piece : Pawn { color : White, hasMoved: true}},
            {rank : 4, file : 2, piece : Pawn { color : Black, hasMoved: true}}
          ]
        coordinateToMove = {rank : 3, file : 1, piece : Pawn { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 4, file : 1, piece : Pawn { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 2, piece : Pawn { color : White, hasMoved: true}}}
          ]

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "white pawn is blocked by other white piece" do 
      let 
        simpleTestBoard = [
            {rank : 3, file : 1, piece : Pawn { color : White, hasMoved: true}},
            {rank : 4, file : 1, piece : Pawn { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 3, file : 1, piece : Pawn { color : White, hasMoved: true}}
        expectedMoves = []

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "white pawn is blocked by other black piece" do
      let 
        simpleTestBoard = [
            {rank : 3, file : 1, piece : Pawn { color : White, hasMoved: true}},
            {rank : 4, file : 1, piece : Pawn { color : Black, hasMoved: true}}
          ]
        coordinateToMove = {rank : 3, file : 1, piece : Pawn { color : White, hasMoved: true}}
        expectedMoves = [
          ]

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "black pawn moves 2 spaces on first move" do
      let 
        simpleTestBoard = [
            {rank : 7, file : 1, piece : Pawn { color : Black, hasMoved: false}}
          ]
        coordinateToMove = {rank : 7, file : 1, piece : Pawn { color : Black, hasMoved: false}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: false}}},
            {from: coordinateToMove, to:{rank : 5, file : 1, piece : Pawn { color : Black, hasMoved: false}}}
          ]

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "black pawn moves 1 space after first move" do
      let 
        simpleTestBoard = [
            {rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: true}}
          ]
        coordinateToMove = {rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 5, file : 1, piece : Pawn { color : Black, hasMoved: true}}}
          ]

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "black pawn captures diagonally" do
      let 
        simpleTestBoard = [
            {rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 5, file : 2, piece : Pawn { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 5, file : 1, piece : Pawn { color : Black, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 2, piece : Pawn { color : Black, hasMoved: true}}}
          ]

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "black pawn is blocked by other black piece" do
      let 
        simpleTestBoard = [
            {rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 5, file : 1, piece : Pawn { color : Black, hasMoved: true}}
          ]
        coordinateToMove = {rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: true}}
        expectedMoves = []

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

    it "black pawn is blocked by other white piece" do
      let 
        simpleTestBoard = [
            {rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 5, file : 1, piece : Pawn { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 6, file : 1, piece : Pawn { color : Black, hasMoved: true}}
        expectedMoves = [
          ]

      (findLegalMoves simpleTestBoard coordinateToMove) `shouldEqual` expectedMoves

  describe "finds legal knight moves" do
    it "moves knight on empty board" do
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Knight { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : Knight { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 2, file : 3, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 5, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 2, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 6, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 2, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 6, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 3, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 5, piece : Knight { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "doesn't go outside board" do
      let 
        simpleTestBoard = [
            {rank : 1, file : 1, piece : Knight { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 1, file : 1, piece : Knight { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 2, file : 3, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 2, piece : Knight { color : White, hasMoved: true}}}
          ]
      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

      let
        simpleTestBoard2 = [
            {rank : 8, file : 8, piece : Knight { color : White, hasMoved: true}}
          ]
        coordinateToMove2 = {rank : 8, file : 8, piece : Knight { color : White, hasMoved: true}}
        expectedMoves2 = [
            {from : coordinateToMove2, to:{rank : 6, file : 7, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove2, to:{rank : 7, file : 6, piece : Knight { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard2 coordinateToMove2) # sort) `shouldEqual` (expectedMoves2 # sort)

      
    it "can't go to square occupied by same color" do
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Knight { color : White, hasMoved: true}},
            {rank : 2, file : 3, piece : Pawn { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : Knight { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 2, file : 5, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 2, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 6, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 2, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 6, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 3, piece : Knight { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 5, piece : Knight { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

  describe "find legal bishop moves" do
    it "moves bishop on empty board" do
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Bishop { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : Bishop { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 2, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 1, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 6, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 7, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 2, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 1, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 6, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 7, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 8, piece : Bishop { color : White, hasMoved: true}}}
          ]
      
      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "stops before first same colored piece" do
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Bishop { color : White, hasMoved: true}},
            {rank : 3, file : 3, piece : Pawn { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : Bishop { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 6, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 7, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 2, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 1, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 6, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 7, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 8, piece : Bishop { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "stops on first opposite colored piece" do 
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Bishop { color : White, hasMoved: true}},
            {rank : 3, file : 3, piece : Pawn { color : Black, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : Bishop { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 6, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 7, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 2, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 1, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 6, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 7, piece : Bishop { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 8, piece : Bishop { color : White, hasMoved: true}}}
          ]
      ((findLegalMoves simpleTestBoard coordinateToMove) # length) `shouldEqual` (expectedMoves # length)
      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

  describe "finds legal rook moves" do
    it "moves rook on empty board" do
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Rook { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : Rook { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 2, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 1, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 6, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 7, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 8, piece : Rook { color : White, hasMoved: true}}}
          ]
      
      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "stops before first same colored piece" do
      let
        simpleTestBoard = [
          {rank : 4, file : 4, piece : Rook { color : White, hasMoved: true}},
          {rank : 2, file : 4, piece : Pawn { color : White, hasMoved: true}}
        ]
        coordinateToMove = {rank : 4, file : 4, piece : Rook { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 2, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 1, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 6, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 7, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 8, piece : Rook { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "stops on first opposite colored piece" do
      let
        simpleTestBoard = [
          {rank : 4, file : 4, piece : Rook { color : White, hasMoved: true}},
          {rank : 2, file : 4, piece : Pawn { color : Black, hasMoved: true}}
        ]
        coordinateToMove = {rank : 4, file : 4, piece : Rook { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 2, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 4, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 2, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 1, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 6, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 7, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 8, piece : Rook { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

  describe "finds legal queen moves" do
    it "moves queen on empty board" do
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Queen { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : Queen { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 2, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 1, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 8, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 2, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 1, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 2, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 1, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 8, piece : Queen { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "stops before first same colored piece" do
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Queen { color : White, hasMoved: true}},
            {rank : 2, file: 4, piece : Pawn { color : White, hasMoved: true}},
            {rank : 3, file: 3, piece : Pawn { color : White, hasMoved: true}}
          ]

        coordinateToMove = {rank : 4, file : 4, piece : Queen { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 2, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 1, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 8, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 2, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 1, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 8, piece : Queen { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "stops on first opposite colored piece" do
      let 
        simpleTestBoard = [
            {rank : 4, file : 4, piece : Queen { color : White, hasMoved: true}},
            {rank : 2, file: 4, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 3, file: 3, piece : Pawn { color : Black, hasMoved: true}}
          ]

        coordinateToMove = {rank : 4, file : 4, piece : Queen { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 2, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 4, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 2, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 1, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 8, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 1, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 2, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 1, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 6, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 7, piece : Queen { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 8, piece : Queen { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)


  describe "finds legal king moves" do
    it "moves king on empty board" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : King { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 4, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : King { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "is blocked by same colored piece" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 3, file : 4, piece : Pawn { color : White, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : King { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : King { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)

    it "can capture opposite colored piece" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 3, file : 4, piece : Pawn { color : Black, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 4, piece : King { color : White, hasMoved: true}}
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 3, file : 4, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 4, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 3, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 3, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : King { color : White, hasMoved: true}}}
          ]

      ((findLegalMoves simpleTestBoard coordinateToMove) # sort) `shouldEqual` (expectedMoves # sort)
  describe "figures out when king is in check" do
    it "not in check" do
      let
        simpleTestBoard = 
          [
            {rank : 8, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 3, file : 4, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 4, file : 4, piece : King { color : Black, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` false
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "pawn check" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 5, file : 3, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 8, file : 4, piece : King { color : Black, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` true
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "knight check" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 6, file : 3, piece : Knight { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 8, file : 4, piece : King { color : Black, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` true
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "bishop check" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 6, file : 6, piece : Bishop { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 8, file : 4, piece : King { color : Black, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` true
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "blocked bishop check" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 6, file : 6, piece : Bishop { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 8, file : 4, piece : King { color : Black, hasMoved: true}},
            {rank : 5, file : 5, piece : Pawn { color : White, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` false
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "rook check" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 4, file : 8, piece : Rook { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 8, file : 4, piece : King { color : Black, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` true
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "blocked rook check" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 4, file : 8, piece : Rook { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 8, file : 4, piece : King { color : Black, hasMoved: true}},
            {rank : 4, file : 5, piece : Pawn { color : White, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` false
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "queen check" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 4, file : 8, piece : Queen { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 8, file : 4, piece : King { color : Black, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` true
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "blocked queen check" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 4, file : 8, piece : Queen { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 8, file : 4, piece : King { color : Black, hasMoved: true}},
            {rank : 4, file : 5, piece : Pawn { color : White, hasMoved: true}}
          ]
      (isInCheck simpleTestBoard White) `shouldEqual` false
      (isInCheck simpleTestBoard Black) `shouldEqual` false

    it "filters out illegal king moves" do 
      let
        simpleTestBoard = 
          [
            {rank : 3, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 4, file : 8, piece : Queen { color : Black, hasMoved: true}}
          ]
        coordinateToMove = {rank : 3, file : 4, piece : King { color : White, hasMoved: true}}
        candidateMoves = [
            {from : coordinateToMove, to:{rank : 4, file : 4, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 2, file : 4, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : King { color : White, hasMoved: true}}}
        ]
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 2, file : 4, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : King { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 3, piece : King { color : White, hasMoved: true}}}
        ]

      filterMovesThatPutKingInCheck simpleTestBoard candidateMoves `shouldEqual` expectedMoves

    it "filters out illegal pieces moves" do
      let 
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 4, file : 5, piece : Rook { color : White, hasMoved: true}},
            {rank : 4, file : 8, piece : Queen { color : Black, hasMoved: true}}
          ]
        coordinateToMove = {rank : 4, file : 5, piece : Rook { color : White, hasMoved: true}}
        candidateMoves = [
            {from : coordinateToMove, to:{rank : 4, file : 6, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 7, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 8, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 5, file : 5, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 6, file : 5, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 7, file : 5, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 8, file : 5, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 3, file : 5, piece : Rook { color : White, hasMoved: true}}}
        ]
        expectedMoves = [
            {from : coordinateToMove, to:{rank : 4, file : 6, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 7, piece : Rook { color : White, hasMoved: true}}},
            {from : coordinateToMove, to:{rank : 4, file : 8, piece : Rook { color : White, hasMoved: true}}}
        ]

      filterMovesThatPutKingInCheck simpleTestBoard candidateMoves `shouldEqual` expectedMoves
  
  describe "checks game end conditions" do

    it "game still going" do
      let
        simpleTestBoard = 
          [
            {rank : 8, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 3, file : 4, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 3, file : 5, piece : Pawn { color : Black, hasMoved: true}},
            {rank : 4, file : 4, piece : King { color : Black, hasMoved: true}}
          ]
      (checkGameResult simpleTestBoard White) `shouldEqual` InProgress
      (checkGameResult simpleTestBoard Black) `shouldEqual` InProgress

    it "stalemate" do 
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 8, file : 3, piece : Rook { color : Black, hasMoved : true}},
            {rank : 8, file : 5, piece : Rook { color : Black, hasMoved : true}},
            {rank : 3, file : 8, piece : Rook { color : Black, hasMoved : true}},
            {rank : 5, file : 8, piece : Rook { color : Black, hasMoved : true}}
          ]
      (checkGameResult simpleTestBoard White) `shouldEqual` Stalemate

    it "checkmate" do
      let
        simpleTestBoard = 
          [
            {rank : 4, file : 4, piece : King { color : White, hasMoved: true}},
            {rank : 8, file : 3, piece : Rook { color : Black, hasMoved : true}},
            {rank : 8, file : 5, piece : Rook { color : Black, hasMoved : true}},
            {rank : 3, file : 8, piece : Rook { color : Black, hasMoved : true}},
            {rank : 5, file : 8, piece : Rook { color : Black, hasMoved : true}},
            {rank : 4, file : 8, piece : Rook { color : Black, hasMoved : true}}
          ]
      (checkGameResult simpleTestBoard White) `shouldEqual` Checkmate



