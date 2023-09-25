module Game.Chess.Move where

import Prelude

import Data.Array (filter, length)
import Game.Chess.Board (Coordinate, Board)
import Game.Chess.Pieces (Color(..), Piece(..), getData)


type Move = { from :: Coordinate, to :: Coordinate}

removePiece :: {rank :: Int, file :: Int} -> Board -> Board
removePiece move board = board # filter (\piece -> piece.rank /= move.rank || piece.file /= move.file)

movePiece :: Move -> Board -> Board
movePiece move board = board <> [move.to]

handleMove :: Move -> Board -> Board
handleMove move board = 
  board 
  # removePiece {rank: move.from.rank, file: move.from.file}
  # removePiece {rank: move.to.rank, file: move.to.file}
  # movePiece move

filterOccupied :: Board -> Array Move -> Array Move
filterOccupied board moves  = 
  moves 
  # filter (\{to} -> 
    (board 
    # filter (\{rank, file} -> rank == to.rank && file == to.file)
    # length)
    == 0
  )

filterSameColor :: Board -> Array Move -> Array Move
filterSameColor board moves  = 
  moves 
  # filter (\{to} -> 
    (board 
    # filter (\{rank, file, piece} -> rank == to.rank && file == to.file && (getData piece).color == (getData to.piece).color)
    # length)
    == 0
  )

filterWithinBoardRange :: Array Move -> Array Move
filterWithinBoardRange moves = 
  moves 
  # filter (\{to} -> 
    to.rank > 0 && to.rank < 9 && to.file > 0 && to.file < 9
  )

extrapolateDiagonal :: ({rank :: Int, file :: Int} -> {rank :: Int, file :: Int}) -> Board -> Coordinate -> {rank :: Int, file :: Int} -> Array Move
extrapolateDiagonal direction board initialCoord ({rank, file})  =
  let
    nextCoord = direction {rank, file}
  in
    if nextCoord.rank > 0 && nextCoord.rank < 9 && nextCoord.file > 0 && nextCoord.file < 9 then
      let 
        candidateMove = [{ from : initialCoord, to: {rank: nextCoord.rank, file: nextCoord.file, piece:initialCoord.piece}}]
        sameColorMove = candidateMove # filterSameColor board
        anyColorMove = candidateMove # filterOccupied board
      in
        if (sameColorMove # length) == 0 then
          []
        else if (anyColorMove # length) == 0 then
          candidateMove
        else 
          candidateMove <> extrapolateDiagonal direction board initialCoord nextCoord
    else
      []

findLegalMoves :: Board -> Coordinate -> Array Move --TODO moves that put king in check
findLegalMoves board (coord@{rank, file, piece : Pawn pieceData}) = -- TODO en passant, Promotions
  let 
    candidateMoves =
      case pieceData.color of
        White -> 
          if rank == 2 && not pieceData.hasMoved then
            [ { from : coord, to: {rank: rank + 1, file: file, piece: coord.piece}}
            , { from : coord, to: {rank:  rank + 2, file: file, piece: coord.piece}}
            ] # filterOccupied board
          else
            [{ from : coord, to: {rank: rank + 1, file: file, piece: coord.piece}}] # filterOccupied board
        Black -> 
          if rank == 7 && not pieceData.hasMoved then
            [
              { from : coord, to: {rank: rank - 1, file: file, piece: coord.piece}}
              ,{ from : coord, to: {rank: rank - 2, file: file, piece: coord.piece}}
            ] # filterOccupied board
          else
            [{ from : coord, to: {rank: rank - 1, file: file, piece: coord.piece}}] # filterOccupied board
    piecesInCaptureRange = 
      board 
      # filter (\{piece} -> 
        (getData piece).color /= pieceData.color
      )
      # filter (\{rank:captureRank, file:captureFile} -> 
        case pieceData.color of
          White -> captureRank == coord.rank + 1 && (captureFile == coord.file + 1 || captureFile == coord.file - 1)
          Black -> captureRank == coord.rank - 1 && (captureFile == coord.file + 1 || captureFile == coord.file - 1)
      )

    captureMoves = 
      piecesInCaptureRange
      # map (\{rank:captureRank, file:captureFile} -> 
        { from : coord, to: {rank: captureRank, file: captureFile, piece: coord.piece}}
      )

    in 
      candidateMoves <> captureMoves

findLegalMoves board (coord@{rank, file, piece : Knight _}) =
  let 
    candidateMoves = 
      [{ from : coord, to: {rank: rank + 2, file: file + 1, piece: coord.piece}}
      ,{ from : coord, to: {rank: rank + 2, file: file - 1, piece: coord.piece}}
      ,{ from : coord, to: {rank: rank - 2, file: file + 1, piece: coord.piece}}
      ,{ from : coord, to: {rank: rank - 2, file: file - 1, piece: coord.piece}}
      ,{ from : coord, to: {rank: rank + 1, file: file + 2, piece: coord.piece}}
      ,{ from : coord, to: {rank: rank + 1, file: file - 2, piece: coord.piece}}
      ,{ from : coord, to: {rank: rank - 1, file: file + 2, piece: coord.piece}}
      ,{ from : coord, to: {rank: rank - 1, file: file - 2, piece: coord.piece}}
      ]
  in 
    candidateMoves
    # filterSameColor board
    # filterWithinBoardRange

findLegalMoves board (coord@{piece : Bishop _}) =
    extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file + 1})    board coord {rank:coord.rank, file:coord.file}
    <> extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
    <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
    <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
    


findLegalMoves board (coord@{piece : Rook _}) =
  extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file})    board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank, file : file - 1}) board coord {rank:coord.rank, file:coord.file}

findLegalMoves board (coord@{piece : Queen _}) =
  extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file})    board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file - 1}) board coord {rank:coord.rank, file:coord.file}


findLegalMoves board (coord@{rank, file, piece : King _}) = -- TODO castling
  [{ from : coord, to: {rank: rank + 1, file: file, piece: coord.piece}}
  , { from : coord, to: {rank: rank - 1, file: file, piece: coord.piece}}
  , { from : coord, to: {rank: rank, file: file + 1, piece: coord.piece}}
  , { from : coord, to: {rank: rank, file: file - 1, piece: coord.piece}}
  , { from : coord, to: {rank: rank + 1, file: file + 1, piece: coord.piece}}
  , { from : coord, to: {rank: rank + 1, file: file - 1, piece: coord.piece}}
  , { from : coord, to: {rank: rank - 1, file: file + 1, piece: coord.piece}}
  , { from : coord, to: {rank: rank - 1, file: file - 1, piece: coord.piece}}
  ]
  # filterSameColor board