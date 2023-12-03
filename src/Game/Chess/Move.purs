module Game.Chess.Move where

import Prelude

import Data.Array (filter, foldl, head, length, mapMaybe, takeWhile)
import Data.Maybe (Maybe(..), fromMaybe)
import Game.Chess.Board (Board, Coordinate, GameResult(..))
import Game.Chess.Pieces (Color(..), Piece(..), getData)

type RawMove = { from :: Coordinate, to :: Coordinate}

data Move = Move RawMove| Promotion RawMove | Castle { rook :: RawMove, king :: RawMove}

derive instance Eq Move
instance Show Move where
  show (Move {from, to}) = show from <> show to
  show (Promotion {from, to}) = show from <> show to <> "P"
  show (Castle {rook, king}) = show rook <> show king <> "C"
instance Ord Move where
  compare (Move m) (Move m') = compare m m'
  compare (Promotion m) (Promotion m') = compare m m'
  compare (Move _) (Promotion _) = LT
  compare (Promotion _) (Move _) = GT
  compare (Castle {king : km}) (Move m) = compare km m
  compare (Move m) (Castle {king : km}) = compare m km
  compare (Castle {king : km}) (Promotion m) = compare km m
  compare (Promotion m) (Castle {king : km}) = compare m km
  compare (Castle {king : km, rook : rm}) (Castle {king : km', rook : rm'}) = compare km km' <> compare rm rm'


getRawMove :: Move -> RawMove
getRawMove (Move move) = move
getRawMove (Promotion move) = move
getRawMove (Castle {king}) = king

getTo :: Move -> Coordinate
getTo (Move {to}) = to
getTo (Promotion {to}) = to
getTo (Castle {king}) = king.to

removePiece :: {rank :: Int, file :: Int} -> Board -> Board
removePiece move board = board # filter (\piece -> piece.rank /= move.rank || piece.file /= move.file)

movePiece :: RawMove -> Board -> Board
movePiece move board = board <> [move.to]

handleMove :: RawMove -> Board -> Board
handleMove move board = 
  board 
  # removePiece {rank: move.from.rank, file: move.from.file}
  # removePiece {rank: move.to.rank, file: move.to.file}
  # movePiece move

filterOccupied :: Board -> Array RawMove -> Array RawMove
filterOccupied board moves  = 
  moves 
  # filter (\{to} -> 
    (board 
    # filter (\{rank, file} -> rank == to.rank && file == to.file)
    # length)
    == 0
  )

filterSameColor :: Board -> Array RawMove -> Array RawMove
filterSameColor board moves  = 
  moves 
  # filter (\{to} -> 
    (board 
    # filter (\{rank, file, piece} -> rank == to.rank && file == to.file && (getData piece).color == (getData to.piece).color)
    # length)
    == 0
  )

filterWithinBoardRange :: Array RawMove -> Array RawMove
filterWithinBoardRange moves = 
  moves 
  # filter (\{to} -> 
    to.rank > 0 && to.rank < 9 && to.file > 0 && to.file < 9
  )

extrapolateDiagonal :: ({rank :: Int, file :: Int} -> {rank :: Int, file :: Int}) -> Board -> Coordinate -> {rank :: Int, file :: Int} -> Array RawMove
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

isInCheck :: Board -> Color -> Boolean
isInCheck board color = 
  let 
    king = 
      board 
      # filter (\{piece} -> 
        case piece of 
        King {color : c} | c == color -> true
        _ -> false
      )
      # head
      
    kingCoord = king # map (\{rank, file} -> {rank, file}) # fromMaybe {rank: 0, file: 0}
    enemyPieces = 
      board 
      # filter (\{piece} -> 
        case getData piece of 
          {color : c} | c /= color -> true
          _ -> false
      )
    enemyMoves = enemyPieces # map (\piece -> findBaseLegalMoves board piece)
    enemyMovesFlat = enemyMoves # foldl (\acc moves -> acc <> moves) []
  in
    (enemyMovesFlat # filter (\move -> (getTo move).rank == kingCoord.rank && (getTo move).file == kingCoord.file) # length) > 0

filterMovesThatPutKingInCheck :: Board -> Array Move -> Array Move
filterMovesThatPutKingInCheck board moves = 
  moves 
  # filter (\move-> 
    let 
      boardAfterMove = handleMove (getRawMove move) board
    in
      not (isInCheck boardAfterMove (getData (getTo move).piece).color)
  )

findCastleMoves :: Board -> Coordinate -> Array Move
findCastleMoves board king =
  let color = (getData king.piece).color in
  if isInCheck board color then 
    []
  else 
    let 
      kingCoord = king # (\{rank, file} -> {rank, file})
      kingHasMoved = (getData king.piece).hasMoved
      unmovedRooks = 
        board 
        # filter (\{piece} -> 
          case piece of 
          Rook {color : c} | c == color -> true
          _ -> false
        )
        # filter (\{piece} -> 
          not (getData piece).hasMoved
        )
    in  
      if kingHasMoved || (length unmovedRooks == 0) then 
        []
      else
        let 
          castleMoves = 
            unmovedRooks
            # mapMaybe (\rook@{rank, file, piece} ->
            
              let 
                rookDirection = if file > kingCoord.file then 1 else -1
                rookMove = {from: rook, to: {rank: rank, file: kingCoord.file + rookDirection, piece: piece}}
                kingMove = {from: king, to: {rank: rank, file: kingCoord.file + (rookDirection * 2), piece: king.piece}}
                allSpacesBetweenKingAndRookAreEmpty = 
                  (board 
                  # filter (\{rank : pieceRank, file : pieceFile} -> 
                    pieceRank == rank && pieceFile > kingCoord.file && pieceFile < kingCoord.file + rookDirection
                  )
                  # length) == 0
                kingDoesntMoveThroughCheck =
                  let 
                    intermediateKingMove = {from: king, to: {rank: rank, file: kingCoord.file + rookDirection, piece: king.piece}}
                    intermediateBoard = handleMove intermediateKingMove board
                  in 
                    not (isInCheck intermediateBoard color)
              in
                if allSpacesBetweenKingAndRookAreEmpty && kingDoesntMoveThroughCheck then 
                  Just $ Castle {rook: rookMove, king: kingMove}
                else Nothing
            )
        in
        castleMoves

convertToPromotions :: Array RawMove -> Array Move
convertToPromotions moves = 
  moves 
  # map (\{from, to} -> 
    case to.piece of 
      Pawn p -> 
        case p.color of 
          White | to.rank == 8 -> 
            Promotion {from, to}
          Black | to.rank == 1-> 
            Promotion {from, to}
          _ -> 
            
            Move {from, to}
      _ -> 
        Move {from, to}
  )


findBaseLegalMoves :: Board -> Coordinate -> Array Move
findBaseLegalMoves board (coord@{rank, file, piece : Pawn pieceData}) = -- TODO en passant
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
      (candidateMoves <> captureMoves)
      # filterWithinBoardRange
      # filterSameColor board
      # convertToPromotions

findBaseLegalMoves board (coord@{rank, file, piece : Knight _}) =
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
    <#> (Move)

findBaseLegalMoves board (coord@{piece : Bishop _}) =
    extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file + 1})    board coord {rank:coord.rank, file:coord.file}
    <> extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
    <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
    <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
    <#> (Move)


findBaseLegalMoves board (coord@{piece : Rook _}) =
  extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file})    board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
  <#> (Move)
findBaseLegalMoves board (coord@{piece : Queen _}) =
  extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file})    board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank + 1, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file + 1}) board coord {rank:coord.rank, file:coord.file}
  <> extrapolateDiagonal (\{rank, file} -> {rank : rank - 1, file : file - 1}) board coord {rank:coord.rank, file:coord.file}
  <#> (Move)

findBaseLegalMoves board (coord@{rank, file, piece : King _}) = -- TODO castling
  let 
    baseMoves = 
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
      # filterWithinBoardRange
      <#> (Move)
    castleMoves = findCastleMoves board coord
  in 
    baseMoves <> castleMoves


findLegalMoves :: Board -> Coordinate -> Array Move 
findLegalMoves board coord = 
  let 
    baseMoves = findBaseLegalMoves board coord
    legalMoves = baseMoves # filterMovesThatPutKingInCheck board
  in
    legalMoves

checkGameResult :: Board -> Color -> GameResult
checkGameResult board color = 
  let 
    pieces = 
      board 
      # filter (\{piece} -> 
        case getData piece of 
        {color : c} | c == color -> true
        _ -> false
      )
    unmoveablePieces = pieces # takeWhile (\piece -> (findLegalMoves board piece # length) == 0)
  in
    if (unmoveablePieces # length) == (pieces # length) && isInCheck board color then
      Checkmate
    else if (unmoveablePieces # length)  == (pieces # length)  then
      Stalemate
    else
      InProgress