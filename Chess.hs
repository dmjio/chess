{-# LANGUAGE OverloadedStrings #-}

module Chess where

import           Control.Monad.State (StateT (..), evalStateT)
import           Data.List.Split
import           Data.Map            hiding (map)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Prelude             hiding (lookup)

type Chess = StateT GameState IO ()

playChess :: GameState -> Chess -> IO ()
playChess = flip evalStateT

data GameState = GameState { p1    :: String
                           , p2    :: String
                           , board :: Board
                           } deriving (Show)

data ChessPiece = Rook | Knight | Bishop | Queen | King | Pawn | Empty deriving (Enum, Show)

data Color      = Black | White | None deriving (Show)
data Letter     = A | B | C | D | E | F | G | H deriving (Show, Eq, Ord, Enum)

type Position   = (Letter, Integer)
type Piece      = (ChessPiece, Color)
type Square     = (Position, Piece)


showHelper :: Square -> String -> Text
showHelper ((l,n),(piece,  color)) icon = T.pack $ show l ++ show n ++ icon

showSquare :: Square -> Text
showSquare ((l,n),(Rook,  Black)) = T.pack $ show l ++ show n ++ "♜"
showSquare ((l,n),(Bishop,Black)) = T.pack $ show l ++ show n ++ "♝"
showSquare ((l,n),(Knight,Black)) = T.pack $ show l ++ show n ++ "♞"
showSquare ((l,n),(Queen, Black)) = T.pack $ show l ++ show n ++ "♛"
showSquare ((l,n),(King,  Black)) = T.pack $ show l ++ show n ++ "♚"
showSquare ((l,n),(Pawn,  Black)) = T.pack $ show l ++ show n ++ "♟"
showSquare ((l,n),(Rook,  White)) = T.pack $ show l ++ show n ++ "♖"
showSquare ((l,n),(Bishop,White)) = T.pack $ show l ++ show n ++ "♗"
showSquare ((l,n),(Knight,White)) = T.pack $ show l ++ show n ++ "♘"
showSquare ((l,n),(Queen, White)) = T.pack $ show l ++ show n ++ "♕"
showSquare ((l,n),(King,  White)) = T.pack $ show l ++ show n ++ "♔"
showSquare ((l,n),(Pawn,  White)) = T.pack $ show l ++ show n ++ "♙"
showSquare ((l,n),(Empty, None)) = T.pack $ show l ++ show n ++ " - "

printBoard :: IO ()
printBoard = mapM_ (prnt . T.intercalate "  ") $ chunksOf 8 $ map showSquare . toList . unBoard $ initialBoard

newtype Board = Board { unBoard :: Map Position Piece } deriving (Show)

initialBoard :: Board
initialBoard    = Board . fromList . concat $ [ blackRoyalRow
                                              ,  blackPawnRow
                                              ,   emptyRow 6
                                              ,   emptyRow 5
                                              ,   emptyRow 4
                                              ,   emptyRow 3
                                              ,  whitePawnRow
                                              , whiteRoyalRow
                                              ]

blackRoyalRow, blackPawnRow, whitePawnRow, whiteRoyalRow :: [Square]
blackRoyalRow   = zip [ (l,8) | l <- [A .. H] ] $ zip [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] (repeat Black)
whiteRoyalRow   = zip [ (l,1) | l <- [A .. H] ] $ zip [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook] (repeat White)
blackPawnRow    = zip [ (l,7) | l <- [A .. H] ] $ repeat (Pawn, Black)
whitePawnRow    = zip [ (l,2) | l <- [A .. H] ] $ repeat (Pawn, White)

emptyRow :: Integer -> [Square]
emptyRow n = zip [ (l,n) | l <- [A .. H] ] $ repeat (Empty, None)

prnt :: Text -> IO ()
prnt = T.putStrLn




