module Main where

import Data.List
import Data.Maybe (Maybe)
import Text.Read (readMaybe)
import System.Console.Haskeline


-- CellState Ship Bomb
data CellState = CellState Bool Bool

-- Board BattleStarted Rows
data Board = Board Bool [ [ CellState ] ]


emptyBoard = Board False $ replicate 10 $ replicate 10 $ CellState False False

main :: IO ()
main = runInputT defaultSettings $ loop emptyBoard
  where
    loop :: Board -> InputT IO ()
    loop board = do
      outputStr $ displayBoard False board
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input
          | isPrefixOf "place " input -> case readMaybe (drop 6 input) of
              Just (x,y) -> case placeShip board x y of
                Left error -> do outputStrLn error
                                 outputStrLn "Please try again."
                                 loop board
                Right new  -> loop new
              Nothing -> do outputStrLn "Invalid input (x,y)."
                            loop board
          | isPrefixOf "start" input -> case startBattle board of
              Left error -> do outputStrLn error
                               outputStrLn "Please try again."
                               loop board
              Right new -> do outputStrLn "Start the battle."
                              loop new
          | otherwise -> do outputStrLn $ "Input was: " ++ input
                            loop board

placeShip :: Board -> Int -> Int -> Either String Board
placeShip (Board battleStarted board) x y
      | x < 0 || x > 9 || y < 0 || y > 9 = Left "Out of bounds."
      | battleStarted = Left "Battle already started"
      | otherwise = 
        let (pre, row:post) = splitAt y board
            (rpre, cell:rpost) = splitAt x row
        in case cell of
          CellState True _ -> Left "Ship already present."
          _                -> Right $ Board False $ pre ++ [rpre ++ [CellState True False] ++ rpost] ++ post


displayBoard :: Bool -> Board -> String
displayBoard hideShips (Board _ rows) = unlines $ map (map $ displayCellState hideShips) rows

displayCellState :: Bool -> CellState -> Char
displayCellState hideShips state = case state of
  CellState False False -> '~'
  CellState True False  -> if hideShips then '~' else 'O'
  CellState True True   -> '*'
  CellState False True  -> 'X'

startBattle :: Board -> Either String Board
startBattle (Board battleStarted rows)
  | battleStarted = Left "Battle already started."
  | otherwise     = Right $ Board True rows