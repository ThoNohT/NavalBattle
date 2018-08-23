module Main where

import Control.Monad.IO.Class
import Data.List
import Data.Maybe (Maybe)
import Text.Read (readMaybe)
import System.Console.ANSI
import System.Console.Haskeline


type ShipPresent = Bool
type Bombed = Bool

data CellState = CellState ShipPresent Bombed
type Board = [ [ CellState ] ]


data Phase = Placing | Bombing deriving (Eq)
data Player = Player1 | Player2
data Game = Game Phase Player Board Board 

newGame :: Game
newGame = Game Placing Player1 emptyBoard emptyBoard
emptyBoard :: Board
emptyBoard = replicate 10 $ replicate 10 $ CellState False False

getBoard :: Game -> (Board, Board)
getBoard (Game _ Player1 b1 b2) = (b1, b2)
getBoard (Game _ Player2 b1 b2) = (b2, b1)

setBoard :: Game -> Board -> Game
setBoard (Game Placing Player1 _ b) new = Game Placing Player1 new b
setBoard (Game Bombing Player1 b _) new = Game Bombing Player1 b new
setBoard (Game Placing Player2 b _) new = Game Placing Player2 b new
setBoard (Game Bombing Player2 _ b) new = Game Bombing Player2 new b


main :: IO ()
main = runInputT defaultSettings $ loop newGame
  where
    loop :: Game -> InputT IO ()
    loop game@(Game phase _ _ _) = let (ownBoard, opponentBoard) = getBoard game in      
      do
      outputStrLn $ displayBoard False opponentBoard
      outputStrLn $ replicate 10 '#'
      outputStrLn $ displayBoard True ownBoard
      minput <- getInputLine "% "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input
          | isPrefixOf "place " input -> if phase /= Placing
            then do
              outputStrLn "The battle is already started."
              loop game
            else handlePlace input ownBoard >>= loop . setBoard game
          | isPrefixOf "bomb " input -> if phase /= Bombing
            then do
              outputStrLn "The battle is not yet started."
              loop game
            else handleBomb input opponentBoard >>= loop . setBoard game
          | isPrefixOf "start" input -> handleStart game >>= loop
          | otherwise -> loop game 

handlePlace :: String -> Board -> InputT IO Board
handlePlace input board =
   case readMaybe (drop 6 input) of
    Just (x,y) -> case placeShip board x y of
      Left error -> do outputStrLn error
                       outputStrLn "Please try again."
                       return board
      Right new  -> return new
    Nothing -> do outputStrLn "Invalid input (x,y)."
                  return board

handleBomb :: String -> Board -> InputT IO Board
handleBomb input board  =
  case readMaybe (drop 5 input) of
    Just (x,y) -> case bomb board x y of
      Left error       -> do outputStrLn error
                             outputStrLn "Please try again."
                             return board
      Right (hit, new) -> do
        outputStrLn $ if hit then "Hit" else "Miss"
        return new
    Nothing -> do outputStrLn "Invalid input (x,y)."
                  return board

handleStart :: Game -> InputT IO Game
handleStart game = case startBattle game of
  Left error -> do outputStrLn error
                   outputStrLn "Please try again."
                   return game
  Right new -> do outputStr clearScreenCode
                  outputStrLn "Start the battle."
                  return new 


placeShip :: Board -> Int -> Int -> Either String Board
placeShip board x y
      | x < 0 || x > 9 || y < 0 || y > 9 = Left "Out of bounds."
      | otherwise = 
        let (pre, row:post) = splitAt y board
            (rpre, cell:rpost) = splitAt x row
        in case cell of
          CellState True _ -> Left "Ship already present."
          _                -> Right $ pre ++ [rpre ++ [CellState True False] ++ rpost] ++ post

countShips :: Board -> Int
countShips = foldl' f 0 . concat
             where f count (CellState True _) = 1 + count
                   f count _ = count

displayBoard :: Bool -> Board -> String
displayBoard hideShips rows = unlines $ map (map $ displayCellState hideShips) rows

displayCellState :: Bool -> CellState -> Char
displayCellState hideShips state = case state of
  CellState False False -> '~'
  CellState True False  -> if hideShips then '~' else 'O'
  CellState True True   -> '*'
  CellState False True  -> 'X'

startBattle :: Game -> Either String Game
startBattle (Game Placing _ b1 b2) = Right $ Game Bombing Player1 b1 b2
startBattle (Game Bombing _ b1 b2) = Left "Battle already started."

bomb :: Board -> Int -> Int -> Either String (Bool, Board)
bomb board x y
  | x < 0 || x > 9 || y < 0 || y > 9 = Left "Out of bounds."
  | otherwise = 
    let (pre, row:post) = splitAt y board
        (rpre, cell:rpost) = splitAt x row
    in case cell of
      CellState _ True -> Left "Already bombed"
      CellState hasShip _ -> Right $ (hasShip, pre ++ [rpre ++ [CellState hasShip True] ++ rpost] ++ post)