-- http://www.reddit.com/r/dailyprogrammer/comments/21kqjq/4282014_challenge_154_hard_wumpus_cave_game/
module Main where

import           Control.Applicative ((<$>))
import           Control.Lens (view, ix, contains)
import           Control.Lens.Fold (preuse)
import           Control.Lens.Getter (to, use)
import           Control.Lens.Indexed (imap)
import           Control.Lens.Operators
import           Control.Monad (when, unless)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Random
import           Control.Monad.State.Strict (evalStateT)
import           Data.Char (toUpper)
import           Data.List (intercalate, delete)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Traversable (traverse)
import           System.Random.Shuffle (shuffleM)

import           Types
import           Lenses

createRooms :: Int -> SpawnRate -> Room -> [Room]
createRooms roomNumber rate = replicate num
  where num = floor $ rate * fromIntegral roomNumber

divideUp :: Int -> [a] -> [[a]]
divideUp n = take n . map (take n) . iterate (drop n)

buildCave :: (Functor m, MonadRandom m) => Int -> SpawnRates -> m Cave
buildCave n rates = do
  createdRooms <- shuffleM . take nRooms $ Entrance : specialRooms ++ repeat Empty
  return . Cave n . Map.fromList $ imap (\i r -> (to2d i, r)) createdRooms
  where
    to2d :: Int -> (Int,Int)
    to2d i = (i `mod` n, i `div` n)
    nRooms :: Int
    nRooms = n*n
    specialRooms :: [Room]
    specialRooms = createRooms nRooms (view wumpusRate rates) Wumpus
                ++ createRooms nRooms (view trapRate rates) Trap
                ++ createRooms nRooms (view goldRate rates) Gold
                ++ createRooms nRooms (view weaponRate rates) Weapon

environment :: (Int,Int) -> [(Int,Int)]
environment (x,y) = [ (x-1,y-1)
                    , (x,y-1)
                    , (x+1,y-1)
                    , (x-1,y)
                    , (x,y)
                    , (x+1,y)
                    , (x-1,y+1)
                    , (x,y+1)
                    , (x+1,y+1)
                    ]

environment4 :: (Int, Int) -> [(Int, Int)]
environment4 (x,y) = [ (x,y-1)
                     , (x-1,y)
                     , (x,y)
                     , (x+1,y)
                     , (x,y+1)
                     ]

safeLookup :: (Int, Int) -> Game Room
safeLookup coord = do
  exploredRooms <- use explored
  if not $ coord `Set.member` exploredRooms
    then return Unknown
    else fromMaybe Blocked <$> preuse (cave . atCoord coord)

movePlayer :: Direction -> Game Directive
movePlayer dir = do
  oldPlayerPos <- use playerCoord
  let newPlayerPos = calculateNewPosition dir oldPlayerPos
  roomAtPos <- safeLookup newPlayerPos
  performTransition newPlayerPos roomAtPos

message :: String -> Game ()
message = liftIO . putStrLn

calculateNewPosition :: Direction -> (Int,Int) -> (Int,Int)
calculateNewPosition North (x,y) = (x,y-1)
calculateNewPosition South (x,y) = (x,y+1)
calculateNewPosition West  (x,y) = (x-1,y)
calculateNewPosition East  (x,y) = (x+1,y)

initializeGame :: Cave -> GameState
initializeGame c = GameState newPlayer c Set.empty initialScoreBoard
  where newPlayer = Player entrancePos 0 False
        entrancePos = findEntrance c

main :: IO ()
main = do
  let sizeOfCave = 10
  builtCave <- buildCave sizeOfCave defaultSpawnRates
  let initGame = initializeGame builtCave
      playerStart = initGame ^. playerCoord
  evalStateT (exploreRoom playerStart >> gameLoop) initGame

gameLoop :: Game ()
gameLoop = do
  message . replicate 30 $ '-'
  environmentWarnings
  printEnv
  gameStatus
  liftIO . putStr $ "Your move (? for help): "
  input <- toUpper . head <$> liftIO getLine
  message ""
  message . replicate 30 $ '-'
  executeCommand input

executeCommand :: Char -> Game ()
executeCommand c = do
  directive <- case c of
    'N' -> movePlayer North
    'S' -> movePlayer South
    'E' -> movePlayer East
    'W' -> movePlayer West
    '?' -> message helpText >> return Continue
    'L' -> loot
    'R' -> escape
    'X' -> message "Goodbye." >> return Stop
    _ -> message ("Unknown command:" ++ [c] ++ ".") >> return Continue
  case directive of
    Continue -> gameLoop
    GameOver -> gameOver
    GameWon -> gameWon
    Stop -> return ()

escape :: Game Directive
escape = do
  curRoom <- use playerRoom
  if curRoom == Entrance
     then return GameWon
    else message "No entrance here." >> return Continue

loot :: Game Directive
loot = do
  curRoom <- use playerRoom
  if curRoom `notElem` [Weapon, Gold]
    then message "Nothing to loot." >> return Continue
    else pickup curRoom >> return Continue

pickup :: Room -> Game ()
pickup Gold = do
  message "You pick up the coins."
  scores . purse += 1
  playerRoom .= Empty
pickup Weapon = do
  message "You pick up the weapon."
  playerHasWeapon .= True
  playerRoom .= Empty
  cave . weaponRooms .= Gold
pickup _ = message "Nothing to pickup here."

helpText :: String
helpText = intercalate "\n" [ "? - Show this help"
                            , "N - Go north if possible"
                            , "S - Go south if possible"
                            , "W - Go west if possible"
                            , "E - Go east if possible"
                            , "L - Loot either gold or weapon"
                            , "R - run of the cave (only at entrance)"
                            , "X - quit game"
                            ]

withDirective :: Directive -> Game ()
withDirective Continue = gameLoop
withDirective GameOver = gameOver
withDirective GameWon = gameWon
withDirective Stop = return ()

gameStatus :: Game ()
gameStatus = do
  hasWeapon <- use playerHasWeapon
  gold <- use $ scores . purse
  let weaponStr = if hasWeapon
                  then "You have a weapon."
                  else "You are not armed."
      goldStr = "Gold: " ++ show gold
  pts <- show <$> calculateScore
  message $ weaponStr ++ "   " ++ goldStr ++ " Score: " ++ pts

environmentWarnings :: Game ()
environmentWarnings = do
  pos <- use playerCoord
  env <- delete pos . environment4 <$> use playerCoord
  c <- use cave
  let nbs = concatMap (\coord -> c ^.. atCoord coord) env
  when (Wumpus `elem` nbs) $ message "You smell a foul stench."
  when (Trap `elem` nbs) $ message "You hear a howling wind."

gameOver :: Game ()
gameOver = message "Game over." >> gameEvaluation

calculateScore :: Game Int
calculateScore = do
  monsters <- use $ scores . slayedMonsters
  gold <- use $ scores . purse
  hasWeapon <- use playerHasWeapon
  roomPts <- use $ scores . exploredEmptyRooms
  let monstertPts = monsters * 10
      goldPts = gold * 5
      weaponPts = if hasWeapon then 5 else 0
  return $ monstertPts + goldPts + weaponPts + roomPts

gameEvaluation :: Game ()
gameEvaluation = do
  pts <- calculateScore
  message $ "You earned " ++ show pts ++ " points."

gameWon :: Game ()
gameWon = message "You win!" >> gameEvaluation

printEnv :: Game ()
printEnv = do
  pos <- use playerCoord
  let env = environment pos
  rs <- divideUp 3 <$> traverse safeLookup env
  let showed = map (view (traverse . to showRoom)) rs
  message $ intercalate "\n" (showed & ix 1 . ix 1 .~ '@')

showRoom :: Room -> String
showRoom Entrance = "^"
showRoom Empty = "."
showRoom Blocked = "#"
showRoom Weapon = "W"
showRoom Gold = "$"
showRoom Trap = "%"
showRoom Wumpus = "!"
showRoom Unknown = "?"

findEntrance :: Cave -> (Int,Int)
findEntrance c = fst
               . head
               . Map.toList
               . Map.filter (== Entrance) $ roomMap
  where roomMap = view rooms c

exploreRoom :: (Int,Int) -> Game ()
exploreRoom pos = explored . contains pos .= True

performTransition :: (Int,Int) -> Room -> Game Directive
performTransition _ Blocked = do
  message "Gmpft.  There's a wall."
  return Continue

performTransition _ Trap = do
  message "Omg is it dark in here, wait whats there? AAAAAAAAAAAAHhhhh SPLAT."
  return GameOver

performTransition newPos Unknown = do
  message "You feel around..."
  exploreRoom newPos
  r <- safeLookup newPos
  performTransition newPos r

performTransition newPos Empty = do
  message "There is nothing."
  curPos <- use playerCoord
  alreadySeen <- use $ explored . contains curPos
  unless alreadySeen $ scores . exploredEmptyRooms += 1
  playerCoord .= newPos
  return Continue

performTransition newPos Wumpus = do
  hasWeapon <- use playerHasWeapon
  if hasWeapon
    then do
      message "A wild wumpus appears!"
      message "You use your weapon to slay it."
      playerCoord .= newPos
      playerRoom .= Empty
      scores . slayedMonsters += 1
      return Continue
    else message "Oh no the Wumpus eats you." >> return GameOver

performTransition newPos Weapon = do
  message "There is a sword on the floor."
  playerCoord .= newPos
  return Continue

performTransition newPos Gold = do
  message "Gold is shimmering in a corner of the room."
  playerCoord .= newPos
  return Continue

performTransition newPos Entrance = do
  message "There is the exit!"
  playerCoord .= newPos
  return Continue
