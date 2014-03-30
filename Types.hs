{-# LANGUAGE TemplateHaskell #-}
module Types ( SpawnRate
             , defaultSpawnRates
             , Directive(..)
             , Direction (..)
             , Room (..)

             , Cave (Cave)
             , caveSize
             , rooms

             , SpawnRates
             , wumpusRate
             , trapRate
             , goldRate
             , weaponRate

             , Player (Player)
             , playerPosition
             , playerGold
             , playerWeapon

             , GameState (GameState)
             , player
             , cave
             , explored
             , scores

             , ScoreBoard
             , purse
             , slayedMonsters
             , exploredEmptyRooms
             , initialScoreBoard

             , Game
             ) where

import Control.Lens.TH
import Control.Monad.State.Strict (StateT)
import Data.Map (Map)
import Data.Set (Set)

type SpawnRate = Double

data Directive = Continue | GameOver | GameWon | Stop

data Room = Entrance
          | Wumpus
          | Trap
          | Gold
          | Weapon
          | Empty
          | Blocked
          | Unknown
          deriving (Show, Eq)

data Cave = Cave { _caveSize :: Int, _rooms :: Map (Int,Int) Room }
makeLenses ''Cave

data SpawnRates = SpawnRates { _wumpusRate :: SpawnRate
                             , _trapRate :: SpawnRate
                             , _goldRate :: SpawnRate
                             , _weaponRate :: SpawnRate
                             }
makeLenses ''SpawnRates


defaultSpawnRates :: SpawnRates
defaultSpawnRates = SpawnRates { _wumpusRate = 0.15
                               , _trapRate = 0.05
                               , _goldRate = 0.15
                               , _weaponRate = 0.15
                               }

data Player = Player { _playerPosition :: (Int,Int)
                     , _playerGold :: Int
                     , _playerWeapon :: Bool
                     }
makeLenses ''Player

data ScoreBoard = ScoreBoard { _purse :: Int
                             , _slayedMonsters :: Int
                             , _exploredEmptyRooms :: Int
                             }
makeLenses ''ScoreBoard

data GameState = GameState { _player :: Player
                           , _cave :: Cave
                           , _explored :: Set (Int,Int)
                           , _scores :: ScoreBoard
                           }
makeLenses ''GameState

initialScoreBoard :: ScoreBoard
initialScoreBoard = ScoreBoard 0 0 0

data Direction = North | South | West | East deriving (Show)

type Game a = StateT GameState IO a
