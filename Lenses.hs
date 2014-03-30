{-# LANGUAGE RankNTypes #-}
module Lenses where

import Control.Lens (at, _Just, traverse, filtered, lens, view)
import Control.Lens.Operators
import Control.Lens.Type

import Types

atCoord :: (Int,Int) -> Traversal' Cave Room
atCoord (x,y) = rooms . at (x,y) . _Just

weaponRooms :: Traversal' Cave Room
weaponRooms = rooms . traverse . filtered (==Weapon)

playerCoord :: Lens' GameState (Int,Int)
playerCoord = player . playerPosition

playerHasWeapon :: Lens' GameState Bool
playerHasWeapon = player . playerWeapon

playerRoom :: Lens' GameState Room
playerRoom = lens getter setter
  where getter g = g ^?! cave . atCoord (view playerCoord g)
        setter g r = g & cave . atCoord (view playerCoord g) .~ r
