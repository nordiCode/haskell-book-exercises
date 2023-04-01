{-# LANGUAGE DuplicateRecordFields #-}

module Main where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Enum, Show)


data Robot = Robot {dir :: Bearing, coors :: (Integer,Integer) } 
  deriving (Eq,Show)


bearing :: Robot -> Bearing
bearing robot = dir robot

coordinates :: Robot -> (Integer, Integer)
coordinates robot = coors robot

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot {dir = direction, coors = coordinates}

-- oneMove :: Robot -> Char -> Robot
-- oneMove r c 
--   | c == 'A' && dir r == North = coors 

--   where
--     prevy = r.coors.y

-- instructions are in the form "RAALAL" 
move :: Robot -> String -> Robot
move robot instructions = error "You need to implement this function."

main :: IO ()
main = do

  print $ "yo"