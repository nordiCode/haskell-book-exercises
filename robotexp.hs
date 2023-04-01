{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Coordinates = Coordinates {x:: Integer, y :: Integer}
  deriving (Eq,Show)

data Robot = Robot {dir :: Bearing, coors :: Coordinates} 
  deriving (Eq,Show)


bearing :: Robot -> Bearing
bearing robot = robot.dir

coordinates :: Robot -> Coordinates
coordinates robot = robot.coors

mkRobot :: Bearing -> Coordinates -> Robot
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