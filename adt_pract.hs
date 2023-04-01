{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

-- Type constructor = Data constructor Types
-- Type constructor = Name of the type, Only one type constructor
-- Data constructor = Used to construct instant of a type, Multiple 
--   data constructors is allowed
data Point a = Point a a
  deriving (Show,Eq)

data PointD = PointD
  {
    x :: Double
  , y :: Double
  }deriving (Show,Eq)

sumCoorPoint :: (Num a) => Point a -> a
sumCoorPoint (Point x y) = x + y 

-- moveUp :: (Num a) => Point a -> Point a
moveUp :: PointD -> PointD
moveUp point = point {y = prev + 1}
  where 
    prev = point.y


data Person = Person { name :: String }
data Company = Company { name :: String, owner :: Person }

main :: IO ()
main = do
  let c = Company { name = "Acme Corp."
                  , owner = Person { name = "Wile E. Coyote" } }
  let p = PointD { x = 0, y = 0}
  print $ "Initial p " ++ show p
  -- moveUp p 
  print $ "Finish p " ++ show p