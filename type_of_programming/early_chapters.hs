data Season = Winter | Spring | Summer | Fall
  deriving (Show)

data Temp = Cold | Warm | Hot | Chilly
  deriving (Show)

rhyme :: Either Temp Season -> String
rhyme = \x -> 
  case x of 
    Left Cold -> "Bold"
    Left Chilly -> "Willy"
    Left Hot -> "Bot"
    Left Warm -> "Carm"
    Right Winter -> "Sinter"
    Right Spring -> "Bring"
    Right Summer -> "Bummer"
    Right Fall -> "Ball"

seasonTemp :: Season -> Temp
seasonTemp = \x ->
  case x of
    Winter -> Cold
    Spring -> Chilly
    Summer -> Hot
    Fall -> Warm

tempSeason :: Temp -> Season
tempSeason = \x ->
  case x of
    Cold -> Winter
    Warm -> Fall
    Hot -> Summer
    Chilly -> Spring

strTemp :: String -> Maybe Temp
strTemp = \x ->
  case x of
    "cold" -> Just Cold
    "warm" -> Just Warm
    "hot" -> Just Hot
    "chilly" -> Just Chilly
    _ -> Nothing

oppositeSeason :: Season -> Season
oppositeSeason = \x ->
  case x of
    Winter -> Summer
    Spring -> Fall
    Summer -> Winter
    Fall -> Spring

main :: IO ()
main = do
  let favSeason = Winter
  print $rhyme (Right Winter)
