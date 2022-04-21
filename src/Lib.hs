module Lib where

import Data.Time
import Data.List
import Data.Foldable
import Data.Fixed
import Distribution.Simple.Utils
import Data.Char (intToDigit)
import System.Console.ANSI
import Control.Concurrent

---- Constants ----

-- CLOCK DESIGN
clockDetails :: [Detail]
clockDetails = [Border, HourMarks, {--MinuteMarks,--} Digits, Brand, HourHand, MinuteHand, SecondHand, Center]

-- CLOCK BRAND
brand :: String
brand = "HClock"

-- Brightness levels
brightness0 = ' '
brightness1 = '░'
brightness2 = '▒'
brightness3 = '▓'
brightness4 = '█'

-- brightness0 = ' '
-- brightness1 = '·'
-- brightness2 = '•'
-- brightness3 = '■'

---- Types ----

newtype Config = Config
  {
    -- Grid will always be square
    -- Would be better to set odd number for better centering
    gridSize :: Int
  }
  deriving Show

data Time = Time
  {
    hour :: Int,
    minute :: Int,
    second :: Int
  }
  deriving Show

type Coords = (Int, Int) -- (x, y)

type Grid = String

data Cell = Cell
  {
    coords :: Coords,
    symbol :: Char
  }
  deriving Show

-- Ignore symbol in cells when comparing
instance Eq Cell where
  (==) (Cell a _) (Cell b _) = a == b

-- Order by coords
instance Ord Cell where
  compare (Cell a _) (Cell b _) = compare a b

newtype Layer = Layer [Cell] deriving Show

instance Semigroup Layer where
  Layer a <> Layer [] = Layer a
  Layer [] <> Layer b = Layer b
  Layer a <> Layer b = Layer (map head . group . sort $ b ++ a)

-- data Figure = Dot { xCoord :: Int, yCoord :: Int }
--             | Line { pointA :: Coords, pointB :: Coords }
--             | Circle { center :: Coords, radius :: Int }

data Detail = Center | Border | HourMarks | MinuteMarks | HourHand | MinuteHand | SecondHand | Digits | Brand

class Drawable a where
  draw :: Config -> Time -> a -> Layer

drawCenter :: Layer
drawCenter = Layer [Cell (0, 0) '•']

drawBorder :: Config -> Layer
drawBorder (Config size) = Layer [Cell (round $ cos value * r, round $ sin value * r) brightness2 | value <- [0,0.01..2 * pi]]
  where r = (fromIntegral size - 1) / 2

drawHourMarks :: Config -> Layer
drawHourMarks (Config size) = drawMarks size brightness4 12

drawMinuteMarks :: Config -> Layer
drawMinuteMarks (Config size) = drawMarks size '*' 60

drawMarks :: Int -> Char -> Int -> Layer
drawMarks size symbol count = Layer [Cell (round $ cos value * r, round $ sin value * r) symbol | value <- map (\x -> fromIntegral x * angle) [1..count]]
  where r = (fromIntegral size - 1) / 2
        angle | count == 12 = pi / 6
              | otherwise = 2 * pi / fromIntegral count

drawHand :: Int -> Int -> Char -> Int -> Int -> Layer
drawHand size lengthPercentage _ value maxValues = Layer [ Cell (x, round $ fromIntegral x * (yEnd / xEnd)) (getDirectionalSymbol a) |
                                          x <- gridRange size,
                                          if xEnd >= 0 then
                                            x >= 0 && x <= round xEnd
                                            else x <= 0 && x >= round xEnd]
                                          where
                                          a = (2 * pi / fromIntegral maxValues) * fromIntegral value
                                          r = (((fromIntegral size - 1) / 2) * fromIntegral lengthPercentage) / 100
                                          xEnd = sin a * r
                                          yEnd = cos a * r

drawHourHand :: Config -> Time -> Layer
drawHourHand (Config size) time = drawHand size 50 brightness3 (hour time) 12

drawMinuteHand :: Config -> Time -> Layer
drawMinuteHand (Config size) time = drawHand size 80 brightness2 (minute time) 60

drawSecondHand :: Config -> Time -> Layer
drawSecondHand (Config size) time = drawHand size 90 brightness1 (second time) 60

getDirectionalSymbol :: Float -> Char
getDirectionalSymbol angle = take 12 (cycle ['|', '/', '╱', '-', '╲', '\\']) !! round ( normalizeAngle (6 * angle / pi))
  where
  normalizeAngle :: Float -> Float
  normalizeAngle angle | reducedAngle < 0 = 2 * pi + angle
                      | otherwise = reducedAngle
                where reducedAngle = angle `mod'` (2 * pi)

drawDigits :: Config -> Layer
drawDigits config = Layer [Cell (getCoordsByHour (gridSize config) h) (intToDigit h) | h <- [1..9]] <> drawDoubleDigits config

drawDoubleDigits :: Config -> Layer
drawDoubleDigits (Config size) = foldl' (<>) (Layer []) $ map (\h -> drawText (getCoordsByHour size h) (show h)) [10..12]

getCoordsByHour :: Int -> Int -> Coords
getCoordsByHour size hour = (round $ sin (fromIntegral hour * angle) * r, round $ cos (fromIntegral hour * angle) * r)
  where r = (fromIntegral size - 1) / 2 * 0.85
        angle = pi / 6

drawText :: Coords -> String -> Layer
drawText _ [] = Layer []
drawText (posX,posY) text = Layer [Cell (x,posY) c | x <- [posX..posX + length text - 1], c <- [text !! (x - posX)]]

drawBrand :: Config -> Layer
drawBrand (Config size) = drawText (round $ fromIntegral (-length brand) / 2,round $ fromIntegral size / 4) brand

instance Drawable Detail where
  draw _ _ Center               = drawCenter
  draw config _ Border          = drawBorder config
  draw config _ HourMarks       = drawHourMarks config
  draw config _ MinuteMarks     = drawMinuteMarks config
  draw config time HourHand     = drawHourHand config time
  draw config time MinuteHand   = drawMinuteHand config time
  draw config time SecondHand   = drawSecondHand config time
  draw config _ Digits          = drawDigits config
  draw config _ Brand           = drawBrand config

---- Functions ----

makeBlankGrid :: Config -> String
makeBlankGrid (Config gridSize) = concat $ replicate gridSize (replicate gridSize brightness0 ++ "\n")

drawClock :: Config -> Time -> Layer
drawClock c t = foldl' (<>) (Layer []) $ map (draw c t) clockDetails

getCell :: Layer -> Coords -> Maybe Cell
getCell (Layer cells) c = safeHead $ filter (\(Cell coords _) -> coords == c) cells

renderGridChar :: Maybe Cell -> Char
renderGridChar Nothing = brightness0
renderGridChar (Just (Cell _ symbol)) = symbol

gridRange :: Int -> [Int]
gridRange gridSize = [gridSize `div` (-2) + 1..gridSize `div` 2]

render :: Config -> Layer -> Grid
render c (Layer []) = makeBlankGrid c
-- render Grid leaving empty column to the left
render (Config gridSize) l = unlines [ ' ' : [ renderGridChar $ getCell l (x,y) |
                                                         x <- gridRange gridSize ] |
                                                         y <- reverse $ gridRange gridSize]

display :: Grid -> IO ()
display = putStrLn

printTime :: IO ()
printTime = print =<< getCurrentTime
--   time <- getCurrentTime
--   let timeString = formatTime defaultTimeLocale "%H:%M" time
--   putStrLn timeString

clScreen :: IO ()
clScreen = putStr "\ESC[3J\ESC[1;1H"

runClock :: Config -> IO ()
runClock config = do
  clScreen
  now <- getCurrentTime
  timeZone <- getCurrentTimeZone
  let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timeZone now
  display $ render config (drawClock config (Time hour minute (floor second)))
  threadDelay $ 1000 * 1000
  runClock config