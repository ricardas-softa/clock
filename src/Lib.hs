module Lib where

import Data.Time
import Data.List
import Data.Foldable
import Distribution.Simple.Utils

---- Constants ----

-- CLOCK DESIGN
clockDetails :: [Detail]
clockDetails = [Center, Border, HourHand, MinuteHand, SecondHand]

-- Brightness levels
brightness0 = ' '
brightness1 = '·'
brightness2 = '•'
brightness3 = '■'

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

drawCenter :: Config -> Time -> Layer
drawCenter _ _= Layer [Cell (0, 0) brightness1]

drawBorder :: Config -> Time -> Layer
drawBorder (Config size) _ = Layer $ drawArk size 1 ++ drawArk size (-1)

-- Helper to draw circle. Takes grid size and direction as 1 or -1
drawArk :: Int -> Int -> [Cell]
drawArk size direction = [ Cell (x, round(sqrt(r ** 2 - fromIntegral x ** 2 )) * direction) brightness2 |
                         x <- gridRange size ]
                         where r = (fromIntegral size - 1) / 2

drawHand :: Int -> Int -> Char -> Int -> Int -> Layer
drawHand size lengthPercentage symbol value maxValues = Layer [ Cell (x, round $ fromIntegral x * (yEnd / xEnd)) symbol |
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

instance Drawable Detail where
  draw config time Center = drawCenter config time
  draw config time Border = drawBorder config time
  draw config time HourHand = drawHourHand config time
  draw config time MinuteHand = drawMinuteHand config time
  draw config time SecondHand = drawSecondHand config time
  draw _ _ _ = Layer [] -- TODO: Implement other Details

-- instance Drawable Figure where
--   draw (Dot x y) = Layer [Cell (x, y) brightness3]
--   draw (Line (x1, y1) (x2, y2)) = Layer [Cell (x1, y1) brightness3, Cell (x2, y2) brightness3]
--   draw (Circle (x, y) r) = Layer [Cell (x, y) brightness3]

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
render (Config gridSize) l = unlines [ [ renderGridChar $ getCell l (x,y) |
                                                         x <- gridRange gridSize ] |
                                                         y <- reverse $ gridRange gridSize]

display :: Grid -> IO ()
display = putStrLn

-- printTime :: IO ()
-- printTime = do
-- --   time <- getCurrentTime
-- --   let timeString = formatTime defaultTimeLocale "%H:%M" time
-- --   putStrLn timeString
--     time <- getCurrentTime
--     print time
