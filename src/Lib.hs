module Lib where
import Data.Time
import Data.List
import Data.Foldable
import Data.Fixed
import Distribution.Simple.Utils
import Data.Char (intToDigit, toLower)
import System.Console.ANSI
import Control.Concurrent
import Control.Monad
import Data.Maybe
import System.Exit
import System.IO
import Text.Read (Lexeme(Char))

---- Constants ----

-- CLOCK DESIGN
clockDetails :: [Detail]
clockDetails = [Border, HourMarks, {--MinuteMarks,--} Digits, Brand, HourHand, MinuteHand, SecondHand, Center]

-- CLOCK BRAND
brand :: String
brand = "HClock"

clockDefaultSize :: Int
clockDefaultSize = 31

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

-- Animation

data Direction = R | D | L | U deriving (Eq, Show)

data AnimationState = AnimationState
  {
    direction :: Direction,
    position :: Int
  }

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
  draw :: Config -> AnimationState -> Time -> a -> Layer

buildInitState :: AnimationState
buildInitState = AnimationState R 0

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
drawHand size lengthPercentage _ value maxValues = Layer [ Cell (round(xEnd * fromIntegral pos / r), round (yEnd * fromIntegral pos / r)) (getDirectionalSymbol a) |
                                          pos <- [0 .. round r]]
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
drawSecondHand (Config size) time = drawHand size 85 brightness1 (second time) 60

getDirectionalSymbol :: Float -> Char
getDirectionalSymbol angle = take 12 (cycle ['‧', '‧', '‧', '‧', '‧', '‧']) !! round ( normalizeAngle (6 * angle / pi))
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

drawBrand :: Config -> AnimationState -> Layer
drawBrand (Config size) state = drawText (x,y) brand <> drawAnimation state
  where x = round $ fromIntegral (-length brand) / 2
        y = round $ fromIntegral size / 4

        drawAnimation :: AnimationState -> Layer
        drawAnimation (AnimationState R pos) = Layer [Cell (x + pos,y + 1) (determineAnimationChar R pos)]
        drawAnimation (AnimationState D pos) = Layer [Cell (x + length brand,y - pos) (determineAnimationChar D pos)]
        drawAnimation (AnimationState L pos) = Layer [Cell (x + length brand - 1 - pos,y - 1) (determineAnimationChar L pos)]
        drawAnimation (AnimationState U pos) = Layer [Cell (x - 1,y + pos) (determineAnimationChar U pos)]

        determineAnimationChar :: Direction -> Int -> Char
        determineAnimationChar R p | p == length brand = '⌍'
                                   | otherwise = '-'
        determineAnimationChar D p | p == 1 = '⌏'
                                   | otherwise = '│'
        determineAnimationChar L p | p == length brand = '⌎'
                                   | otherwise = '-'
        determineAnimationChar U p | p == 1 = '⌌'
                                   | otherwise = '│'

centerText :: Int -> String -> String
centerText size text = replicate ((size - length text) `div` 2) ' ' ++ text

instance Drawable Detail where
  -- draw :: Config -> AnimationState -> Time -> a -> Layer
  draw _ _ _ Center               = drawCenter
  draw config _ _ Border          = drawBorder config
  draw config _ _ HourMarks       = drawHourMarks config
  draw config _ _ MinuteMarks     = drawMinuteMarks config
  draw config _ time HourHand     = drawHourHand config time
  draw config _ time MinuteHand   = drawMinuteHand config time
  draw config _ time SecondHand   = drawSecondHand config time
  draw config _ _ Digits          = drawDigits config
  draw config state _ Brand           = drawBrand config state

---- Functions ----

makeBlankGrid :: Config -> String
makeBlankGrid (Config gridSize) = concat $ replicate gridSize (replicate gridSize brightness0 ++ "\n")

drawClock :: Config -> AnimationState -> Time -> Layer
drawClock c s t = foldl' (<>) (Layer []) $ map (draw c s t) clockDetails

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

transition :: AnimationState -> AnimationState
transition (AnimationState R pos) = if pos == length brand then AnimationState D 0
                                  else AnimationState R (pos + 1)
transition (AnimationState D pos) = if pos == 1 then AnimationState L 0
                                  else AnimationState D (pos + 1)
transition (AnimationState L pos) = if pos == length brand then AnimationState U 0
                                  else AnimationState L (pos + 1)
transition (AnimationState U pos) = if pos == 1 then AnimationState R 0
                                  else AnimationState U (pos + 1)


display :: Grid -> IO ()
display = putStrLn

clScreen :: IO ()
clScreen = putStr "\ESC[3J\ESC[1;1H"

-- runClock :: Config -> AnimationState -> IO ()
-- runClock config state = do
--   clScreen
--   now <- getCurrentTime
--   timeZone <- getCurrentTimeZone
--   let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timeZone now
--   display $ render config (drawClock config state (Time hour minute (floor second)))
--   threadDelay $ 1000 * 1000
--   runClock config (transition state)

runClock :: Config -> AnimationState -> IO ()
runClock config state = do
    kbInput <- newEmptyMVar
    hSetBuffering stdin NoBuffering
    forkIO $ do
      key <- getChar
      putMVar kbInput key
    wait kbInput state
        where wait kbInput state = do
                key <- tryTakeMVar kbInput
                if isJust key then void $ clScreen >> clearScreen >> showCursor >> putStrLn "\nGood Bye!"
                else do
                  clScreen
                  now <- getCurrentTime
                  timeZone <- getCurrentTimeZone
                  let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timeZone now
                  display $ render config (drawClock config state (Time hour minute (floor second)))
                  putStrLn $ centerText (gridSize config) "Press ANY key to exit"
                  threadDelay $ 10 * 1000
                  wait kbInput (transition state)