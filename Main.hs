{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Raster.Array
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..))
import System.Environment
import Data.Array.Repa                  as R
import Data.List
import Prelude                          as P
import System.Random
import qualified Data.Vector            as V
import Data.Array.Repa.Repr.Vector (fromVector)
import qualified System.Exit      as System

data Action = L | R | U | D
  deriving (Show, Eq, Bounded, Enum)

instance Random Action where
  random g = case randomR (fromEnum (minBound :: Action), fromEnum (maxBound :: Action)) g of
                (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                      (r, g') -> (toEnum r, g')

type State = Int
type Symbol = Int

type Table = V.Vector Point
type Tape = V.Vector Symbol

newtype Point = Point (State, Symbol, Action)
  deriving Show

data World = World {
    table     :: Table
  , tape      :: Tape
  , state     :: State
  , nStates   :: State
  , nSymbols  :: Symbol
  , pos       :: (Int, Int)
  , size      :: (Int, Int)
  , stepcount :: Int
}

genTable :: State -> Symbol -> IO Table
genTable nst nsy = do
  seed <- newStdGen
  let points = V.unfoldr f seed
  return $ V.take (nst * nsy) points
  where 
    f :: StdGen -> Maybe (Point, StdGen)
    f g = let (rndSt, g')     = (randomR (0, pred nst) g  :: (State, StdGen))
              (rndSy, g'')    = (randomR (0, pred nsy) g' :: (Symbol, StdGen))
              (rndAc, newGen) = (random g'' :: (Action, StdGen))
              rndPoint        = Point (rndSt, rndSy, rndAc)
          in return (rndPoint, newGen)

genTape :: Int -> Int -> Symbol -> IO Tape
genTape h w _ = return $ V.replicate (w * h) 0

genWorld :: Int -> Int -> State -> Symbol -> IO World
genWorld w h nst nsy = do
  tbl <- genTable nst nsy
  tp <- genTape w h nsy
  return $ World tbl tp (0 :: State) nst nsy (w `div` 2, h `div` 2) (w, h) 100

tickWorld :: World -> World
tickWorld world@(World wTable wTape st numStates _ (headX, headY) (nCols, nRows) _) =
  let tapePos = ((nRows * headX) + headY)
      sym = wTape `V.unsafeIndex` tapePos
      idx = (numStates * sym) + st
      Point (st', sym', ac) = wTable V.! idx
      (headX', headY') = case ac of
                          L -> if headX <= 0
                                then (pred nCols, headY)
                                else (pred headX, headY)
                          R -> if headX >= pred nCols
                                then (0, headY)
                                else (succ headX, headY)
                          U -> if headY <= 0
                                then (headX, pred nRows)
                                else (headX, pred headY)
                          D -> if headY >= pred nRows
                                then (headX, 0)
                                else (headX, succ headY)
    in world {
        pos   = (headX', headY')
      , state = st'
      , tape  = wTape `V.unsafeUpd` [(tapePos, sym')]
    }

symToCol :: Symbol -> Color
symToCol sym =
  case sym of 
    1 -> white
    2 -> red
    3 -> green
    4 -> azure
    5 -> cyan
    6 -> magenta
    7 -> blue
    _ -> black

displayWorld :: World -> IO (Array D DIM2 Color)
displayWorld (World _ tbl _ _ _ _ (nCols, nRows) _) = do
  let v' = V.map symToCol tbl
  return $ delay $ fromVector (Z:. nRows :. nCols) v'


-- handle space -- reset to start
handleEvent :: Event -> World -> IO World
handleEvent (EventKey (SpecialKey KeySpace) _ _ _) world@(World _ _ _ _ nsy _ (nCols, nRows)_ ) = 
   do tp  <- genTape nCols nRows nsy
      return world{
        tape  = tp
        , pos = (nCols `div` 2, nRows `div` 2)
        , state = 0
      }

-- handle enter - totally new
handleEvent (EventKey (SpecialKey KeyEnter) _ _ _) world@(World _ _ _ nst nsy _ (nCols, nRows)_ ) = 
   do tbl <- genTable nst nsy
      tp  <- genTape nCols nRows nsy
      return world{
        table = tbl
        , tape  = tp
        , pos = (nCols `div` 2, nRows `div` 2)
        , state = 0
      }

-- handle up - speed up
handleEvent (EventKey (SpecialKey KeyUp) _ _ _) world@(World _ _ _ _ _ _ _ s) = 
  return world{ stepcount = s + 5 }

-- handle down - slow down
handleEvent (EventKey (SpecialKey KeyDown) _ _ _) world@(World _ _ _ _ _ _ _ s) = 
  return world{ stepcount = if s > 5 then s - 5 else s }

-- handle escape - quit
handleEvent (EventKey (SpecialKey KeyEsc) _ _ _) _= System.exitWith System.ExitSuccess

handleEvent _ w = return w

fpow :: Int -> (b -> b) -> b -> b
fpow n f = foldr (.) id $ replicate n f

handleStep :: Float -> World -> IO World
handleStep _ w = return $ fpow (stepcount w) tickWorld w

run :: Int -> Int -> Int -> Int -> State -> Symbol -> IO ()
run windowX windowY scaleX scaleY !numStates !numSymbols
 = do
    let !sizeX = windowX `div` scaleX
    let !sizeY = windowY `div` scaleY
    initialWorld <- genWorld sizeX sizeY numStates numSymbols
    playArrayIO (InWindow "Turing Drawings" (windowX, windowY) (10, 10))
                (scaleX, scaleY)
                20
                initialWorld
                displayWorld
                handleEvent
                handleStep

main :: IO ()
main
 = do args <- getArgs
      case args of
        [] -> run 800 600 4 4 6 6
        [sizeX, sizeY, scaleX, scaleY, nSt, nSym]
              -> run (read sizeX) (read sizeY) (read scaleX) (read scaleY) (read nSt) (read nSym)
        _ -> putStr $ unlines
          [ "todo: use example"]
