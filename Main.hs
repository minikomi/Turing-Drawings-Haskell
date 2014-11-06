{-# LANGUAGE BangPatterns #-}
import Graphics.Gloss.Raster.Array
import Graphics.Gloss.Interface.Pure.Game (Event(..), Key(..), SpecialKey(..))
import System.Environment
import Data.Array.Repa                  as R
import Data.List
import Prelude                          as P
import System.Random
import qualified Data.Vector            as V
import qualified Data.Vector.Mutable    as M
import Data.Array.Repa.Repr.Vector (fromVector)
import qualified System.Exit      as System
import Control.Applicative (pure)

-- Point -----------------------------------------------------------------------

type State = Int
type Symbol = Int
data Action = L | R | U | D
  deriving (Show, Eq, Bounded, Enum)

instance Random Action where
  random g = case randomR (fromEnum (minBound :: Action), fromEnum (maxBound :: Action)) g of
                (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                      (r, g') -> (toEnum r, g')

newtype Point = Point (State, Symbol, Action)
  deriving Show

-- Table -----------------------------------------------------------------------

type Table = V.Vector Point

showTable :: Table -> String
showTable = V.foldr fmt ""
    where fmt (Point (st, sy, ac)) s = s Data.List.++ show st Data.List.++ show sy Data.List.++ show ac

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

-- Tape ------------------------------------------------------------------------

type Tape = V.Vector Symbol

updateTape :: Int -> Symbol -> Tape -> IO Tape
updateTape idx s t = {-# SCC update_tape #-} do 
  tMut <- V.thaw t
  M.write tMut idx s
  t' <- V.freeze tMut
  return $ t'

genTape :: Int -> Int -> Symbol -> IO Tape
genTape h w _ = return $ V.replicate (w * h) 0

-- World -----------------------------------------------------------------------

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


genWorld :: Int -> Int -> State -> Symbol -> IO World
genWorld w h nst nsy = do
  tbl <- genTable nst nsy
  tp <- genTape w h nsy
  return $ World tbl tp (0 :: State) nst nsy (w `div` 2, h `div` 2) (w, h) 3

tickWorld :: IO World -> IO World
tickWorld w = do
  world@(World wTable wTape st numStates _ (headX, headY) (nCols, nRows) _) <- w
  let tapePos = ((nRows * headX) + headY)
      sym     = wTape `V.unsafeIndex` tapePos
      idx     = (numStates * sym) + st
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
  newTape <- updateTape tapePos sym' wTape
  return world {
      pos   = (headX', headY')
    , state = st'
    , tape  = newTape
  }

symToCol :: Symbol -> Color
symToCol sym = {-# SCC color_lookup #-}
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
displayWorld (World _ tbl _ _ _ _ (nCols, nRows) _) = 
  return $ {-# SCC map_col #-}     R.map symToCol
         $ {-# SCC from_vector #-} fromVector (Z:. nRows :. nCols) tbl

-- Event -----------------------------------------------------------------------

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
      print $ showTable tbl
      return world{
        table = tbl
        , tape  = tp
        , pos = (nCols `div` 2, nRows `div` 2)
        , state = 0
      }

-- handle up - speed up
handleEvent (EventKey (SpecialKey KeyUp) _ _ _) world@(World _ _ _ _ _ _ _ s) = 
  return world{ stepcount = s + 1 }

-- handle down - slow down
handleEvent (EventKey (SpecialKey KeyDown) _ _ _) world@(World _ _ _ _ _ _ _ s) = 
  return world{ stepcount = if s > 1 then s - 1 else 1 }

-- handle escape - quit
handleEvent (EventKey (SpecialKey KeyEsc) _ _ _) _= System.exitWith System.ExitSuccess

-- handle other - ignore
handleEvent _ w = return w

fpow :: Int -> (b -> b) -> b -> b
fpow n f = foldr (.) f $ replicate (pred n) f

handleStep :: Float -> World -> IO World
handleStep _ w = {-# SCC handle_step #-} fpow (stepcount w) tickWorld $ pure w

run :: Int -> Int -> Int -> Int -> State -> Symbol -> IO ()
run windowX windowY scaleX scaleY numStates numSymbols
 = do
    let !sizeX = windowX `div` scaleX
    let !sizeY = windowY `div` scaleY
    initialWorld <- genWorld sizeX sizeY numStates numSymbols
    print $ showTable (table initialWorld)
    playArrayIO (InWindow "Turing Drawings" (windowX, windowY) (10, 10))
                (scaleX, scaleY)
                100
                initialWorld
                displayWorld
                handleEvent
                handleStep

main :: IO ()
main
 = do args <- getArgs
      case args of
        [] -> run 600 600 4 4 3 6
        [sizeX, sizeY, scaleX, scaleY, nSt, nSym]
              -> run (read sizeX) (read sizeY) (read scaleX) (read scaleY) (read nSt) (read nSym)
        _ -> putStr $ unlines
          [ "todo: use example"]
