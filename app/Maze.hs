{-# LANGUAGE TemplateHaskell, FlexibleContexts, BlockArguments #-}

module Maze where

import Control.Monad.State.Strict (State, when, execState, gets, modify)
import Control.Monad.ST.Lazy      (runST, strictToLazyST, ST)
import Data.Set                   (Set, toList, fromList, empty, member, insert)
import System.Random.MWC          (Seed,createSystemRandom, Gen, save, restore, uniformR, Variate)
import Data.List                  (sortBy)
import Data.Ord                   (comparing)
import Control.Monad              (liftM2)
import System.Environment         (getArgs)
import Control.Lens               (lens, Lens')
import Control.Lens.Operators     ((^.),(<%=),(%=),(%%=))

-- Data Structures:

type Action  = State States
type Vector  = (Int,Int)
type History = Set (Vector,Vector)

data States = States { stack      :: [Vector]
                     , history    :: History
                     , dims       :: Dimensions
                     , randomness :: [Int] } deriving Show

lstack = lens stack \s b -> s { stack = b }
lhistory = lens history \s b -> s { history = b }
lrandomness = lens randomness \s b -> s { randomness = b }

data Dimensions = Dimensions { width  :: Int
                             , height :: Int
                             , cx     :: Int
                             , cy     :: Int } deriving Show

-- Random Helpers

randomRs :: (Int,Int) -> Seed -> [Int]
randomRs r s = runST $ do
  g <- strictToLazyST $ restore s
  advance r g

advance :: (Int,Int) -> Gen s -> ST s [Int]
advance r g = do
  x <- strictToLazyST $ uniformR r g
  xs <- x `seq` advance r g
  return (x:xs)

-- Main:

main :: IO ()
main = do args <- getArgs
          let (w:h:rest) | length args > 1 = map read args
                         | otherwise       = [10,5]

              randomGen  | null rest       = randomRs (0,3) `fmap` (save =<< createSystemRandom)
                         | otherwise       = return $ cycle rest

              pipeline   = putStr . draw dimensions . toList . history . execState step . States [(cx dimensions, cy dimensions)] empty dimensions
              dimensions = Dimensions w h (w `div` 2) (h `div` 2)

          if length args < 2 then putStrLn "Usage: maze <width/2> <height/2> <entropy>*"
                             else randomGen >>= pipeline

-- Algorithm:

step :: Action ()
step = top >>= visit >> getNeighbours >>= mapM_ proceed >> pop

proceed :: Vector -> Action ()
proceed cell = visited cell >>= flip when (removeWall cell >> push cell >> step) . not

-- Helpers:

push, visit, removeWall :: Vector -> Action ()
push       cell = lstack %= (cell :)
visit      cell = lhistory %= insert (cell, (0,0))
removeWall cell = do exit   <- top
                     lhistory %= insert (exit, cell `minus` exit)
pop :: Action ()
pop  = lstack %= tail

top :: Action Vector
top = gets $ head . stack

visited :: Vector -> Action Bool
visited cell  = member (cell, (0,0)) `fmap` gets history

getNeighbours :: Action [Vector]
getNeighbours = liftM2 (,) popRands (gets dims) >>= flip fmap top . uncurry adjacent

popRands :: Action [Int]
popRands = do rands <- gets (take 4 . randomness)
              lrandomness %= drop 4
              return rands

minus, plus :: Vector -> Vector -> Vector
minus (ox,oy) (lx,ly) = (ox-lx,oy-ly)
plus  (ox,oy) (lx,ly) = (ox+lx,oy+ly)

adjacent :: Ord a => [a] -> Dimensions -> (Int, Int) -> [(Int, Int)]
adjacent rands ds (x,y)  = map snd $ sortBy (comparing fst) $ zip rands $ filter (not . onBounds (width ds) (height ds)) [(x,y+1),(x+1,y),(x,y-1),(x-1,y)]

onBounds :: Int -> Int -> (Int, Int) -> Bool
onBounds w h (x,y) = x == 0 || y == 0 || x == w || y == h

-- Drawing Functions:

draw :: Dimensions -> [((Int, Int), Vector)] -> String
draw ds = format ds . fill ds . fromList . entryExit ds . map (uncurry plus . space)

-- Creates the entry and exit holes in the sides of the maze
entryExit :: Dimensions -> [Vector] -> [Vector]
entryExit ds = ([(1, cy ds * 2), (width ds * 2 - 1, cy ds * 2)] ++)

format :: Dimensions -> String -> String
format ds s = "P1\n" ++ show (width ds * 2 - 1) ++ " " ++ show (height ds * 2 - 1) ++ "\n" ++ s

fill :: Dimensions -> Set Vector -> String
fill ds ls = unlines $ map (\y -> map (wall ls y) [1 .. width ds * 2 - 1]) [1 .. height ds * 2 - 1]

wall :: (Ord t, Ord t1) => Set (t, t1) -> t1 -> t -> Char
wall ls y x = if member (x,y) ls then '0' else '1'

space :: (Num t1, Num t2) => ((t1, t2), t) -> ((t1, t2), t)
space ((x,y),d) = ((x*2,y*2),d)
