{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Graphics.Gloss ( Display(..), Color )
import Graphics.Gloss.Raster.Field ( animateField, rgb )
import Data.Fixed ( mod' )

import Maze qualified as M
import Data.Set qualified as S
import Control.Monad.State.Strict qualified as S

-- main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)

window :: Display
window = InWindow "Moving Maze" (2000, 1500) (0,0)

fullscreen :: Display
fullscreen = FullScreen

sin' :: Floating a => a -> a
sin' t = sin t / 2 + 0.5

sin2 :: Floating a => a -> a
sin2 t = (sin t / 2 + 0.5) ** 2

gray :: Float -> Color
gray v = rgb v v v

distortAround :: Fractional t => (t -> t) -> t -> t -> t -> t -> t
distortAround f o m s x = x + m * f (x/s-o)

lensAround :: Floating a => a -> a -> a
lensAround o x = x + 1 - exp (negate (x-o) **2) / 2

type A = Float -> (Float, Float) -> Color

type F = A -> A

main :: IO ()
main = animateField window (2,2) scene4

testMaze :: S.Set M.Vector
testMaze = S.fromList $ draw $ pipeline (cycle [1,1,2,1,1,4,3])
    where
    draw = M.entryExit dimensions . map (uncurry M.plus . M.space)
    pipeline = S.toList . M.history . S.execState M.step . M.States [(M.cx dimensions, M.cy dimensions)] S.empty dimensions
    dimensions = M.Dimensions w h (w `div` 2) (h `div` 2)
    w = 80
    h = 60

scene4 :: A
scene4 = fran2 (fran image)
    where
    image :: A
    image t (x,y)
        | S.member (q x + 80, q' y + 60) testMaze = rgb (sin2 x) (sin2 y) (0.4 * sin' t)
        | otherwise  = gray 0

    q :: Float -> Int
    q f = floor $ 100 * f

    q' :: Float -> Int
    q' f = floor $ 70 * f

    fran :: F
    fran a t (x,y) = a t (x+sin(x*10+t/5)/20,y+sin(y*10+t/5)/20)

    fran2 a t (x,y) = a t (x/1.2,y/1.2)

scene3 :: A
scene3 = fran2 $ fran image
    where
    image :: A
    image t (x,y) = rgb (sin2 x) (sin2 y) (0.4 * sin' t)

    fran :: F
    fran a t (x,y) = a t (x*100,y*100)

    fran2 :: F
    fran2 a t (x,y) = a t (x',y')
        where
            x' = lensAround (sin (t/20) / 2) x
            y' = lensAround (cos (t/20) / 2) y

scene2 :: A
scene2 = fran2 (fran image)
    where
    image :: A
    image _ (x,y) = gray $ (mod' x 1 + mod' y 1) / 2

    fran :: F
    fran a t (x,y) = a t (10*x,10*y)

    fran2 :: F
    fran2 a t (x,y) = a t (x',y')
        where
        x' = distortAround sin (t/100) 100 50 x
        y' = y

scene1 :: A
scene1 = fran image
    where
    image :: A
    image t (x,y) = rgb (sin' (t*0.5)) (sin' (x+t/9)) (sin' (y+t/20))

    fran :: F
    fran a t (x,y) = a t p'
        where
        p' = (x+ 100 * sin' (t/5+y), y+ 100 * sin' (t/9+x))