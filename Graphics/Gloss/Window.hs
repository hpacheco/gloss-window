{-# LANGUAGE CPP, ScopedTypeVariables, GeneralizedNewtypeDeriving, ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}

module Graphics.Gloss.Window where

import Graphics.Gloss.Window.Picture
import Graphics.Gloss.Window.Dimension
import Graphics.Gloss (Picture(..),Point(..),Display(..),Path(..))
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.Environment

import Data.Semigroup
import Data.Monoid

import Control.Monad.Reader (Reader(..),ReaderT(..))
import qualified Control.Monad.Reader as Reader
import Control.Monad.Writer (Writer(..),WriterT(..))
import qualified Control.Monad.Writer as Writer
import Control.Monad

import Generics.Pointless.Combinators

-- * Monad

type SimpleWindow = Window ()

newtype Window a = Window { unWindow :: ReaderT Dimension (Writer Picture) a }
    deriving (Monad,Applicative,Functor)

mkWindow :: (Dimension -> Picture) -> Window ()
mkWindow f = Window $ do
    screen <- Reader.ask
    Writer.tell $ f screen

runWindow :: Dimension -> Window a -> (a,Picture)
runWindow screen (Window w) = Writer.runWriter (Reader.runReaderT w screen)

execWindow :: Dimension -> Window a -> Picture
execWindow screen w = snd $ runWindow screen w

askDimension :: Window Dimension
askDimension = Window $ Reader.ask

withDimension :: (Dimension -> Dimension) -> Window a -> Window a
withDimension f (Window w) = Window $ Reader.local f w

mapPicture :: (Picture -> Picture) -> Window a -> Window a
mapPicture f (Window w) = Window $ Reader.mapReaderT (Writer.mapWriter (id >< f)) w

-- * General combinators

none :: Window a -> Window ()
none w = w >> return ()

empty :: Window ()
empty = Window $ Writer.tell Blank
    
many :: [Window a] -> Window [a]
many = sequence

-- | draws a square window inside the current window
square :: Window a -> Window a
square w = withDimension f w
    where f (x,y) = let xy = min x y in (xy,xy)
    
-- top-biased vertical composition
-- function determines the height of the top window
vT :: Int -> Window a -> Window b -> Window (a,b)
vT sy1 w1 w2  = Window $ do
    dim@(sx,sy) <- Reader.ask
    let sy2 = sy - sy1
    let (a,p1) = runWindow (sx,sy1) w1
    let (b,p2) = runWindow (sx,sy2) w2
    Writer.tell $ Pictures [flattenTranslate 0 (realToFrac sy2/2) p1,flattenTranslate 0 (-realToFrac sy1/2) p2]
    return (a,b)

vT' :: (Dimension -> Int) -> Window a -> Window b -> Window (a,b)
vT' f w1 w2 = liftM f askDimension >>= \i -> vT i w1 w2

-- bottom-biased vertical composition
-- function determines the height of the bottom window
vB :: Int -> Window a -> Window b -> Window (a,b)
vB sy2 w1 w2  = Window $ do
    dim@(sx,sy) <- Reader.ask
    let sy1 = sy - sy2
    let (a,p1) = runWindow (sx,sy1) w1
    let (b,p2) = runWindow (sx,sy2) w2
    Writer.tell $ Pictures [flattenTranslate 0 (realToFrac sy2/2) p1,flattenTranslate 0 (-realToFrac sy1/2) p2]
    return (a,b)

vB' :: (Dimension -> Int) -> Window a -> Window b -> Window (a,b)
vB' f w1 w2 = liftM f askDimension >>= \i -> vB i w1 w2

-- evenly splits the screen vertically into a list
vs :: [Window a] -> Window [a]
vs ws = Window $ do
    dim@(dimx,dimy) <- Reader.ask
    let dimyn = floor $ (realToFrac dimy) / (realToFrac $ length ws)
    let go [] = fmap (Prelude.const []) empty
        go (x:xs) = fmap (uncurry (:)) (vT dimyn x (go xs))
    unWindow $ go ws

-- left-biased horizontal composition
-- function determines the width of the left window
hL :: Int -> Window a -> Window b -> Window (a,b)
hL sx1 w1 w2 = Window $ do
    (sx,sy) <- Reader.ask
    let sx2 = sx - sx1
    let (a,p1) = runWindow (sx1,sy) w1
    let (b,p2) = runWindow (sx2,sy) w2
    Writer.tell $ Pictures [flattenTranslate (-realToFrac sx2/2) 0 p1,flattenTranslate (realToFrac sx1/2) 0 p2]
    return (a,b)

hL' :: (Dimension -> Int) -> Window a -> Window b -> Window (a,b)
hL' f w1 w2 = liftM f askDimension >>= \i -> hL i w1 w2

hR :: Int -> Window a -> Window b -> Window (a,b)
hR sx2 w1 w2 = Window $ do
    (sx,sy) <- Reader.ask
    let sx1 = sx - sx2
    let (a,p1) = runWindow (sx1,sy) w1
    let (b,p2) = runWindow (sx2,sy) w2
    Writer.tell $ Pictures [flattenTranslate (-realToFrac sx2/2) 0 p1,flattenTranslate (realToFrac sx1/2) 0 p2]
    return (a,b)
    
hR' :: (Dimension -> Int) -> Window a -> Window b -> Window (a,b)
hR' f w1 w2 = liftM f askDimension >>= \i -> hR i w1 w2
    
-- evenly splits the screen horizontally into a list
hs :: [Window a] -> Window [a]
hs ws = Window $ do
    dim@(dimx,dimy) <- Reader.ask
    let dimxn = floor $ (realToFrac dimx) / (realToFrac $ length ws)
    let go [] = fmap (Prelude.const []) empty
        go (x:xs) = fmap (uncurry (:)) (hL dimxn x (go xs))
    unWindow $ go ws
  
-- evenly splits the screen for a rows-of-columns matrix
vhs :: [[Window a]] -> Window [[a]]
vhs = vs . map hs
    
-- splits the current window into a square matrix
vhsSquare :: [[Window a]] -> Window [[a]]
vhsSquare wws = withDimension mkSquare $ vhs wws 
    where
    mkSquare (wx,wy) = (floor wx',floor wy')
        where
        wx' = side * realToFrac l
        wy' = side * realToFrac c
        (side::Float) = min (realToFrac wx / realToFrac l) (realToFrac wy / realToFrac c)
        (l,c) = matrixDimension wws
        matrixDimension [] = (0,0)
        matrixDimension (x:xs) = (length x,succ $ length xs)
    
-- aligns a smaller window to the left of a larger window
alignL :: Window a -> Window a
alignL w = Window $ do
    screen@(sx,sy) <- Reader.ask
    let (a,pic) = runWindow screen w
    let (cx,cy) = pictureSize pic
    let diffx = max 0 (sx-cx)
    Writer.tell $ Translate (-realToFrac diffx/2) 0 pic
    return a

matrix :: Int -> Int -> (Int -> Int -> Window a) -> Window [[a]]
matrix l c draw = vhs $ map (map (uncurry draw)) m
    where
    m = [[(i,j) | j <- [0..c-1] ] | i <- [0..l-1]  ]
    
scale :: Point -> Window a -> Window a
scale (fx,fy) w  = Window $ do
    screen <- Reader.ask
    let (a,pic) = runWindow screen w
    Writer.tell $ Scale fx fy pic
    return a

scale' :: (Dimension -> Point) -> Window a -> Window a
scale' f w1 = liftM f askDimension >>= \i -> scale i w1

translate :: Point -> Window a -> Window a
translate (fx,fy) w  = Window $ do
    screen <- Reader.ask
    let (a,pic) = runWindow screen w
    Writer.tell $ Translate fx fy pic
    return a
    
translate' :: (Dimension -> Point) -> Window a -> Window a
translate' f w1 = liftM f askDimension >>= \i -> translate i w1
    
border :: Int -> Window a -> Window a
border space w = withDimension mkScreen w
    where
    mkScreen screen = (max 0 >< max 0) $ screen .-. (space*2,space*2)
    
border' :: (Dimension -> Int) -> Window a -> Window a
border' f w1 = liftM f askDimension >>= \i -> border i w1
    
rotate :: Float -> Window a -> Window a
rotate ang w = Window $ do
    screen <- Reader.ask
    let (a,pic) = runWindow screen w
    Writer.tell $ Gloss.rotate ang pic
    return a
    
rotate' :: (Dimension -> Float) -> Window a -> Window a
rotate' f w1 = liftM f askDimension >>= \i -> rotate i w1
    
rectangleSolid :: Window ()
rectangleSolid = Window $ do
    (x,y) <- Reader.ask
    Writer.tell $ Gloss.rectangleSolid (realToFrac x) (realToFrac y)
    
rectangleWire :: Window ()
rectangleWire = Window $ do
    (x,y) <- Reader.ask
    Writer.tell $ Gloss.rectangleWire (realToFrac x) (realToFrac y)
    
circle :: Window ()
circle = Window $ do
    (x,y) <- Reader.ask
    let r = (realToFrac $ min x y) / 2
    Writer.tell $ Gloss.circle r

circleSolid :: Window ()
circleSolid = Window $ do
    (x,y) <- Reader.ask
    let r = (realToFrac $ min x y) / 2
    Writer.tell $ Gloss.circleSolid r
    
-- * Special combinators

-- fit picture to window, returns picture's dimension
fit :: Picture -> Window Dimension
fit pic = fitWith (pictureSize pic) pic

fitWith :: Dimension -> Picture -> Window Dimension
fitWith (cx,cy) pic = Window $ do
    screen@(sx,sy) <- Reader.ask
    let scalex = realToFrac sx / realToFrac cx
        scaley = realToFrac sy / realToFrac cy
        scalexy = max 0 (min scalex scaley)
    Writer.tell $ Scale scalexy scalexy pic
    return (round $ realToFrac cx * scalexy,round $ realToFrac cy * scalexy)

-- horizontally fits a picture to the window, returns picture's height
fitH :: Picture -> Window Int
fitH pic@(pictureSize -> (cx,cy)) = Window $ do
    screen@(sx,sy) <- Reader.ask
    let scalex = max 0 (realToFrac sx / realToFrac cx)
    Writer.tell $ Scale scalex scalex pic
    return (round $ realToFrac cy * scalex)

-- vertically fits a picture to the window, returns picture's width
fitV :: Picture -> Window Int
fitV pic@(pictureSize -> (cx,cy)) = Window $ do
    screen@(sx,sy) <- Reader.ask
    let scaley = max 0 (realToFrac sy / realToFrac cy)
    Writer.tell (Scale scaley scaley pic)
    return (round $ realToFrac cx * scaley)

-- stretches a picture to fir the window
stretch :: Picture -> Window ()
stretch pic@(pictureSize -> (cx,cy)) = Window $ do
    screen@(sx,sy) <- Reader.ask
    let scalex = max 0 (realToFrac sx / realToFrac cx)
        scaley = max 0 (realToFrac sy / realToFrac cy)
    Writer.tell $ Scale scalex scaley pic
    return ()

charHeight :: Int
charHeight =  100
charWidth :: Int
charWidth = 70

text :: String -> Window Dimension
text str = fitWith dim pic
    where
    textWidth = realToFrac (length str) * (realToFrac charWidth)
    pic = Translate (-realToFrac textWidth/2) (-realToFrac charHeight/2) $ Gloss.Text str
    dim = (round textWidth,charHeight)

hLFitV :: Picture -> Window a -> Window (Int,a)
hLFitV p w2 = Window $ do
    screen <- Reader.ask
    let (px,pic1) = runWindow screen $ fitV p
    let w1 = Window $ do
            Writer.tell pic1
            return px
    unWindow $ hL px w1 w2

-- fit vertically with maximum horizontal size
hLFitV' :: Int -> Picture -> Window a -> Window (Dimension,a)
hLFitV' maxpx p w2 = Window $ do
    screen@(sx,sy) <- Reader.ask
    let (screen1@(px1,py1),pic1) = runWindow (maxpx,sy) $ fit p
    let w1 = Window $ do
            Writer.tell pic1
            return screen1
    unWindow $ hL px1 w1 w2

const :: Picture -> Window ()
const p = Window $ Writer.tell p

--fixedV :: Int -> Window a -> Window a
--fixedV y w = withDimension mkDim w
--    where
--    mkDim (sx,sy) = (sx,y)

