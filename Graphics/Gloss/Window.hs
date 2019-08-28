{-# LANGUAGE CPP, ScopedTypeVariables, ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}

module Graphics.Gloss.Window where

import Graphics.Gloss.Picture.Size
import Graphics.Gloss (Picture(..),Point(..),Display(..),Path(..))
import qualified Graphics.Gloss as Gloss
import Graphics.Gloss.Interface.Environment

import Data.Semigroup
import Data.Monoid

type Dimension = (Int,Int)

displayDimension :: Display -> IO Dimension
#if defined(ghcjs_HOST_OS)
    
displayDimension (Display x y) = return (x,y)

#else
    
displayDimension (InWindow _ dim _) = return dim
displayDimension (FullScreen) = getScreenSize
    
#endif


infix .-.
(.-.) :: Dimension -> Dimension -> Dimension
(x1,y1) .-. (x2,y2) = (x1 - x2,y1 - y2)

-- a function that draws a picture for aa given window size
type Window = Dimension -> Picture

--instance Semigroup Window where
--	(<>) = mappend

instance Monoid Window where
    mempty = empty
    mappend x y = many [x,y]
    mconcat = many  

-- | draws a square inside the current window
square :: Window -> Window
square w (x,y) = w (xy,xy)
    where xy = min x y

vhsSquare :: [[Window]] -> Window
vhsSquare wws (wx,wy) = vhs wws (floor wx',floor wy')
    where
    wx' = side * realToFrac l
    wy' = side * realToFrac c
    (side::Float) = min (realToFrac wx / realToFrac l) (realToFrac wy / realToFrac c)
    (l,c) = matrixDimension wws
    matrixDimension [] = (0,0)
    matrixDimension (x:xs) = (length x,succ $ length xs)

empty :: Window
empty (x,y) = Blank
    
many :: [Window] -> Window
many ws screen = Pictures $ map (\w -> w screen) ws

alignL :: Window -> Window
alignL w screen@(sx,sy) = Translate (-realToFrac diffx/2) 0 pic
    where
    pic = w screen
    (cx,cy) = pictureSize pic
    diffx = max 0 (sx-cx)

fit :: Picture -> Window
fit pic@(pictureSize -> (cx,cy)) screen@(sx,sy) = Scale scalexy scalexy pic
    where
    scalex = realToFrac sx / realToFrac cx
    scaley = realToFrac sy / realToFrac cy
    scalexy = min scalex scaley

fitH :: Picture -> Window
fitH pic@(pictureSize -> (cx,cy)) screen@(sx,sy) = Scale scalex scalex pic
    where
    scalex = realToFrac sx / realToFrac cx
    
fitV :: Picture -> Window
fitV pic = fst . fitV' pic

fitV' :: Picture -> Dimension -> (Picture,Int)
fitV' pic@(pictureSize -> (cx,cy)) screen@(sx,sy) = (Scale scaley scaley pic,round $ realToFrac cx * scaley)
    where
    scaley = realToFrac sy / realToFrac cy
    
stretch :: Picture -> Window
stretch pic@(pictureSize -> (cx,cy)) screen@(sx,sy) = Scale scalex scaley pic
    where
    scalex = realToFrac sx / realToFrac cx
    scaley = realToFrac sy / realToFrac cy

hLFitV :: Picture -> Window -> Window
hLFitV p w2 s = hL (const hl) (const pl) w2 s
    where
    (pl,hl) = fitV' p s

-- left-biased horizontal composition
-- function determines the width of the left window
hL :: (Int -> Int) -> Window -> Window -> Window
hL mkLeft w1 w2 (sx,sy) = Pictures [flattenTranslate (-realToFrac sx2/2) 0 $ w1 (sx1,sy),flattenTranslate (realToFrac sx1/2) 0 $ w2 (sx2,sy)]
    where
    sx1 = mkLeft sx
    sx2 = sx - sx1

hR :: (Int -> Int) -> Window -> Window -> Window
hR mkRight w1 w2 (sx,sy) = Pictures [flattenTranslate (-realToFrac sx2/2) 0 $ w1 (sx1,sy),flattenTranslate (realToFrac sx1/2) 0 $ w2 (sx2,sy)]
    where
    sx1 = sx - sx2
    sx2 = mkRight sx

-- evenly splits the screen horizontally into a list
hs :: [Window] -> Window
hs ws dim@(dimx,dimy) = go ws dim
    where
    dimxn = floor $ (realToFrac dimx) / (realToFrac $ length ws)
    go [] = empty
    go (x:xs) = hL (const dimxn) x (go xs)


-- top-biased vertical composition
-- function determines the height of the top window
vT :: (Int -> Int) -> Window -> Window -> Window
vT mkTop w1 w2 dim@(sx,sy) = Pictures [flattenTranslate 0 (realToFrac sy2/2) $ w1 (sx,sy1),flattenTranslate 0 (-realToFrac sy1/2) $ w2 (sx,sy2)]
    where
    sy1 = mkTop sy
    sy2 = sy - sy1

-- bottom-biased vertical composition
-- function determines the height of the bottom window
vB :: (Int -> Int) -> Window -> Window -> Window
vB mkBot w1 w2 dim@(sx,sy) = Pictures [flattenTranslate 0 (realToFrac sy2/2) $ w1 (sx,sy1),flattenTranslate 0 (-realToFrac sy1/2) $ w2 (sx,sy2)]
    where
    sy2 = mkBot sy
    sy1 = sy - sy2
    
-- vertically merge two windows after drawing (does not guarantee vertical size fits within window)
--vMerge :: Window -> Window -> Window
--vMerge w1 w2 dim@(sx,sy) = Pictures [Translate 0 (realToFrac pic2y/2) pic1,Translate 0 (-realToFrac pic1y/2) pic2]
--    where
--    pic1 = w1 dim
--    pic2 = w2 dim
--    (pic1x,pic1y) = pictureSize pic1
--    (pic2x,pic2y) = pictureSize pic2

-- evenly splits the screen vertically into a list
vs :: [Window] -> Window
vs ws dim@(dimx,dimy) = go ws dim
    where
    dimyn = floor $ (realToFrac dimy) / (realToFrac $ length ws)
    go [] = empty
    go (x:xs) = vT (const dimyn) x (go xs)

-- evenly splits the screen for a rows-of-columns matrix
vhs :: [[Window]] -> Window
vhs = vs . map hs

matrix :: Int -> Int -> (Int -> Int -> Window) -> Window
matrix l c draw = vhs $ map (map (uncurry draw)) m
    where
    m = [[(i,j) | j <- [0..c-1] ] | i <- [0..l-1]  ]

scale :: (Dimension -> Point) -> Window -> Window
scale factor w screen = Scale fx fy (w screen)
    where
    (fx,fy) = factor screen

border :: (Dimension -> Int) -> Window -> Window
border space w screen = w screen'
    where
    screen' = screen .-. (space screen*2,space screen*2)
    
rectangleSolid :: Window
rectangleSolid (x,y) = Gloss.rectangleSolid (realToFrac x) (realToFrac y)

circle :: Window
circle (x,y) = Gloss.circle r
    where
    r = (realToFrac $ min x y) / 2

circleSolid :: Window
circleSolid (x,y) = Gloss.circleSolid r
    where
    r = (realToFrac $ min x y) / 2

rectangleWire :: Window
rectangleWire (x,y) = Gloss.rectangleWire (realToFrac x) (realToFrac y)

fixedV :: Int -> Window -> Window
fixedV y w (sx,sy) = w (sx,y)

rotate :: Float -> Window -> Window
rotate ang w screen = Gloss.rotate ang $ w screen

translate :: (Dimension -> Point) -> Window -> Window
translate factor w screen = Gloss.translate fx fy $ w screen
    where
    (fx,fy) = factor screen

text :: String -> Window
text = const . Gloss.Text
