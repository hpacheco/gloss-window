{-# LANGUAGE CPP, ScopedTypeVariables, ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}

module Graphics.Gloss.Window.Dimension where

import Graphics.Gloss.Window.Picture
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