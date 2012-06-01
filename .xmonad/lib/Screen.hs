{-# LANGUAGE DeriveDataTypeable #-}

module Screen where

import XMonad
import XMonad.Actions.PhysicalScreens
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as S

import Control.Applicative ((<$>))
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List (lookup, sortBy)
import Data.Ord (comparing)

type Screens = [(ScreenId, Rectangle)]

getScreens :: MonadIO m => m Screens
getScreens = do
    dpy <- liftIO $ openDisplay ""
    rects <- getCleanedScreenInfo dpy
    let sortedList = sortBy (comparing (\(_, Rectangle x y _ _) -> (x, y))) (zip [0..] rects)
    return sortedList

data Edge = TopEdge | BottomEdge deriving (Read, Show, Eq)

data Alignment = LeftA | CenterA | RightA deriving (Read, Show, Eq)

data Width = WidthRatio Rational
           | WidthPixels Dimension

getBarPos :: Rectangle -> Edge -> Alignment -> Width -> Dimension -> Rectangle
getBarPos (Rectangle x y w h) edge align width height = Rectangle x' y' w' h'
  where
    w' = calcWidth width `min` w

    x' | align == LeftA   = x
       | align == CenterA = x + fromIntegral (w - w') `div` 2
       | otherwise        = x + fromIntegral (w - w')

    calcWidth (WidthPixels wp) = wp
    calcWidth (WidthRatio wr)  = wp
      where
        wp | align == RightA = floor (wr * toRational w)
           | otherwise       = ceiling (wr * toRational w)

    h' = height `min` h

    y' | edge == TopEdge = y
       | otherwise       = y + fromIntegral (h - h')

trayerCmdSpec :: Edge -> Alignment -> Width -> Dimension -> [String]
trayerCmdSpec edge align width height = concat [edgeCmd, alignCmd, widthCmd, heightCmd]
  where
    edgeCmd   = ["--edge", if edge == TopEdge then "top" else "bottom"]
    alignCmd  = ["--align", case align of
                                LeftA   -> "left"
                                CenterA -> "center"
                                RightA  -> "right"
                ]
    widthCmd  = case width of
        WidthRatio wr  -> ["--widthtype", "percent", "--width", let p = floor (wr * 100) :: Int in show p]
        WidthPixels wp -> ["--widthtype", "pixel", "--width", show wp]

    heightCmd = ["--height", show height]

rectToDzenCmdSpec :: Rectangle -> [String]
rectToDzenCmdSpec (Rectangle x y w h) =
    [ "-x", show x
    , "-y", show y
    , "-w", show w
    , "-h", show h
    ]
