{-# LANGUAGE
  OverloadedStrings
  #-}
module Main where

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Haste.App    
import Haste.DOM
import qualified Haste.DOM.JSString as DOMJS
import Haste.Events
import Haste.Graphics.Canvas
import Haste.JSON

sideSize :: Double
sideSize = 16

defaultColor :: Color
defaultColor = RGB 0xbb 0xbb 0xbb
           
filledColor :: Color
filledColor = RGB 0xbb 0xbb 0x11

main :: IO ()
main = do  
  withElems ["board","input","load","units"] $ \[board, input, loadButton, unitsDiv] -> do
    do
      let width, height :: Int
          width = 5
          height = 10
      setBoardSize board width height
      Just canvas <- getCanvas board
      renderBoard canvas width height Map.empty Nothing

    onEvent loadButton Click $ \_ -> do
      s <- DOMJS.getProp input "value"
      let Right json = decodeJSON s
          decodeCell cell = (round x, round y)
            where
              Num x = cell ! "x"
              Num y = cell ! "y"
      let Num width'  = json ! "width"
          Num height' = json ! "height"
          width  = round width'
          height = round height'
          filled = Map.fromList [(decodeCell cell, filledColor) | let Arr filled = json ! "filled", cell <- filled]
          
          units = [(map decodeCell members, decodeCell pivot) | let Arr units' = json ! "units", unit' <- units', let Arr members = unit' ! "members", let pivot = unit' ! "pivot"]

      setBoardSize board width height
      Just canvas <- getCanvas board
      renderBoard canvas width height filled Nothing

      clearChildren unitsDiv             
      forM_ units $ \unit@(members, pivot) -> do
        let min_x = minimum [x | (x,y) <- pivot:members]
            max_x = maximum [x | (x,y) <- pivot:members]
            min_y = minimum [y | (x,y) <- pivot:members]
            max_y = maximum [y | (x,y) <- pivot:members]
            offset_x = (min_x `div` 2) * 2
            offset_y = (min_y `div` 2) * 2
            f (x,y) = (x - min_x, y - min_y)
            members' = map f members
            pivot' = f pivot
            width  = maximum [x | (x,y) <- pivot':members'] + 1
            height = maximum [y | (x,y) <- pivot':members'] + 1
        c <- newElem "canvas"
        appendChild unitsDiv c
        setBoardSize c width height
        Just canvas <- getCanvas c
        renderBoard canvas width height (Map.fromList [((x,y), filledColor) | (x,y) <- members']) (Just pivot')

    return ()

setBoardSize :: MonadIO m => Elem -> Int -> Int -> m ()
setBoardSize board width height = do
  setAttr board "width" $ show (sideSize * sqrt 3 * (fromIntegral width + 0.5) + 5) ++ "px"
  setAttr board "height" $ show (sideSize * 1.5 * fromIntegral height + sideSize + 5) ++ "px"
           
renderBoard :: MonadIO m => Canvas -> Int -> Int -> Map (Int,Int) Color -> Maybe (Int,Int) -> m ()
renderBoard canvas width height collors pivot = do
  render canvas $ do
    setStrokeColor $ RGB 0 0 0
    forM_ [0 .. height - 1] $ \i -> do
      let y0 = 2 + sideSize * 1.5 * fromIntegral i
      forM_ [0 .. width - 1] $ \j -> do
        let x0 = 2 + sideSize * sqrt 3 * (if even i then fromIntegral j else fromIntegral j + 0.5)
            hex :: Shape ()
            hex = path
              [ (x0, y0 + sideSize / 2)
              , (x0 + sideSize * sqrt 3 / 2, y0)
              , (x0 + sideSize * sqrt 3, y0 + sideSize / 2)
              , (x0 + sideSize * sqrt 3, y0 + sideSize * 1.5)
              , (x0 + sideSize * sqrt 3 / 2, y0 + sideSize * 2)
              , (x0, y0 + sideSize * 1.5)
              ]
        setFillColor $
          case Map.lookup (j,i) collors of
            Nothing -> defaultColor
            Just c -> c
        fill hex
        stroke hex

        when (Just (j,i) == pivot) $ do
          setFillColor $ RGB 0xff 0 0
          let radius = sideSize / 3
          fill $ circle (x0 + sideSize * sqrt 3 / 2, y0 + sideSize) radius
