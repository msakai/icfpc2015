{-# LANGUAGE
  OverloadedStrings
  #-}
module Main where

import Control.Monad
import Data.Set (Set)
import qualified Data.Set as Set

-- import Haste
import Haste.App    
import Haste.DOM
import qualified Haste.DOM.JSString as DOMJS
import Haste.Events
import Haste.Graphics.Canvas
import Haste.JSON

sideSize :: Double
sideSize = 20

main :: IO ()
main = do  
  withElems ["board","input","load"] $ \[board, input, loadButton] -> do
    let setSize width height = do
          setAttr board "width" $ show (sideSize * sqrt 3 * (fromIntegral width + 0.5) + 5) ++ "px"
          setAttr board "height" $ show (sideSize * 1.5 * fromIntegral height + sideSize + 5) ++ "px"
    do
      let width, height :: Int
          width = 5
          height = 10
      setSize width height
      Just canvas <- getCanvas board
      renderBoard canvas width height Set.empty

    onEvent loadButton Click $ \_ -> do
      s <- DOMJS.getProp input "value"
      let Right json = decodeJSON s
      let Num width'  = json ! "width"
          Num height' = json ! "height"
          width  = round width'
          height = round height'
          filled = Set.fromList [(round x, round y) | let Arr filled = json ! "filled", cell <- filled, let Num x = cell ! "x", let Num y = cell ! "y"]
      setSize width height
      Just canvas <- getCanvas board
      renderBoard canvas width height filled

    return ()

renderBoard :: MonadIO m => Canvas -> Int -> Int -> Set (Int,Int) -> m ()
renderBoard canvas width height filled = do
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
          if (j,i) `Set.member` filled
          then RGB 0xbb 0xbb 0x11
          else RGB 0xbb 0xbb 0xbb
        fill hex
        stroke hex
