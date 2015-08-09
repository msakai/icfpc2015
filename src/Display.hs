module Display where

import Data.Char
import Data.List
import Data.Ix
import qualified Data.Set as Set
import Text.PrettyPrint.Boxes
import Types
import Board
import Cell
import Unit

dispCell :: Bool -> Cell -> Box
dispCell False c = symEmpty
dispCell True  c = symFull

symEmpty :: Box
symEmpty = text [chr 0x00b7]

symFull :: Box
symFull = text "*"

dispBoard :: Board -> [Unit] -> Box
dispBoard b us = vcat top $ zigzag $ map (hsep 1 left . map disp) ls
  where
    disp c = dispCell (Set.member c cs) c
    ls  = splitsBy w rng
    rng = range (cell (0,0), cell (w-1,h-1)) 
    h  = Board.rows b
    w  = Board.cols b
    cs = Set.unions $ fulls b : map members us

splitsBy :: Number -> [a] -> [[a]]
splitsBy k = unfoldr phi
  where
    phi [] = Nothing
    phi xs = Just (splitAt k xs)

zigzag,zagzig :: [Box] -> [Box]
zigzag []     = []
zigzag (x:xs) = zig x : zagzig xs
  where
    zig = id

zagzig [] = []
zagzig (x:xs) = zag x : zigzag xs
  where
    zag = moveRight 1
