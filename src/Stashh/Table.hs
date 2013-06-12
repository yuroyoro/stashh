{-# LANGUAGE OverloadedStrings #-}

module Stashh.Table where

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.List (transpose, intercalate, intersperse)
import Data.Text.ICU.Char

-- a type for records
data A = A { make  :: String
           , model :: String
           , years :: [Int] }
    deriving Show

-- a type for fill functions
type Filler = Int -> String -> String

-- a type for describing table columns
data ColDesc t = ColDesc { colTitleFill :: Filler
                         , colTitle     :: String
                         , colValueFill :: Filler
                         , colValue     :: t -> String
                         }

-- functions that fill a string (s) to a given width (n) by adding pad
-- character (c) to align left, right, or center
fillLeft c n s = s ++ replicate (n - eawLength s) c
fillRight c n s = replicate (n - eawLength s) c ++ s
fillCenter c n s = replicate l c ++ s ++ replicate r c
    where x = n - eawLength s
          l = x `div` 2
          r = x - l

-- functions that fill with spaces
left = fillLeft ' '
right = fillRight ' '
center = fillCenter ' '

-- calculate string width with consideration about EastAsianWidth
eawLength :: String -> Int
eawLength = sum . map waWidth

waWidth :: Char -> Int
waWidth c = case property EastAsianWidth c of
  EANeutral   -> 1
  EAAmbiguous -> 2
  EAHalf      -> 1
  EAFull      -> 2
  EANarrow    -> 1
  EAWide      -> 2
  EACount     -> 2


-- converts a list of items into a table according to a list
-- of column descriptors
showTable :: [ColDesc t] -> [t] -> String
showTable cs ts =
    let header = map colTitle cs
        rows = [[colValue c t | c <- cs] | t <- ts]
        widths = [maximum $ map eawLength col | col <- transpose $ header : rows]
        separator = intercalate "-+-" [replicate width '-' | width <- widths]
        fillCols fill cols = intercalate " | " [fill c width col | (c, width, col) <- zip3 cs widths cols]
    in
        unlines $ fillCols colTitleFill header : separator : map (fillCols colValueFill) rows

class TableDef t where
  columnsDef :: [ColDesc t]

renderTable :: (TableDef t) => [t] -> String
renderTable ts = showTable columnsDef ts

class PagingDef t where
  paging_start  :: t -> Int
  paging_size   :: t -> Int
  paging_limit  :: t -> Int

pagingInfo :: (PagingDef t) => t -> String
pagingInfo t = concat $ intersperse " / " $ map printPair ps
  where
    printPair p = (fst p) <> ":" <> (snd p)
    ps =
      [ ("start", show $ paging_start t)
      , ("size",  show $ paging_size t)
      , ("limit", show $ paging_limit t)
      ]

showMaybe :: (Show a) => (t -> Maybe a) -> t -> String
showMaybe f t = fromMaybe "" $ show <$> (f t)
