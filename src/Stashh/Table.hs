{-# LANGUAGE OverloadedStrings #-}

module Stashh.Table where

import Stashh.AnsiColor

import Data.Time.Clock.POSIX
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import System.IO.Unsafe

import Control.Applicative
import Data.Maybe
import Data.Monoid
import Data.List (transpose, intercalate, intersperse, stripPrefix)
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
                         , colColor     :: String -> String
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
eawLength = sum . (map eawWidth) . removeEscapeSequence

eawWidth :: Char -> Int
eawWidth c = case property EastAsianWidth c of
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
        fillCols fill color cols = intercalate " | " [color c $ fill c width col | (c, width, col) <- zip3 cs widths cols]
    in
        unlines $ fillCols colTitleFill colColor header : separator : map (fillCols colValueFill colColor) rows

class TableDef t where
  columnsDef :: [ColDesc t]

renderTable :: (TableDef t) => [t] -> String
renderTable ts = showTable columnsDef ts

class PagingDef t where
  paging_start  :: t -> Int
  paging_size   :: t -> Int
  paging_limit  :: t -> Int
  paging_last   :: t -> Bool

pagingInfo :: (PagingDef t) => t -> String
pagingInfo t = concat $ intersperse " / " $ map printPair ps
  where
    printPair p = (fst p) <> ":" <> (snd p)
    ps =
      [ ("start", show $ paging_start t)
      , ("size",  show $ paging_size t)
      , ("limit", show $ paging_limit t)
      , ("isLastPage", show $ paging_last t)
      ]

showWithMax :: Int -> (t -> String) -> t -> String
showWithMax max f t = foldl appends "" (f t)
  where
    appends s c = if (((eawLength s) + (eawWidth c)) > max) then s else s <> [c]

showMaybe :: (Show a) => (t -> Maybe a) -> t -> String
showMaybe f t = fromMaybe "" $ show <$> (f t)

showTime :: (t -> Int) -> t -> String
showTime f t = formatTime defaultTimeLocale  "%F %X" $ posixSecondsToUTCTime $ time
  where
    time = ((fromIntegral (f t)) / 1000) :: POSIXTime

showTimeAgo :: (t -> Int) -> t -> String
showTimeAgo f t = case diff of
  0 -> "just now"
  n | 1       <= n && n <= 59     -> (show n) <> "seconds ago"
  n | 60      <= n && n <= 119    -> "1 minute age"
  n |  120    <= n && n <= 3540   -> (show $ quot n 60) <> "minutes ago"
  n |  3541   <= n && n <= 7100   -> "1 hour ago"
  n |  7101   <= n && n <= 82800  -> (show $ quot (n + 99) 3600) <> "hours ago"
  n |  82801  <= n && n <= 172000 -> "1 day ago"
  n -> (show $ quot (n + 800) 86400) <> "days ago"

  where
    time    = ((fromIntegral (f t)) / 1000) :: POSIXTime
    current = unsafePerformIO getPOSIXTime
    diff    = truncate (current - time)


showRef :: (t -> String) -> t -> String
showRef f t = fromMaybe (f t) $ stripPrefix "refs/heads/" (f t)

showRefWithMax :: Int -> (t -> String) -> t -> String
showRefWithMax max f t = take max $ fromMaybe (f t) $ stripPrefix "refs/heads/" (f t)
