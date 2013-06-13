module Stashh.AnsiColor where

import Data.List
import Text.Regex

data Color  =  Black
            |  Red
            |  Green
            |  Yellow
            |  Blue
            |  Magenta
            |  Cyan
            |  White
            |  Default
  deriving Enum

esc :: [String] -> String
esc []  =  ""
esc xs  =  "\ESC[" ++ (concat . intersperse ";" $ xs) ++ "m"

col :: Color -> Bool -> Color -> [String]
col fg bf bg = show (fromEnum fg + 30) : bf' [show (fromEnum bg + 40)]
  where bf' | bf         =  ("01" :)
            | otherwise  =  id


inColor :: Color -> Bool -> Color -> String -> String
inColor c bf bg txt = esc (col c bf bg) ++ txt ++ esc ["00"]

black, red, green, yellow, blue, magenta, cyan, white :: String -> String
black   = inColor Black   False Default
red     = inColor Red     False Default
green   = inColor Green   False Default
yellow  = inColor Yellow  False Default
blue    = inColor Blue    False Default
magenta = inColor Magenta False Default
cyan    = inColor Cyan    False Default
white   = inColor White   False Default

bold, italic, underline, inverse :: String -> String
bold      = ansi "1" "22"
italic    = ansi "3" "23"
underline = ansi "4" "24"
inverse   = ansi "7" "27"

ansi :: String -> String -> String -> String
ansi on off txt = esc [on] ++ txt ++ esc [off]

removeEscapeSequence :: String -> String
removeEscapeSequence s = subRegex (mkRegex "\ESC\\[[0-9]+(;[0-9]+)?m") s ""
