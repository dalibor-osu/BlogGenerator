import Data.Word (Word8) -- Word8 is an 8-bit unsigned integer type
import Data.Maybe (listToMaybe)

-- | A data type representing colors
data Color
  = RGB Word8 Word8 Word8

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

isBright :: AnsiColor -> Bool
isBright ansiColor =
  case ansiColor of
    AnsiColor Bright _ -> True
    _ -> False

ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor =
  case ansiColor of
    AnsiColor Dark Black ->
      RGB 1 1 1
    AnsiColor Dark Red ->
      RGB 222 56 43
    AnsiColor Dark Green ->
      RGB 57 181 74
    AnsiColor Dark Yellow ->
      RGB 255 199 6
    AnsiColor Dark Blue ->
      RGB 0 111 184
    AnsiColor Dark Magenta ->
      RGB 118 38 113
    AnsiColor Dark Cyan ->
      RGB 44 181 233
    AnsiColor Dark White ->
      RGB 204 204 204
    AnsiColor Bright Black ->
      RGB 128 128 128
    AnsiColor Bright Red ->
      RGB 255 0 0
    AnsiColor Bright Green ->
      RGB 0 255 0
    AnsiColor Bright Yellow ->
      RGB 255 255 0
    AnsiColor Bright Blue ->
      RGB 0 0 255
    AnsiColor Bright Magenta ->
      RGB 255 0 255
    AnsiColor Bright Cyan ->
      RGB 0 255 255
    AnsiColor Bright White ->
      RGB 255 255 255

isEmpty :: [a] -> Bool
isEmpty list =
  case list of
    [] -> True
    _ -> False

