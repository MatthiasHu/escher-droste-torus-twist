
import qualified Codec.Picture as P
import Data.Maybe (isJust)


type Color = P.PixelRGB8
type Image = P.Image Color


data NestingParams = NestingParams
  { center :: (Float, Float)
  , period :: Float
  }

lookupPixel :: Image -> Int -> Int -> Maybe Color
lookupPixel img x y =
  if x >= 0 && x < w && y >= 0 && y < h
  then Just (P.pixelAt img x y)
  else Nothing
  where
    w = P.imageWidth img
    h = P.imageHeight img

directQuery :: Image -> Float -> Float -> Maybe Color
directQuery img x y = lookupPixel img (round x) (round y)

nestedQuery :: Image -> NestingParams -> Float -> Float -> Color
nestedQuery img (NestingParams (cx, cy) period) x y =
  undefined
--  inward (outward 0)
  where
    x' i = cx + ((period^^i) * (x - cx))
    y' i = cy + ((period^^i) * (y - cy))

    lookup i = directQuery img (x' i) (y' i)
    test = isJust . lookup

    outward i = if test i then outward (i + i) else i
    inward i =
      case lookup i of
        Nothing -> inward (i - 1)
        Just x -> x

-- Social contract:
--   f (x + 1) y = f x (y + 1) = f x y
type TorusImage = Float -> Float -> Color

twist :: TorusImage -> TorusImage
twist f x y = f x (x + y)

main :: IO ()
main = undefined
