
import qualified Codec.Picture as P
import Data.Maybe (isJust)


type Color = P.PixelRGB8
type DiscreteImage = P.Image Color

data NestingParams = NestingParams
  { center :: (Float, Float)
  , period :: Float
  }

lookupPixel :: DiscreteImage -> Int -> Int -> Maybe Color
lookupPixel img x y =
  if x >= 0 && x < w && y >= 0 && y < h
  then Just (P.pixelAt img x y)
  else Nothing
  where
    w = P.imageWidth img
    h = P.imageHeight img

directQuery :: DiscreteImage -> Float -> Float -> Maybe Color
directQuery img x y =
--  (trace $ "directQuery " ++ show x ++ "," ++ show y) $
  lookupPixel img (round x) (round y)

nestedQuery :: DiscreteImage -> NestingParams -> Float -> Float -> Color
nestedQuery img (NestingParams (cx, cy) period) x y =
  inward (outward 0)
  where
    x' i = (period^^i) * x
    y' i = (period^^i) * y

    lookup i = directQuery img (x' i + cx) (y' i + cy)
    test = isJust . lookup

    outward i = if test i then outward (i + 1) else i
    inward i =
      case lookup i of
        Nothing -> inward (i - 1)
        Just x -> x

-- Social contract:
--   f (x + 1) y = f x (y + 1) = f x y
type TorusImage = Float -> Float -> Color

twist :: TorusImage -> TorusImage
twist f x y = f x (x + y)

load :: FilePath -> IO DiscreteImage
load imgPath = do
  eitherDynImg <- P.readImage imgPath
  case eitherDynImg of
    Left err -> error err
    Right dynImg -> return (P.convertRGB8 dynImg)

discretize :: (Float -> Float -> Color) -> Int -> DiscreteImage
discretize colorAt width =
  P.generateImage colorAt' width width
  where
    colorAt' x y =
      colorAt
        (fromIntegral (x - (width `div` 2)) + shiftToAvoidLimitPoint)
        (fromIntegral (y - (width `div` 2)))
    shiftToAvoidLimitPoint = 0.1

-- Fill in the nested parts of a picture in case they are missing.
fillInNesting :: FilePath -> NestingParams -> IO ()
fillInNesting imgPath params = do
  img <- load imgPath
  let filledIn = discretize (nestedQuery img params) outputWidth
  P.saveJpgImage jpgQuality "result.jpg" (P.ImageRGB8 filledIn)
  where
    outputWidth = 500
    jpgQuality = 90


main :: IO ()
-- main = fillInNesting "assets/escher.jpg" (NestingParams (850,850) 10)
main = fillInNesting "assets/droste.jpg" (NestingParams (130,1450) 10)
