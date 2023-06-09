
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

type PlaneImage = Float -> Float -> Color

-- Social contract:
--   f (period * x) (period * y) = f x y
-- for some "period".
type NestedImage = Float -> Float -> Color

nestedQuery :: DiscreteImage -> NestingParams -> NestedImage
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
--   f (r + 1) phi = f r (phi + 1) = f r phi
type TorusImage = Float -> Float -> Color

twist :: TorusImage -> TorusImage
twist f x y = f (x + y) y

tau :: (Floating a) => a
tau = 2 * pi

toTorusPoint :: Float -> (Float, Float) -> (Float, Float)
toTorusPoint period (x, y) = (r, phi)
  where
    r = log (sqrt (x*x + y*y)) / log period
    phi = atan2 y x / tau

fromTorusPoint :: Float -> (Float, Float) -> (Float, Float)
fromTorusPoint period (r, phi) = (x, y)
  where
    x = (period**r * cos (phi * tau))
    y = (period**r * sin (phi * tau))

toTorus :: Float -> NestedImage -> TorusImage
toTorus period f r phi = f x y
  where
    (x, y) = fromTorusPoint period (r, phi)

fromTorus :: Float -> TorusImage -> NestedImage
fromTorus period f x y = f r phi
  where
    (r, phi) = toTorusPoint period (x, y)

load :: FilePath -> IO DiscreteImage
load imgPath = do
  eitherDynImg <- P.readImage imgPath
  case eitherDynImg of
    Left err -> error err
    Right dynImg -> return (P.convertRGB8 dynImg)

save :: DiscreteImage -> IO ()
save img = do
  P.saveJpgImage jpgQuality "result.jpg" (P.ImageRGB8 img)
  where
    jpgQuality = 90

discretize :: (Float -> Float -> Color) -> DiscreteImage
discretize colorAt =
  P.generateImage colorAt' width width
  where
    colorAt' x y =
      colorAt
        (fromIntegral (x - (width `div` 2)) + shiftToAvoidLimitPoint)
        (fromIntegral (y - (width `div` 2)))
    shiftToAvoidLimitPoint = 0.1
    -- output width (and height)
    width = 1000

-- Fill in the nested parts of a picture in case they are missing.
fillInNesting :: FilePath -> NestingParams -> IO ()
fillInNesting imgPath params = do
  img <- load imgPath
  let filledIn = discretize (nestedQuery img params)
  save filledIn

twistImage :: FilePath -> NestingParams -> IO ()
twistImage imgPath params@(NestingParams center period) = do
  img <- load imgPath
  let filledIn = nestedQuery img params
  let twisted = fromTorus period . twist . toTorus period $ filledIn
  save (discretize twisted)

main :: IO ()
-- main = fillInNesting "assets/escher.jpg" (NestingParams (850,850) 10)
main = twistImage "assets/droste.jpg" (NestingParams (140,1455) 10)
