
import qualified Codec.Picture as P
import Data.Maybe (isJust)


type Color = P.PixelRGB8
type DiscreteImage = P.Image Color

newtype Cartesian a = Cartesian ((Float, Float) -> a)
type CartesianImage = Cartesian Color

newtype Polar a = Polar ((Float, Float) -> a)
type PolarImage = Polar Color

lookupPixel :: DiscreteImage -> Int -> Int -> Maybe Color
lookupPixel img x y =
  if x >= 0 && x < w && y >= 0 && y < h
  then Just (P.pixelAt img x y)
  else Nothing
  where
    w = P.imageWidth img
    h = P.imageHeight img

undiscretize :: DiscreteImage -> Cartesian (Maybe Color)
undiscretize img = Cartesian $ \(x, y) ->
  lookupPixel img (round x) (round y)

newtype NestingScale = NestingScale Float

tau :: (Floating a) => a
tau = 2 * pi

toPolar :: NestingScale -> Cartesian a -> Polar a
toPolar (NestingScale s) (Cartesian f) = Polar $ \(r, phi) ->
  let
    x = (s**r * cos (phi * tau))
    y = (s**r * sin (phi * tau))
  in
    f (x, y)

fromPolar :: NestingScale -> Polar a -> Cartesian a
fromPolar (NestingScale s) (Polar f) = Cartesian $ \(x, y) ->
  let
    r = log (sqrt (x*x + y*y)) / log s
    phi = atan2 y x / tau
  in
    f (r, phi)

translate :: (Float, Float) -> Cartesian a -> Cartesian a
translate (cx, cy) (Cartesian f) = Cartesian $ \(x, y) ->
  f (cx + x, cy + y)

{-
-- We look up the desired color as far away from the limit point as possible
-- to retain the best possible resolution.
-- We assume that the input "partial image" is roughly convex.
nest :: Polar (Maybe a) -> Polar a
nest (Polar f) = Polar $ \(r, phi) ->
  inward (outward 0)
  where
    r' i = x + (fromIntegral i)

    lookup i = f (r' i, phi)
    test = isJust . lookup

    outward i = if test i then outward (i + 1) else i
    inward i =
      case lookup i of
        Nothing -> inward (i - 1)
        Just x -> x

nestedQuery :: NestingParams -> DiscreteImage -> NestedImage
nestedQuery params = nest params . centeredQuery params

-- Remove (or add) artificial rotation/torque.
torque :: Float -> Float -> (Float -> Float -> a) -> (Float -> Float -> a)
torque period dphi f x y = f x' y'
  where
    (r, phi) = toPolar period (x, y)
    phi' = phi + dphi * log r / log period
    (x', y') = fromPolar period (r, phi')

-- Social contract:
--   f (r + 1) phi = f r (phi + 1) = f r phi
type TorusImage = Float -> Float -> Color

twist :: Float -> TorusImage -> TorusImage
twist n f x y = f (x + n*y) y

toTorus :: Float -> NestedImage -> TorusImage
toTorus period f r phi = f x y
  where
    (x, y) = fromPolar period (r, phi)

fromTorus :: Float -> TorusImage -> NestedImage
fromTorus period f x y = f r phi
  where
    (r, phi) = toPolar period (x, y)

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
    width = 500

-- Fill in the nested parts of a picture in case they are missing.
fillInNesting :: FilePath -> NestingParams -> IO ()
fillInNesting imgPath params = do
  img <- load imgPath
  let filledIn = discretize (nestedQuery params img)
  save filledIn

twistImage :: FilePath -> NestingParams -> IO ()
twistImage imgPath params@(NestingParams center period) = do
  img <- load imgPath
  let filledIn = nestedQuery params img
  let twisted = fromTorus period . twist (-1) . toTorus period $ filledIn
  save (discretize twisted)

main :: IO ()
-- main = fillInNesting "assets/escher.jpg" (NestingParams (850,850) 10)
main = do
  let period = 14
  let params = (NestingParams (900,860) period)
  img <- load "assets/escher.jpg"
  save
    . discretize
    . fromTorus period
    . twist 1
    . toTorus period
    . nest params
    . torque period (-0.35*tau)
    . centeredQuery params
    $ img
-- main = twistImage "assets/droste.jpg" (NestingParams (140,1455) 10)
-- main = twistImage "assets/costarica.jpg" (NestingParams (1578,1666) 140)
-}
