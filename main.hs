
import qualified Codec.Picture as P
import Data.Maybe (isJust)


type Color = P.PixelRGB8
type DiscreteImage = P.Image Color

newtype Cartesian a = Cartesian ((Float, Float) -> a)

-- If (Polar f), then:
--   f (r, phi + 1) = f (r, phi)
newtype Polar a = Polar ((Float, Float) -> a)

-- If (Toroidal (Polar f)), then:
--   f (r + 1, phi) = f (r, phi)
newtype Toroidal a = Toroidal (Polar a)

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

discretize :: Int -> Cartesian Color -> DiscreteImage
discretize width (Cartesian colorAt) =
  P.generateImage colorAt' width width
  where
    colorAt' x y =
      colorAt
        ( fromIntegral (x - (width `div` 2)) + shiftToAvoidLimitPoint
        , fromIntegral (y - (width `div` 2)) )
    shiftToAvoidLimitPoint = 0.1

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

-- We look up the desired color as far away from the limit point as possible
-- to retain the best possible resolution.
-- We assume that the input "partial image" is roughly convex.
nest :: Polar (Maybe a) -> Toroidal a
nest (Polar f) = Toroidal . Polar $ \(r, phi) ->
  let
    r' i = r + (fromIntegral i)

    lookup i = f (r' i, phi)
    test = isJust . lookup

    outward i = if test i then outward (i + 1) else i
    inward i =
      case lookup i of
        Nothing -> inward (i - 1)
        Just x -> x
  in
    inward (outward 0)

forgetToroidal :: Toroidal a -> Polar a
forgetToroidal (Toroidal polar) = polar

rotate :: Float -> Polar a -> Polar a
rotate dphi (Polar f) = Polar $ \(r, phi) ->
  f (r, phi - dphi)

zoom :: Float -> Polar a -> Polar a
zoom dr (Polar f) = Polar $ \(r, phi) ->
  f (r - dr, phi)

-- Remove (or add) artificial torque (rotation increasing towards limit point)
torque :: Float -> Polar a -> Polar a
torque dphi (Polar f) = Polar $ \(r, phi) ->
  f (r, phi + dphi * r)

-- We assume that n is an integer.
twist :: Float -> Toroidal a -> Toroidal a
twist n (Toroidal (Polar f)) = Toroidal . Polar $ \(r, phi) ->
  f (r + n*phi, phi)

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

(-.) = flip (.)

main :: IO ()
main =
  let
    s = NestingScale 15
    center = (900,900)
  in
    load "assets/escher.jpg" >>=
       undiscretize
    -. translate center
    -. toPolar s
    -. torque (-0.35)
    -. rotate 0.05
    -. nest
    -. twist 1
    -. forgetToroidal
    -. zoom 0.5
    -. fromPolar s
    -. discretize 1000
    -. save
