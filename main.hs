
import Codec.Picture

-- Social contract:
--   f (x + 1) y = f x (y + 1) = f x y
type TorusImage = Float -> Float -> PixelRGB8

twist :: TorusImage -> TorusImage
twist f x y = f x (x + y)
