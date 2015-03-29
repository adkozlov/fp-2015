module Complex where
import Data.List.Split

-- (2 балл)

data Complex = Complex { real :: Double, im :: Double } deriving Eq

instance Num Complex where
    (+) (Complex re1 im1) (Complex re2 im2) = Complex (re1 + re2) (im1 + im2)
    (*) (Complex re1 im1) (Complex re2 im2) = Complex (re1 * re2 - im1 * im2) (re1 * im2 + im1 * re2)
    fromInteger n = Complex (fromIntegral n) 0
    negate (Complex re im) = Complex (-re) (-im)
    abs (Complex re im) = Complex (sqrt $ re ^ 2 + im ^ 2) 0
    
    signum _ = error "Complex: signum isn't defined"

instance Fractional Complex where
    (/) (Complex re1 im1) (Complex re2 im2) = Complex ((re1 * re2 + im1 * im2) / r) ((re2 * im1 - re1 * im2) / r) where r = re2 ** 2 + im2 ** 2
    fromRational r = Complex (fromRational r) 0

-- show и read должны работать как описано в тестах в Main.hs
instance Show Complex where
    show (Complex 0 0) = "0"
    show (Complex re 0) = show re
    show (Complex 0 im) = show im ++ showI
    show (Complex re im) = show re ++ showIm im ++ showI
   
showI :: String
showI = " * i"                                   

showIm :: Double -> String
showIm im | im > 0 = " + " ++ show im
          | im < 0 = " - " ++ show (abs im)

instance Read Complex where
    readsPrec _ s = [((fromList $ splitOn " " s), "")]

fromList :: [String] -> Complex
fromList [] = error "Complex: cannot construct complex number from empty list"
fromList ["i"] = i
fromList ["-i"] = negate i
fromList [re] = Complex (read re) 0
fromList [im, "*", "i"] = Complex 0 $ read im
fromList ["-", im, "*", "i"] = Complex 0 $ -(read im)
fromList [re, "+", im, "*", "i"] = Complex (read re) $ read im
fromList [re, "-", im, "*", "i"] = Complex (read re) $ -(read im)

i :: Complex
i = Complex 0 1
