--
-- Starting code for CPSC 449 Assignment 1
--
-- Generate and output a Mondrian-style image as an SVG tag within an HTML 
-- document.
--
import System.IO
import Control.Monad (replicateM)
import System.Random (randomRIO, StdGen, randomR, mkStdGen)

--
-- The width and height of the image being generated.
--
width :: Int
width = 1024

height :: Int
height = 768

split1 :: Float
split1 = 0.33

split2 :: Float
split2 = 0.67

colorChoose :: Float -> [Int]
colorChoose randomValues 
    | randomValues < 0.0833 = [255, 0, 0]
    | randomValues < 0.1667 = [135, 206, 250]
    | randomValues < 0.25 = [255, 255, 0]
    | otherwise = [255, 255, 255]

splitThink :: Int -> Int -> Bool
splitThink x y
    | x > 120 && x > y = True
    | otherwise = False

--
-- Generate and return a list of 20000 random floating point numbers between 
-- 0 and 1.  (Increase the 20000 if you ever run out of random values).
-- 
randomList :: Int -> [Float]
randomList seed = take 20000 (rl_helper (mkStdGen seed))

rl_helper :: StdGen -> [Float]
rl_helper g = fst vg : rl_helper (snd vg)
  where vg = randomR (0.0, 1.0 :: Float) g

--
-- Compute an integer between low and high from a (presumably random) floating
-- point number between 0 and 1.
--
randomInt :: Int -> Int -> Float -> Int
randomInt low high x = round ((fromIntegral (high - low) * x) + fromIntegral low)

splitPoint :: Int -> Float -> Int
splitPoint x z = randomInt (round(fromIntegral(x) * (split1))) (round(fromIntegral(x) * (split2))) z

--
-- Generate the tag for a rectangle with random color.  Replace the 
-- implementation of this function so that it generates all of the tags
-- needed for a piece of random Mondrian art.
-- 
-- Parameters:
--   x, y: The upper left corner of the region
--   w, h: The width and height of the region
--   r:s:t:rs: A list of random floating point values between 0 and 1
--
-- Returns:
--   [Float]: The remaining, unused random values
--   String: The SVG tags that draw the image
--

mondrian :: Int -> Int -> Int -> Int -> Float -> Float -> Float -> String
mondrian x y w h r g b =
  "<rect x=" ++ (show (x)) ++
  " y=" ++ (show (y)) ++
  " width=" ++ (show w) ++
  " height=" ++ (show h) ++
  " fill=\"rgb(" ++ show (randomInt 0 255 r) ++ ","
                 ++ show (randomInt 0 255 g) ++ ","
                 ++ show (randomInt 0 255 b) ++ ")\" "
                 ++ "stroke=\"none\" />"


tsquare :: Int -> Int -> Int -> Int -> [Float] -> (String, [Float])
tsquare x y w h (r:g:b:rest)
  | w >= 16 && h >= 16 = -- draw a square in the middle of the region
                       (mondrian x y w h r g b, rest)
                       -- call myself recursively 4 times
--                       ul_tags ++
--                       ur_tags ++
--                       ll_tags ++
--                       lr_tags,
                       -- and also return whatever random values were unused
                       -- after the last recursive call
--                       lr_rest)
  | otherwise        = (mondrian x y w h r g b, rest)
  where 
    new_w = w `div` 2
    new_h = h `div` 2
    (ul_tags, ul_rest) = (tsquare x y new_w new_h rest)
    (ur_tags, ur_rest) = (tsquare (x + new_w) y new_w new_h ul_rest)
    (ll_tags, ll_rest) = (tsquare x (y + new_h) new_w new_h ur_rest)
    (lr_tags, lr_rest) = (tsquare (x + new_w) (y + new_h) new_w new_h ll_rest)

--
-- The main program which generates and outputs mondrian.html.
--
main :: IO ()
main = do
  --  Right now, the program will generate a different sequence of random
  --  numbers each time it is run.  If you want the same sequence each time
  --  use "let seed = 0" instead of "seed <- randomRIO (0, 100000 :: Int)"

  --let seed = 0
  seed <- randomRIO (0, 100000 :: Int)
  let randomValues = randomList seed

  let prefix = "<html><head></head><body>\n" ++
               "<svg width=\"" ++ (show width) ++ 
               "\" height=\"" ++ (show height) ++ "\">"
      image = fst (tsquare 0 0 width height randomValues)
      suffix = "</svg>\n</html>"

  writeFile "mondrian.html" (prefix ++ image ++ suffix)
