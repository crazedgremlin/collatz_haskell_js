module Main where
import Haste
import Haste.App
import Haste.Graphics.Canvas

-- Compute the stopping time for the input n, where n is an integer and n > 0.
collatzCount :: Int -> Int
collatzCount n = length $ takeWhile (>1) $ iterate collatz n
  where
    collatz :: Int -> Int
    collatz m
      | mod m 2 == 0    =   div m 2
      | otherwise       =   3*m + 1

-- For the given x value, create the Point that should be plotted.
collatzPoint :: Int -> Int -> Point
collatzPoint x yMax = (fromIntegral x, yMax' - yScale*stoppingTime)
  where
    n :: Int
    n = round ((fromIntegral x) * xScale)

    yMax' :: Double
    yMax' = fromIntegral yMax

    stoppingTime :: Double
    stoppingTime = fromIntegral $ collatzCount n

-- Draws a point to the canvas without clearing.
drawPoint :: MonadIO m => Canvas -> Point -> m ()
drawPoint c (x,y) = renderOnTop c $ pixel
  where
    pixel :: Picture ()
    pixel = color (RGBA 0xFF 0x00 0x00 0.3) $ fill $ circle (x,y) 1.0

drawTickX :: MonadIO m => Canvas -> Int -> m ()
drawTickX c x = renderOnTop c $ text (fromIntegral x, 10) $ show x



xScale, yScale :: Double
xScale = 1/15
yScale = 3

-- Plot the stopping times of the values along the x axis.
main :: IO ()
main = do
  Just ce <- elemById "canvas"
  Just c <- getCanvas ce

  --setStyle ce "background-color" "black"

  widthStr <- getAttr ce "width"
  heightStr <- getAttr ce "height"
  let Just width = fromString widthStr
  let Just height = fromString heightStr
  writeLog widthStr
  writeLog heightStr

  let nLast :: Int
      nLast = round (width/xScale) - 1
  mapM (\x -> drawPoint c (collatzPoint x height)) [0..fromIntegral nLast]
  mapM (\x -> drawTickX c x) [0,100..nLast] 

  return ()
